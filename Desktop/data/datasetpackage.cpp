﻿//
// Copyright (C) 2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "datasetpackage.h"
#include "log.h"
#include "utilities/qutils.h"
#include "sharedmemory.h"
#include <QThread>
#include "engine/enginesync.h"
#include "qquick/jasptheme.h"
#include "columnencoder.h"
#include "timers.h"
#include "utilities/appdirs.h"
#include "utils.h"
#include "gui/messageforwarder.h"
#include "datasetpackagesubnodemodel.h"


DataSetPackage * DataSetPackage::_singleton = nullptr;

DataSetPackage::DataSetPackage(QObject * parent) : QAbstractItemModel(parent)
{
	if(_singleton) throw std::runtime_error("DataSetPackage can be constructed only once!");
	_singleton = this;
	//True init is done in setEngineSync!

	connect(this, &DataSetPackage::isModifiedChanged,	this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::loadedChanged,		this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::currentFileChanged,	this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::folderChanged,		this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::currentFileChanged,	this, &DataSetPackage::nameChanged);

	_dataSubModel	= new SubNodeModel(parIdxType::dataRoot);
	_filterSubModel = new SubNodeModel(parIdxType::filterRoot);
	_labelsSubModel = new SubNodeModel(parIdxType::labelRoot);
}

void DataSetPackage::setEngineSync(EngineSync * engineSync)
{
	_engineSync = engineSync;

	//These signals should *ONLY* be called from a different thread than _engineSync!
	connect(this,	&DataSetPackage::enginesPrepareForDataSignal,	_engineSync,	&EngineSync::enginesPrepareForData,	Qt::BlockingQueuedConnection);
	connect(this,	&DataSetPackage::enginesReceiveNewDataSignal,	_engineSync,	&EngineSync::enginesReceiveNewData,	Qt::BlockingQueuedConnection);

	reset();
}

bool DataSetPackage::isThisTheSameThreadAsEngineSync()
{
	return	QThread::currentThread() == _engineSync->thread();
}

void DataSetPackage::enginesPrepareForData()
{
	if(isThisTheSameThreadAsEngineSync())	_engineSync->enginesPrepareForData();
	else									emit enginesPrepareForDataSignal();
}

void DataSetPackage::enginesReceiveNewData()
{
	if(isThisTheSameThreadAsEngineSync())	_engineSync->enginesReceiveNewData();
	else									emit enginesReceiveNewDataSignal();

	ColumnEncoder::setCurrentColumnNames(getColumnNames()); //Same place as in engine, should be fine right?
}

void DataSetPackage::reset()
{
	beginLoadingData();
	setDataSet(nullptr);
	_archiveVersion				= Version();
	_dataArchiveVersion			= Version();
	_analysesHTML				= std::string();
	_analysesData				= Json::arrayValue;
	_warningMessage				= std::string();
	_hasAnalysesWithoutData		= false;
	_analysesHTMLReady			= false;
	_isArchive					= false;
	_dataFilter					= DEFAULT_FILTER;
	_filterConstructorJSON		= DEFAULT_FILTER_JSON;
	_computedColumns.reset();
	_filterShouldRunInit		= false;

	setLoaded(false);
	setModified(false);
	setCurrentFile("");

	resetEmptyValues();
	endLoadingData();
}

void DataSetPackage::setDataSet(DataSet * dataSet)
{
	if(_dataSet == dataSet)
		return;

	if(_dataSet && !dataSet)
		freeDataSet();

	_dataSet = dataSet;
}

void DataSetPackage::createDataSet()
{
	setDataSet(SharedMemory::createDataSet()); //Why would we do this here but the free in the asyncloader?
}

void DataSetPackage::freeDataSet()
{
	if(_dataSet)
		emit freeDatasetSignal(_dataSet);
	_dataSet = nullptr;
}

void DataSetPackage::regenerateInternalPointers()
{
	//Instead of hardcoding the positions for the rootnodes like is done in ::index we could also construct a map here to get back to the internal pointer easily, but it is a bit overkill. So im leaving it
	//The following must all be done in the same order as the definition of parIdxType otherwise ::index(...) breaks:
	_internalPointers = {{ parIdxType::dataRoot,	0 },
						 { parIdxType::data,		0 },
						 { parIdxType::filterRoot,	0 },
						 { parIdxType::filter,		0 }}; // none of the before need a column indicated.

	for(int col=0; col<columnCount(); col++)
		_internalPointers.push_back({parIdxType::labelRoot, col}); //these do because they need to know which column is ref'd

	for(int col=0; col<columnCount(); col++)
		_internalPointers.push_back({parIdxType::label,		col}); //these do because they need to know which column is ref'd
}

QModelIndex DataSetPackage::index(int row, int column, const QModelIndex &parent) const
{
	const void * pointer = nullptr;

	if(!parent.isValid()) //this is a rootnode then
	{
		//rootnodes are as follows (arranged as columns on row 0):	dataRoot, filterRoot
		//and as columns on row1:									labelRoot-col0 ... labelRoot-colN
		parIdxType	newIndexType = row < 1 ? (column == 0 ? parIdxType::dataRoot : parIdxType::filterRoot) : parIdxType::labelRoot;
					pointer = static_cast<const void*>(_internalPointers.data() + int(newIndexType) + (newIndexType == parIdxType::labelRoot ? column: 0));
	}
	else
	{
		const intnlPntPair * parentInfo = getInternalPointerPairFromIndex(parent);

		switch(parentInfo->first)
		{
		case parIdxType::dataRoot:
			pointer = static_cast<const void*>(_internalPointers.data() + int(parIdxType::data));
			break;

		case parIdxType::filterRoot:
			pointer = static_cast<const void*>(_internalPointers.data() + int(parIdxType::filter));
			break;

		case parIdxType::labelRoot:

			pointer = static_cast<const void*>(_internalPointers.data() + int(parIdxType::labelRoot) + columnCount() + parentInfo->second); //We add columnCount to jump over labelRoot entries. And we use the column from the parent so that we can later find our parent again.
			break;

		default:
			pointer = nullptr;
			Log::log() << "Got a valid parent in DataSetPackage::index but it isn't one of the `*Root`s" << std::endl;
			break;
		}
			
	}

	return createIndex(row, column, pointer);
}

const DataSetPackage::intnlPntPair * DataSetPackage::getInternalPointerPairFromIndex(const QModelIndex & index) const
{
	const void * internalPointer = index.internalPointer();
	if(internalPointer >= _internalPointers.data() && internalPointer <= (_internalPointers.data() + _internalPointers.size())) // Is this pointer in our vector? 
	{
		return static_cast<const intnlPntPair*>(internalPointer);
	}
	
	Log::log() << "getInternalPointerPairFromIndex received an internalPointer that doesnt seem to be part of _internalPointers" << std::endl;
	
	return _internalPointers.data(); //aka the one with parIdxType::root
}


parIdxType DataSetPackage::parIdxTypeIs(const QModelIndex &index) const
{
	if(!index.isValid()) return parIdxType::dataRoot;

	return getInternalPointerPairFromIndex(index)->first;
}

QModelIndex DataSetPackage::parent(const QModelIndex & index) const
{
	if(!index.isValid())
		return QModelIndex();

	const	intnlPntPair *	pointerPair		= getInternalPointerPairFromIndex(index);
			parIdxType		myType			= pointerPair->first;
			int				myColumn		= pointerPair->second;
	
	return parentModelForType(myType, myColumn);
}

parIdxType DataSetPackage::parIdxTypeParentForChild(parIdxType type)
{
	switch(type)
	{
	case parIdxType::data:			return parIdxType::dataRoot;
	case parIdxType::filter:		return parIdxType::filterRoot;
	case parIdxType::label:			return parIdxType::labelRoot;
	default:						break;
	}

	Log::log() << "Parent parIdxType requested for " << parIdxTypeToString(type) << " but that isn't possible, so returning it" << std::endl;
	return type;
}

parIdxType DataSetPackage::parIdxTypeChildForParent(parIdxType type)
{
	switch(type)
	{
	case parIdxType::dataRoot:		return parIdxType::data;
	case parIdxType::filterRoot:	return parIdxType::filter;
	case parIdxType::labelRoot:		return parIdxType::label;
	default:						break;
	}

	Log::log() << "Child parIdxType requested for " << parIdxTypeToString(type) << " but that isn't possible, so returning it" << std::endl;
	return type;
}

QModelIndex DataSetPackage::parentModelForType(parIdxType type, int column) const
{
	if(column < 0)
		return QModelIndex();

	switch(type)
	{
	case parIdxType::dataRoot:		[[fallthrough]];
	case parIdxType::filterRoot:	[[fallthrough]];
	case parIdxType::labelRoot:		return QModelIndex();

	case parIdxType::data:			return rootModelIndexForType(parIdxType::dataRoot,		column);
	case parIdxType::filter:		return rootModelIndexForType(parIdxType::filterRoot,	column);
	case parIdxType::label:			return rootModelIndexForType(parIdxType::labelRoot,		column);
	}

	return QModelIndex(); //Gcc really doesn't get it.
}

QModelIndex DataSetPackage::rootModelIndexForType(parIdxType type, int column) const
{
	if(column < 0)
		return QModelIndex();

	switch(type)
	{
	case parIdxType::dataRoot:		return index(0,		0,			QModelIndex());
	case parIdxType::filterRoot:	return index(0,		1,			QModelIndex());
	case parIdxType::labelRoot:		return index(1,		column,		QModelIndex());
	default:						break;
	}

	return QModelIndex(); //Gcc really doesn't get it.
}



int DataSetPackage::rowCount(const QModelIndex & parent) const
{
	if(!_dataSet) return 0;

	switch(parIdxTypeIs(parent))
	{
	case parIdxType::labelRoot:
	{
		if(parent.column() >= int(_dataSet->columnCount()))
			return 0;

		Column & col = _dataSet->columns()[parent.column()];
		if(col.getColumnType() == columnType::scale)
			return 0;

		int labelSize = col.labels().size();
		return labelSize;
	}
	case parIdxType::filterRoot:	[[fallthrough]];
	case parIdxType::dataRoot:		return !_dataSet ? 0 : _dataSet->rowCount();
	default:						return 0;
	}

	return 0; // <- because gcc is stupid
}

int DataSetPackage::columnCount(const QModelIndex &parent) const
{
	switch(parIdxTypeIs(parent))
	{
	case parIdxType::filter:		[[fallthrough]];
	case parIdxType::filterRoot:	return 1;

	case parIdxType::label:			[[fallthrough]];
	case parIdxType::labelRoot:		return 1;				 //The parent index has a column index in it that tells you which actual column was selected!

	case parIdxType::data:			[[fallthrough]];
	case parIdxType::dataRoot:		return _dataSet == nullptr ? 0 : _dataSet->columnCount();
	}

	return 0; // <- because gcc is stupid
}

bool DataSetPackage::getRowFilter(int row) const
{
	return data(this->index(row, 0, parentModelForType(parIdxType::filter))).toBool();
}

QVariant DataSetPackage::getDataSetViewLines(bool up, bool left, bool down, bool right) const
{
	return			(left ?		1 : 0) +
					(right ?	2 : 0) +
					(up ?		4 : 0) +
					(down ?		8 : 0);
}

QVariant DataSetPackage::data(const QModelIndex &index, int role) const
{
	if(!index.isValid()) return QVariant();

	parIdxType myType = parIdxTypeIs(index);
	
	if(role == int(specialRoles::selected))
		return false; //DataSetPackage doesnt know anything about selected, only LabelModel does (now)

	switch(myType)
	{
	default:
		return QVariant();

	case parIdxType::filter:
		if(_dataSet == nullptr || index.row() < 0 || index.row() >= int(_dataSet->filterVector().size()))
			return true;
		return _dataSet->filterVector()[index.row()];

	case parIdxType::data:
		if(_dataSet == nullptr || index.column() >= int(_dataSet->columnCount()) || index.row() >= int(_dataSet->rowCount()))
			return QVariant(); // if there is no data then it doesn't matter what role we play

		switch(role)
		{
		case Qt::DisplayRole:
		case int(specialRoles::label):				return tq(_dataSet->column(index.column())[index.row()]);
		case int(specialRoles::value):				return tq(_dataSet->column(index.column()).getOriginalValue(index.row()));
		case int(specialRoles::filter):				return getRowFilter(index.row());
		case int(specialRoles::columnType):			return int(_dataSet->columns()[index.column()].getColumnType());
		case int(specialRoles::lines):
		{
			bool	iAmActive		= getRowFilter(index.row()),
					belowMeIsActive = index.row() < rowCount() - 1	&& data(this->index(index.row() + 1, index.column(), index.parent()), int(specialRoles::filter)).toBool();

			return getDataSetViewLines(
				iAmActive,
				iAmActive,
				iAmActive && !belowMeIsActive,
				iAmActive && index.column() == columnCount(index.parent()) - 1 //always draw left line and right line only if last col
			);
		}
		}

	case parIdxType::label:
	{
		int parRowCount = rowCount(index.parent());

		if(!_dataSet || index.row() >= parRowCount)
			return QVariant(); // if there is no data then it doesn't matter what role we play

		//We know which column we need through the intnlPntPair!
		Labels & labels = _dataSet->column(getInternalPointerPairFromIndex(index)->second).labels();

		switch(role)
		{
		case int(specialRoles::filter):				return labels[index.row()].filterAllows();
		case int(specialRoles::value):				return tq(labels.getValueFromRow(index.row()));
		case int(specialRoles::lines):				return getDataSetViewLines(index.row() == 0, index.column() == 0, true, true);
		case Qt::DisplayRole:
		case int(specialRoles::label):				return tq(labels.getLabelFromRow(index.row()));
		default:									return QVariant();
		}
	}
	}

	return QVariant(); // <- because gcc is stupid
}

size_t DataSetPackage::getMaximumColumnWidthInCharacters(int columnIndex) const
{
	return _dataSet ? _dataSet->getMaximumColumnWidthInCharacters(columnIndex) : 0;
}

QVariant DataSetPackage::headerData(int section, Qt::Orientation orientation, int role)	const
{
	if (!_dataSet || section < 0 || section >= (orientation == Qt::Horizontal ? columnCount() : rowCount()))
		return QVariant();

	switch(role)
	{
	case int(specialRoles::maxColString):
	{
		//calculate some kind of maximum string to give views an expectation of the width needed for a column
		QString dummyText = headerData(section, orientation, Qt::DisplayRole).toString() + "XXXXX" + (isColumnComputed(section) ? "XXXXX" : ""); //Bit of padding for filtersymbol and columnIcon
		int colWidth = getMaximumColumnWidthInCharacters(section);

		while(colWidth > dummyText.length())
			dummyText += "X";

		return dummyText;
	}
	case int(specialRoles::maxRowHeaderString):				return QString::number(_dataSet->maxRowCount()) + "XXX";
	case int(specialRoles::filter):							return getColumnHasFilter(section) || isColumnUsedInEasyFilter(section);
	case Qt::DisplayRole:									return orientation == Qt::Horizontal ? tq(_dataSet->column(section).name()) : QVariant(section);
	case Qt::TextAlignmentRole:								return QVariant(Qt::AlignCenter);
	case int(specialRoles::labelsHasFilter):				return getColumnHasFilter(section);
	case int(specialRoles::columnIsComputed):				return isColumnComputed(section);
	case int(specialRoles::computedColumnError):			return tq(getComputedColumnError(section));
	case int(specialRoles::computedColumnIsInvalidated):	return isColumnInvalidated(section);
	}

	return QVariant();
}

bool DataSetPackage::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if(!index.isValid() || !_dataSet) return false;

	parIdxType myType = parIdxTypeIs(index);

	switch(myType)
	{
	default:
		return false;

	case parIdxType::filter:
		if(index.row() < 0 || index.row() >= int(_dataSet->filterVector().size()) || value.typeId() != QMetaType::Bool)
			return false;

		if(_dataSet->filterVector()[index.row()] != value.toBool())
		{
			_dataSet->filterVector()[index.row()] = value.toBool();

			emit dataChanged(DataSetPackage::index(index.row(), 0, parentModelForType(parIdxType::filter)),		DataSetPackage::index(index.row(), columnCount(index.parent()), parentModelForType(parIdxType::filter)));	//Emit dataChanged for filter
			emit dataChanged(DataSetPackage::index(index.row(), 0, parentModelForType(parIdxType::data)),		DataSetPackage::index(index.row(), columnCount(),				parentModelForType(parIdxType::data)));		//Emit dataChanged for data
			return true;
		}
		else
			return false;

	case parIdxType::data:
		Log::log() << "setData for data is not supported!" << std::endl;
		return false;

	case parIdxType::label:
	{
		int parColCount = columnCount(index.parent()),
			parRowCount = rowCount(index.parent());

		if(!_dataSet || index.column() >= parColCount || index.row() >= parRowCount || index.column() < 0 || index.row() < 0)
			return false;

		//We know which column we need through the parent index!
		int columnIndex = index.parent().column();
		Labels				&	labels	= _dataSet->column(columnIndex).labels();
		switch(role)
		{
		case int(specialRoles::filter):
			if(value.typeId() != QMetaType::Bool) return false;
			return setAllowFilterOnLabel(index, value.toBool());

		case int(specialRoles::value):
			return false;

		default:
		{
			QString originalLabel = tq(labels.getLabelFromRow(index.row()));
			if(labels.setLabelFromRow(index.row(), value.toString().toStdString()))
			{
				QModelIndex parent	= index.parent();
				size_t		row		= index.row(),
							col		= parent.column();

				emit dataChanged(DataSetPackage::index(row, 0, parent), DataSetPackage::index(row, columnCount(parent), parent));	//Emit dataChanged for filter

				parent = parentModelForType(parIdxType::data);
				emit dataChanged(DataSetPackage::index(0, col, parent), DataSetPackage::index(rowCount(), col, parent), { Qt::DisplayRole });

				emit labelChanged(tq(getColumnName(col)), originalLabel, tq(labels.getLabelFromRow(index.row())));
				return true;
			}
			break;
		}
		}
	}
	}

	return false;
}


void DataSetPackage::resetFilterAllows(size_t columnIndex)
{
	if(!_dataSet) return;

	_dataSet->column(columnIndex).resetFilter();

	emit labelFilterChanged();

	QModelIndex parentModel = parentModelForType(parIdxType::data);
	emit dataChanged(DataSetPackage::index(0, columnIndex,	parentModel),	DataSetPackage::index(rowCount(), columnIndex, parentModel), {int(specialRoles::filter)} );

	parentModel = parentModelForType(parIdxType::label, columnIndex);
	emit dataChanged(DataSetPackage::index(0, 0,	parentModel),			DataSetPackage::index(rowCount(parentModel), columnCount(parentModel), parentModel), {int(specialRoles::filter)} );


	emit filteredOutChanged(columnIndex);
}

bool DataSetPackage::setAllowFilterOnLabel(const QModelIndex & index, bool newAllowValue)
{
	if(parIdxTypeIs(index.parent()) != parIdxType::labelRoot)
		return false;

	bool atLeastOneRemains = newAllowValue;


	QModelIndex parent	= index.parent();
	size_t		row		= index.row(),
				col		= parent.column();


	if(int(col) > columnCount() || int(row) > rowCount(parent))
		return false;

	Column & column = _dataSet->column(col);
	Labels & labels = column.labels();

	if(!atLeastOneRemains) //Do not let the user uncheck every single one because that is useless, the user wants to uncheck row so lets see if there is another one left after that.
		for(size_t i=0; i< labels.size(); i++)
		{
			if(i != row && labels[i].filterAllows())
			{
				atLeastOneRemains = true;
				break;
			}
			else if(i == row && labels[i].filterAllows() == newAllowValue) //Did not change!
				return true;
		}

	if(atLeastOneRemains)
	{
		bool before = column.hasFilter();
		labels[row].setFilterAllows(newAllowValue);

		if(before != column.hasFilter())
			notifyColumnFilterStatusChanged(col);


		emit labelFilterChanged();
		emit dataChanged(DataSetPackage::index(row, 0, parent),	DataSetPackage::index(row, columnCount(parent), parent));	//Emit dataChanged for filter
		emit filteredOutChanged(col);

		return true;
	}
	else
		return false;

	return atLeastOneRemains;

}

int DataSetPackage::filteredOut(size_t col) const
{
	if(!_dataSet || int(col) > columnCount())
		return 0; //or -1?

	Labels &	labels		= _dataSet->column(col).labels();
	int			filteredOut = 0;

	for(const Label & label : labels)
		if(!label.filterAllows())
			filteredOut++;

	return filteredOut;
}

Qt::ItemFlags DataSetPackage::flags(const QModelIndex &index) const
{
	return Qt::ItemIsSelectable | Qt::ItemIsEnabled | (parIdxTypeIs(index) != parIdxType::data ? Qt::ItemIsEditable : Qt::NoItemFlags);
}

QHash<int, QByteArray> DataSetPackage::roleNames() const
{
	static bool						set = false;
	static QHash<int, QByteArray> roles = QAbstractItemModel::roleNames ();

	if(!set)
	{
		roles[int(specialRoles::value)]							= QString("value").toUtf8();
		roles[int(specialRoles::lines)]							= QString("lines").toUtf8();
		roles[int(specialRoles::label)]							= QString("label").toUtf8();
		roles[int(specialRoles::filter)]						= QString("filter").toUtf8();
		roles[int(specialRoles::selected)]						= QString("selected").toUtf8();
		roles[int(specialRoles::columnType)]					= QString("columnType").toUtf8();
		roles[int(specialRoles::maxColString)]					= QString("maxColString").toUtf8();
		roles[int(specialRoles::labelsHasFilter)]				= QString("labelsHasFilter").toUtf8();
		roles[int(specialRoles::columnIsComputed)]				= QString("columnIsComputed").toUtf8();
		roles[int(specialRoles::maxRowHeaderString)]			= QString("maxRowHeaderString").toUtf8();
		roles[int(specialRoles::columnWidthFallback)]			= QString("columnWidthFallback").toUtf8();
		roles[int(specialRoles::computedColumnError)]			= QString("computedColumnError").toUtf8();
		roles[int(specialRoles::computedColumnIsInvalidated)]	= QString("computedColumnIsInvalidated").toUtf8();

		set = true;
	}

	return roles;
}

void DataSetPackage::setModified(bool value)
{
	if ((!value || _isLoaded || _hasAnalysesWithoutData) && value != _isModified)
	{
		_isModified = value;
		emit isModifiedChanged();
	}
}

void DataSetPackage::setLoaded(bool loaded)
{
	if(loaded == _isLoaded)
		return;

	_isLoaded						= loaded;

	emit loadedChanged();
}

size_t DataSetPackage::findIndexByName(std::string name) const
{
	if(!_dataSet)
		throw columnNotFound(name);

	return _dataSet->columns().findIndexByName(name);
}

bool DataSetPackage::isColumnNameFree(std::string name) const
{
	try			{ findIndexByName(name); return false;}
	catch(...)	{ }

	return true;
}

bool DataSetPackage::isColumnComputed(size_t colIndex) const
{
	try
	{
		const Column & normalCol = _dataSet->columns().at(colIndex);
		_computedColumns.findIndexByName(normalCol.name());
		return true;
	}
	catch(...) {}

	return false;

}

bool DataSetPackage::isColumnComputed(std::string name) const
{
	try
	{
		_computedColumns.findIndexByName(name);
		return true;
	}
	catch(...) {}

	return false;

}

bool DataSetPackage::isColumnInvalidated(size_t colIndex) const
{
	try
	{
		const Column & normalCol		= _dataSet->columns().at(colIndex);
		const ComputedColumn & compCol	= _computedColumns[normalCol.name()];

		return compCol.isInvalidated();
	}
	catch(...) {}

	return false;
}

std::string DataSetPackage::getComputedColumnError(size_t colIndex) const
{
	try
	{
		const Column & normalCol		= _dataSet->columns().at(colIndex);
		const ComputedColumn & compCol	= _computedColumns[normalCol.name()];

		return compCol.error();
	}
	catch(...) {}

	return "";
}

void DataSetPackage::setColumnsUsedInEasyFilter(std::set<std::string> usedColumns)
{
	std::set<std::string> toUpdate(usedColumns);

	for(const auto & nameAndUsed : _columnNameUsedInEasyFilter)
		if(nameAndUsed.second)
			toUpdate.insert(nameAndUsed.first);

	_columnNameUsedInEasyFilter.clear();

	for(const std::string & col : usedColumns)
		_columnNameUsedInEasyFilter[col] = true;

	if(_dataSet)
		for(const std::string & col: toUpdate)
			try { notifyColumnFilterStatusChanged(findIndexByName(col)); } catch(columnNotFound &) {}
}


bool DataSetPackage::isColumnUsedInEasyFilter(int column) const
{
	if(_dataSet != nullptr && size_t(column) < _dataSet->columnCount())
	{
		std::string colName = _dataSet->column(column).name();
		return _columnNameUsedInEasyFilter.count(colName) > 0 && _columnNameUsedInEasyFilter.at(colName);
	}
	return false;
}

void DataSetPackage::notifyColumnFilterStatusChanged(int columnIndex)
{
	emit columnsFilteredCountChanged();
	emit headerDataChanged(Qt::Horizontal, columnIndex, columnIndex);
}


QVariant DataSetPackage::getColumnTypesWithCorrespondingIcon() const
{
	static QVariantList ColumnTypeAndIcons;

	//enum ColumnType { unknown = 0, nominal = 1, nominalText = 2, ordinal = 4, scale = 8 };

	if(ColumnTypeAndIcons.size() == 0)
	{
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("variable-nominal.png");
		ColumnTypeAndIcons.push_back("variable-nominal-text.png");
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("variable-ordinal.png");
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("variable-scale.png");
	}

	return QVariant(ColumnTypeAndIcons);
}


QVariant DataSetPackage::getColumnTitle(int column) const
{
	if(_dataSet != nullptr && column >= 0 && size_t(column) < _dataSet->columnCount())
		return tq(_dataSet->column(column).name());

	return QVariant();
}

QVariant DataSetPackage::getColumnIcon(int column) const
{
	if(_dataSet != nullptr && column >= 0 && size_t(column) < _dataSet->columnCount())
		return QVariant(int(_dataSet->column(column).getColumnType()));

	return QVariant(-1);
}

bool DataSetPackage::getColumnHasFilter(int column) const
{
	if(_dataSet != nullptr && column >= 0 && size_t(column) < _dataSet->columnCount())
		return _dataSet->column(column).hasFilter();
	
	return false;
}


int DataSetPackage::columnsFilteredCount()
{
	if(_dataSet == nullptr) return 0;

	int colsFiltered = 0;

	for(auto & col : _dataSet->columns())
		if(col.hasFilter())
			colsFiltered++;

	return colsFiltered;
}

void DataSetPackage::resetAllFilters()
{
	for(auto & col : _dataSet->columns())
		col.resetFilter();

	emit allFiltersReset();
	emit columnsFilteredCountChanged();
	emit headerDataChanged(Qt::Horizontal, 0, columnCount());
}

bool DataSetPackage::setColumnType(int columnIndex, columnType newColumnType)
{
	if (_dataSet == nullptr)
		return true;

	columnTypeChangeResult	feedback;

	enlargeDataSetIfNecessary([&](){ feedback = _dataSet->column(columnIndex).changeColumnType(newColumnType); }, "setColumnType");

	if (feedback == columnTypeChangeResult::changed) //Everything went splendidly
	{
		emit headerDataChanged(Qt::Orientation::Horizontal, columnIndex, columnIndex);
		emit columnDataTypeChanged(tq(_dataSet->column(columnIndex).name()));
	}
	else
	{
		QString informUser = tr("Something went wrong converting columntype, but it is unclear what.");

		switch(feedback)
		{
		default:	break; //default text already set above.
		case columnTypeChangeResult::cannotConvertDoubleValueToInteger: //Aka something failed converting to ordinal because to nominal would yield nominalText
			informUser = tr("Failed to convert column to ordinal: values contain decimals.");
			break;

		case columnTypeChangeResult::cannotConvertStringValueToDouble: //Aka something failed converting to scaler from nominaltext
			informUser = tr("Failed to convert column to continuous: values contain text.");
			break;

		case columnTypeChangeResult::cannotConvertStringValueToInteger: //Aka something failed converting to ordinal from nominaltext
			informUser = tr("Failed to convert column to ordinal: values contain text or decimals.");
			break;
		}

		MessageForwarder::showWarning("Changing column type failed", informUser);
	}

	return feedback == columnTypeChangeResult::changed;
}

void DataSetPackage::refreshColumn(QString columnName)
{
	if(!_dataSet) return;

	int colIndex = getColumnIndex(columnName);

	if(colIndex >= 0)
	{
		QModelIndex p = parentModelForType(parIdxType::data);
		emit dataChanged(index(0, colIndex, p), index(rowCount(p), colIndex, p));
		emit headerDataChanged(Qt::Horizontal, colIndex, colIndex);
	}
}

void DataSetPackage::columnWasOverwritten(std::string columnName, std::string)
{
	for(size_t col=0; col<_dataSet->columns().columnCount(); col++)
		if(_dataSet->columns()[col].name() == columnName)
			emit dataChanged(index(0, col, parentModelForType(parIdxType::data)), index(rowCount()-1, col, parentModelForType(parIdxType::data)));
}

int DataSetPackage::setColumnTypeFromQML(int columnIndex, int newColumnType)
{
	setColumnType(columnIndex, columnType(newColumnType));
	return int(getColumnType(columnIndex));
}

void DataSetPackage::beginSynchingData()
{
	beginLoadingData();
	_synchingData = true;
}

void DataSetPackage::endSynchingDataChangedColumns(std::vector<std::string>	&	changedColumns)
{
	 std::vector<std::string>				missingColumns;
	 std::map<std::string, std::string>		changeNameColumns;

	endSynchingData(changedColumns, missingColumns, changeNameColumns, false, false);
}

void DataSetPackage::endSynchingData(std::vector<std::string>			&	changedColumns,
									 std::vector<std::string>			&	missingColumns,
									 std::map<std::string, std::string>	&	changeNameColumns,  //origname -> newname
									 bool									rowCountChanged,
									 bool									hasNewColumns)
{

	endLoadingData();
	_synchingData = false;
	//We convert all of this stuff to qt containers even though this takes time etc. Because it needs to go through a (queued) connection and it might not work otherwise
	emit datasetChanged(tq(changedColumns), tq(missingColumns), tq(changeNameColumns), rowCountChanged, hasNewColumns);
}


void DataSetPackage::beginLoadingData()
{
	JASPTIMER_SCOPE(DataSetPackage::beginLoadingData);

	enginesPrepareForData();
	beginResetModel();
}

void DataSetPackage::endLoadingData()
{
	JASPTIMER_SCOPE(DataSetPackage::endLoadingData);

	regenerateInternalPointers();
	endResetModel();
	enginesReceiveNewData();

	emit modelInit();
	emit dataSetChanged();
}

void DataSetPackage::setDataSetSize(size_t columnCount, size_t rowCount)
{
	enlargeDataSetIfNecessary([&]()
	{
		_dataSet->setColumnCount(columnCount);

		if (rowCount > 0)
			_dataSet->setRowCount(rowCount);
	}, "setDataSetSize");
}

bool DataSetPackage::initColumnAsScale(size_t colNo, std::string newName, const std::vector<double> & values)
{
	bool out = false;

	enlargeDataSetIfNecessary([&]()
	{
		Column &column = _dataSet->column(colNo);
		column.setName(newName);
		out = column.setColumnAsScale(values);
	}, "initColumnAsScale");

	emit columnNamesChanged();

	return out;
}

std::map<int, std::string> DataSetPackage::initColumnAsNominalText(size_t colNo, std::string newName, const std::vector<std::string> & values, const std::map<std::string, std::string> & labels)
{
	std::map<int, std::string> out;

	enlargeDataSetIfNecessary([&]()
	{
		Column &column = _dataSet->column(colNo);
		column.setName(newName);
		out = column.setColumnAsNominalText(values, labels);
	}, "initColumnAsNominalText");

	emit columnNamesChanged();

	return out;
}

bool DataSetPackage::initColumnAsNominalOrOrdinal(size_t colNo, std::string newName, const std::vector<int> & values, bool is_ordinal)
{
	bool out = false;

	enlargeDataSetIfNecessary([&]()
	{
		Column &column = _dataSet->column(colNo);
		column.setName(newName);
		out = column.setColumnAsNominalOrOrdinal(values, is_ordinal);
	}, "initColumnAsNominalOrOrdinal");

	emit columnNamesChanged();

	return out;
}

bool DataSetPackage::initColumnAsNominalOrOrdinal(size_t colNo, std::string newName, const std::vector<int> & values, const std::map<int, std::string> &uniqueValues, bool is_ordinal)
{
	bool out = false;

	enlargeDataSetIfNecessary([&]()
	{
		Column &column = _dataSet->column(colNo);
		column.setName(newName);
		out = column.setColumnAsNominalOrOrdinal(values, uniqueValues, is_ordinal);
	}, "initColumnAsNominalOrOrdinal");

	emit columnNamesChanged();

	return out;
}

bool DataSetPackage::initColumnAsScale(QVariant colID, std::string newName, const std::vector<double> & values)
{
	if(colID.typeId() == QMetaType::Int || colID.typeId() == QMetaType::UInt)
	{
		int colNo = colID.typeId() == QMetaType::Int ? colID.toInt() : colID.toUInt();
		return initColumnAsScale(colNo, newName, values);
	}
	else
		return initColumnAsScale(colID.toString().toStdString(), newName, values);
}

std::map<int, std::string> DataSetPackage::initColumnAsNominalText(QVariant colID, std::string newName, const std::vector<std::string> & values, const std::map<std::string, std::string> & labels)
{
	if(colID.typeId() == QMetaType::Int || colID.typeId() == QMetaType::UInt)
	{
		int colNo = colID.typeId() == QMetaType::Int ? colID.toInt() : colID.toUInt();
		return initColumnAsNominalText(colNo, newName, values, labels);
	}
	else
		return initColumnAsNominalText(colID.toString().toStdString(), newName, values, labels);
}

bool DataSetPackage::initColumnAsNominalOrOrdinal(	QVariant colID, std::string newName, const std::vector<int> & values, bool is_ordinal)
{
	if(colID.typeId() == QMetaType::Int || colID.typeId() == QMetaType::UInt)
	{
		int colNo = colID.typeId() == QMetaType::Int ? colID.toInt() : colID.toUInt();
		return initColumnAsNominalOrOrdinal(colNo, newName, values, is_ordinal);
	}
	else
		return initColumnAsNominalOrOrdinal(colID.toString().toStdString(), newName, values, is_ordinal);
}

bool DataSetPackage::initColumnAsNominalOrOrdinal(	QVariant colID, std::string newName, const std::vector<int> & values, const std::map<int, std::string> &uniqueValues, bool is_ordinal)
{
	if(colID.typeId() == QMetaType::Int || colID.typeId() == QMetaType::UInt)
	{
		int colNo = colID.typeId() == QMetaType::Int ? colID.toInt() : colID.toUInt();
		return initColumnAsNominalOrOrdinal(colNo, newName, values, uniqueValues, is_ordinal);
	}
	else
		return initColumnAsNominalOrOrdinal(colID.toString().toStdString(), newName, values, uniqueValues, is_ordinal);
}

void DataSetPackage::enlargeDataSetIfNecessary(std::function<void()> tryThis, const char * callerText)
{
	while(true)
	{
		try	{ tryThis(); return; }
		catch (boost::interprocess::bad_alloc &)
		{
			try							{ setDataSet(SharedMemory::enlargeDataSet(_dataSet)); }
			catch (std::exception &)	{ throw std::runtime_error("Out of memory: this data set is too large for your computer's available memory");	}
		}
		catch (std::exception & e)	{ Log::log() << "std::exception in enlargeDataSetIfNecessary for " << callerText << ": " << e.what() << std::endl;	return;}
		catch (...)					{ Log::log() << "something went wrong while enlargeDataSetIfNecessary for " << callerText << "..." << std::endl;	return;}

	}
}

std::vector<std::string> DataSetPackage::getColumnNames(bool includeComputed)
{
	std::vector<std::string> names;

	if(_dataSet)
		for(const Column & col : _dataSet->columns())
			if(includeComputed || !isColumnComputed(col.name()))
				names.push_back(col.name());

	return names;
}

bool DataSetPackage::isColumnDifferentFromStringValues(std::string columnName, std::vector<std::string> strVals)
{
	try
	{
		Column & col = _dataSet->column(columnName);
		return col.isColumnDifferentFromStringValues(strVals);
	}
	catch(columnNotFound & ) { }

	return true;
}

void DataSetPackage::renameColumn(std::string oldColumnName, std::string newColumnName)
{
	try
	{
		Column & col = _dataSet->column(oldColumnName);
		col.setName(newColumnName);
		emit columnNamesChanged();
	}
	catch(...)
	{
		Log::log() << "Couldn't rename column from '" << oldColumnName << "' to '" << newColumnName << "'" << std::endl;
	}
}

void DataSetPackage::writeDataSetToOStream(std::ostream & out, bool includeComputed)
{
	std::vector<Column*> cols;

	int columnCount = _dataSet->columnCount();
	for (int i = 0; i < columnCount; i++)
	{
		Column &column		= _dataSet->column(i);
		std::string name	= column.name();

		if(!isColumnComputed(name) || includeComputed)
			cols.push_back(&column);
	}


	for (size_t i = 0; i < cols.size(); i++)
	{
		Column *column		= cols[i];
		std::string name	= column->name();

		if (stringUtils::escapeValue(name))	out << '"' << name << '"';
		else								out << name;

		if (i < cols.size()-1)	out << ",";
		else					out << "\n";

	}

	size_t rows = rowCount();

	for (size_t r = 0; r < rows; r++)
		for (size_t i = 0; i < cols.size(); i++)
		{
			Column *column = cols[i];

			std::string value = column->getOriginalValue(r);
			if (value != ".")
			{
				if (stringUtils::escapeValue(value))	out << '"' << value << '"';
				else									out << value;
			}

			if (i < cols.size()-1)		out << ",";
			else if (r != rows-1)		out << "\n";
		}
}

std::string DataSetPackage::getColumnTypeNameForJASPFile(columnType columnType)
{
	switch(columnType)
	{
	case columnType::nominal:			return "Nominal";
	case columnType::nominalText:		return "NominalText";
	case columnType::ordinal:			return "Ordinal";
	case columnType::scale:				return "Continuous";
	default:							return "Unknown";
	}
}

columnType DataSetPackage::parseColumnTypeForJASPFile(std::string name)
{
	if (name == "Nominal")				return  columnType::nominal;
	else if (name == "NominalText")		return  columnType::nominalText;
	else if (name == "Ordinal")			return  columnType::ordinal;
	else if (name == "Continuous")		return  columnType::scale;
	else								return  columnType::unknown;
}

Json::Value DataSetPackage::columnToJsonForJASPFile(size_t columnIndex, Json::Value & labelsData, size_t & dataSize)
{
	Column &column					= _dataSet->column(columnIndex);
	std::string name				= column.name();
	Json::Value columnMetaData		= Json::Value(Json::objectValue);
	columnMetaData["name"]			= Json::Value(name);
	columnMetaData["measureType"]	= Json::Value(getColumnTypeNameForJASPFile(column.getColumnType()));

	if(column.getColumnType() == columnType::scale)
	{
		   columnMetaData["type"] = Json::Value("number");
		   dataSize += sizeof(double) * rowCount();
	}
	else
	{
		   columnMetaData["type"] = Json::Value("integer");
		   dataSize += sizeof(int) * rowCount();
	}


	if (column.getColumnType() != columnType::scale)
	{
		Labels &labels = column.labels();

		if (labels.size() > 0)
		{
			Json::Value &columnLabelData	= labelsData[name];
			Json::Value &labelsMetaData		= columnLabelData["labels"];
			int labelIndex = 0;

			for (const Label &label : labels)
			{
				Json::Value keyValueFilterPair(Json::arrayValue);

				keyValueFilterPair.append(label.value());
				keyValueFilterPair.append(label.text());
				keyValueFilterPair.append(label.filterAllows());

				labelsMetaData.append(keyValueFilterPair);
				labelIndex += 1;
			}

			Json::Value &orgStringValuesMetaData	= columnLabelData["orgStringValues"];
			std::map<int, std::string> &orgLabels	= labels.getOrgStringValues();
			for (const auto & pair : orgLabels)
			{
				Json::Value keyValuePair(Json::arrayValue);
				keyValuePair.append(pair.first);
				keyValuePair.append(pair.second);
				orgStringValuesMetaData.append(keyValuePair);
			}
		}
	}

	return columnMetaData;
}

void DataSetPackage::columnLabelsFromJsonForJASPFile(Json::Value xData, Json::Value columnDesc, size_t columnIndex, std::map<std::string, std::map<int, int> > & mapNominalTextValues)
{
	std::string name					= columnDesc["name"].asString();
	Json::Value &orgStringValuesDesc	= columnDesc["orgStringValues"];
	Json::Value &labelsDesc				= columnDesc["labels"];
	std::map<int, int>& mapValues		= mapNominalTextValues[name];	// This is needed for old JASP file where factor keys where not filled in the right way

	if (labelsDesc.isNull() &&  ! xData.isNull())
	{
		Json::Value &columnlabelData = xData[name];

		if (!columnlabelData.isNull())
		{
			labelsDesc			= columnlabelData["labels"];
			orgStringValuesDesc = columnlabelData["orgStringValues"];
		}
	}

	enlargeDataSetIfNecessary([&]()
	{
		Column &column = _dataSet->column(columnIndex);
		columnType columnType = parseColumnTypeForJASPFile(columnDesc["measureType"].asString());

		column.setName(name);
		column.setColumnType(columnType);

		Labels &labels = column.labels();
		labels.clear();
		int index = 1;

		for (Json::Value & keyValueFilterTrip : labelsDesc)
		{
			int zero		= 0; //MSVC complains on int(0) with: error C2668: 'Json::Value::get': ambiguous call to overloaded function
			int key			= keyValueFilterTrip.get(zero,		Json::nullValue).asInt();
			std::string val = keyValueFilterTrip.get(1,			Json::nullValue).asString();
			bool fil		= keyValueFilterTrip.get(2,			true).asBool();
			int labelValue	= key;

			if (columnType == columnType::nominalText)
			{
				labelValue		= index;
				mapValues[key]	= labelValue;
			}

			labels.add(labelValue, val, fil, columnType == columnType::nominalText);

			index++;
		}

		if (!orgStringValuesDesc.isNull())
		{
			for (Json::Value & keyValuePair : orgStringValuesDesc)
			{
				int zero		= 0; //MSVC complains on int(0) with: error C2668: 'Json::Value::get': ambiguous call to overloaded function
				int key			= keyValuePair.get(zero,	Json::nullValue).asInt();
				std::string val = keyValuePair.get(1,		Json::nullValue).asString();
				if (mapValues.find(key) != mapValues.end())
					key = mapValues[key];
				else
					Log::log() << "Cannot find key " << key << std::flush;
				labels.setOrgStringValues(key, val);
			}
		}

	}, "columnLabelsFromJsonForJASPFile");

	emit columnNamesChanged();
}

std::vector<int> DataSetPackage::getColumnDataInts(size_t columnIndex)
{
	if(_dataSet == nullptr) return {};

	Column & col = _dataSet->column(columnIndex);
	return std::vector<int>(col.AsInts.begin(), col.AsInts.end());
}

std::vector<double> DataSetPackage::getColumnDataDbls(size_t columnIndex)
{
	if(_dataSet == nullptr) return {};

	Column & col = _dataSet->column(columnIndex);
	return std::vector<double>(col.AsDoubles.begin(), col.AsDoubles.end());
}

void DataSetPackage::setColumnDataInts(size_t columnIndex, std::vector<int> ints)
{
	Column & col = _dataSet->column(columnIndex);
	Labels & lab = col.labels();

	for(size_t r = 0; r<ints.size(); r++)
	{
		int value = ints[r];

		//Maybe something went wrong somewhere and we do not have labels for all values...
		try
		{
			if (value != std::numeric_limits<int>::lowest())
				lab.getLabelObjectFromKey(value);
		}
		catch (const labelNotFound &)
		{
			Log::log() << "Value '" << value << "' in column '" << col.name() << "' did not have a corresponding label, adding one now.\n";
			lab.add(value, std::to_string(value), true, col.getColumnType() == columnType::nominalText);
		}

		col.setValue(r, value);
	}
}


void DataSetPackage::setColumnDataDbls(size_t columnIndex, std::vector<double> dbls)
{
	Column & col = _dataSet->column(columnIndex);

	for(size_t r = 0; r<dbls.size(); r++)
		col.setValue(r, dbls[r]);
}

void DataSetPackage::emptyValuesChangedHandler()
{
	if (isLoaded())
	{
		beginSynchingData();
		std::map<std::string, std::map<int, std::string> > emptyValuesChanged;
		std::vector<std::string> colChanged;

		enlargeDataSetIfNecessary([&](){ emptyValuesChanged = _dataSet->resetEmptyValues(emptyValuesMap()); }, "emptyValuesChangedHandler");

		for (auto & it : emptyValuesChanged)
		{
			colChanged.push_back(it.first);
			storeInEmptyValues(it.first, it.second);
		}

		endSynchingDataChangedColumns(colChanged);
	}
}

bool DataSetPackage::setFilterData(std::string filter, std::vector<bool> filterResult)
{
	setDataFilter(filter);

	bool someFilterValueChanged = _dataSet ? _dataSet->setFilterVector(filterResult) : false;

	if(someFilterValueChanged) //We could also send exactly those cells that were changed if we were feeling particularly inclined to write the code...
	{
		//emit dataChanged(index(0, 0, parentModelForType(parIdxType::filter)),	index(rowCount(), 0,				parentModelForType(parIdxType::filter)));
		//This actually lets the whole application freeze when a filter is undone... -> emit dataChanged(index(0, 0, parentModelForType(parIdxType::data)),		index(rowCount(), columnCount(),	parentModelForType(parIdxType::data)));

		beginResetModel();
		regenerateInternalPointers();
		endResetModel();
	}

	return someFilterValueChanged;
}

columnType DataSetPackage::getColumnType(std::string columnName)	const
{
	int colIndex = getColumnIndex(columnName);

	if(colIndex > -1)	return getColumnType(colIndex);
	else				return columnType::unknown;
}

QStringList DataSetPackage::getColumnLabelsAsStringList(std::string columnName)	const
{
	int colIndex = getColumnIndex(columnName);

	if(colIndex > -1)	return getColumnLabelsAsStringList(colIndex);
	else				return QStringList();;
}

QStringList DataSetPackage::getColumnLabelsAsStringList(size_t columnIndex)	const
{
	QStringList list;
	if(columnIndex < 0 || columnIndex >= columnCount()) return list;

	for(const Label & label : _dataSet->column(columnIndex).labels())
		list.append(tq(label.text()));

	return list;
}

void DataSetPackage::labelMoveRows(size_t column, std::vector<size_t> rows, bool up)
{
	Labels & labels = _dataSet->column(column).labels();

	std::vector<Label> new_labels(labels.begin(), labels.end());

	int mod = up ? -1 : 1;

	std::sort(rows.begin(), rows.end(), [&](const size_t & l, const size_t & r) { return up ? l < r : r < l; });

	for (size_t row : rows)
		if(int(row) + mod < 0 || int(row) + mod >= int(labels.size()))
			return; //Because we can't move out of our labels for obvious reasons

	std::set<size_t> rowsChanged;

	for (size_t row : rows)
	{
		iter_swap(new_labels.begin() + row, new_labels.begin() + (row + mod));
		rowsChanged.insert(row);
		rowsChanged.insert(row + mod);
	}

	labels.set(new_labels);
	QModelIndex p = parentModelForType(parIdxType::label, column);

	for(size_t row: rowsChanged)
		emit dataChanged(index(row, 0, p), index(row, columnCount(p), p));

	emit labelsReordered(tq(getColumnName(column)));
}

void DataSetPackage::labelReverse(size_t column)
{
	Labels & labels = _dataSet->column(column).labels();

	std::vector<Label> new_labels(labels.begin(), labels.end());

	std::reverse(new_labels.begin(), new_labels.end());
	labels.set(new_labels);
	QModelIndex p = parentModelForType(parIdxType::label, column);

	emit dataChanged(index(0, 0, p), index(rowCount(p), columnCount(p), p));
	emit labelsReordered(tq(getColumnName(column)));
}

void DataSetPackage::columnSetDefaultValues(std::string columnName, columnType columnType)
{
	if(!_dataSet) return;

	int colIndex = getColumnIndex(columnName);

	if(colIndex >= 0)
	{
		QModelIndex p = parentModelForType(parIdxType::data);
		_dataSet->column(colIndex).setDefaultValues(columnType);
		emit dataChanged(index(0, colIndex, p), index(rowCount(p), colIndex, p));
		emit headerDataChanged(Qt::Horizontal, colIndex, colIndex);
	}
}

bool DataSetPackage::createColumn(std::string name, columnType columnType)
{
	if(getColumnIndex(name) >= 0) return false;

	size_t newColumnIndex	= columnCount();

	enginesPrepareForData();
	beginResetModel();
	setDataSetColumnCount(newColumnIndex + 1);

	_dataSet->columns().initializeColumnAs(newColumnIndex, name)->setDefaultValues(columnType);
	regenerateInternalPointers();
	endResetModel();
	enginesReceiveNewData();

	return true;
}

void DataSetPackage::removeColumn(std::string name)
{
	int colIndex = getColumnIndex(name);
	if(colIndex == -1) return;

	emit columnAboutToBeRemoved(colIndex);

	beginResetModel();
	_dataSet->columns().removeColumn(name);
	regenerateInternalPointers();
	endResetModel();

	if(isLoaded()) setModified(true);

	if (!_synchingData) // If we are synchronising, the datasetChanged is already called
		emit datasetChanged({}, {tq(name)}, {}, false, false);
}

std::vector<bool> DataSetPackage::filterVector()
{
	std::vector<bool> out;

	if(_dataSet)
	{
		out.reserve(_dataSet->filterVector().size());
		for(bool f : _dataSet->filterVector())
			out.push_back(f);
	}

	return out;
}


void DataSetPackage::setCurrentFile(QString currentFile)
{
	if (_currentFile == currentFile)
		return;

	_currentFile = currentFile;
	emit currentFileChanged();

	QFileInfo	file(_currentFile);
	QUrl		url(_currentFile);

#ifdef _WIN32
	setFolder(file.exists() ? file.absolutePath().replace('/', '\\')	: url.isValid() ? "OSF" : "");
#else
	setFolder(file.exists() ? file.absolutePath()						: url.isValid() ? "OSF" : "");
#endif
}

void DataSetPackage::setFolder(QString folder)
{
	//Remove the last part if it is the name of the file regardless of extension
	QString _name	= name();
	int		i		= _name.size();
	for(; i < folder.size(); i++)
		if(folder.right(i).startsWith(_name))
		{
			folder = folder.left(folder.size() - i);
			break;
		}
#ifdef _WIN32
		else if(folder.right(i).contains('\\'))	break;
#else
		else if(folder.right(i).contains('/'))	break;
#endif

	if (_folder == folder)
		return;

	_folder = folder;
	emit folderChanged();
}


QString DataSetPackage::name() const
{
	QFileInfo	file(_currentFile);

	if(file.completeBaseName() != "")
		return file.completeBaseName();

	return "JASP";
}

QString DataSetPackage::windowTitle() const
{
	QString name	= DataSetPackage::name(),
			folder	= DataSetPackage::folder();
	
#ifdef _WIN32
	if(folder.startsWith(AppDirs::examples().replace('/', '\\')))
#else
	if(folder.startsWith(AppDirs::examples()))
#endif
		folder = "";

	folder = folder == "" ? "" : "      (" + folder + ")";

	return name + (isModified() ? "*" : "") + folder;
}

// This function can be called from a different thread then where the underlying value for isReady() is set, but I don't think a mutex or whatever is necessary here. What could go wrong with checking a boolean?
// Also this was already the case, so I'm not making things worse here...
void DataSetPackage::waitForExportResultsReady() 
{ 
	int maxSleepTime	= 10000,
		sleepTime		= 100,
		delay			= 0;
	
	while (!isReady())
	{
		if (delay > maxSleepTime)
			break;
		
		Utils::sleep(sleepTime);
		delay += sleepTime;
	}
	
	if(!isReady())
		Log::log() << "Results were not exported properly!" << std::endl; //Should we maybe create a dummy result that explains something went wrong with the upload? Should we abort saving? What is going on?
}
