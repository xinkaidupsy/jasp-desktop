import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name:			"Learn Bayes"
	title : 		qsTr("Learn Bayes")
	description:	qsTr("Learning Bayesian Statistics with JASP")
	icon:			"learn-bayes.svg"
	version:		"0.14"
	requiresData:	false
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"jasp-stats.org"
	license:		"GPL (>= 2)"

	Package { name: "HDInterval" }
	Package { name: "DT" }
	GroupTitle
	{
		title: 	"Counts"
		icon:	"analysis-bayesian-crosstabs.svg"
	}
	
	Analysis
	{
		title:	"Binomial Estimation"
		qml:	"LSbinomialestimation.qml"
		func:	"LSbinomialestimation"
	}

	Analysis
	{
		title:	"Binomial Testing"
		qml:	"LSbinomialtesting.qml"
		func:	"LSbinomialtesting"
	}

	GroupTitle
	{
		title: 	"Problem of Points"
	}
	
	Analysis
	{
		title:	"Game of Chance"
		qml:	"LSgameofchance.qml"
		func:	"LSgameofchance"
	}

	Analysis
	{
		title:	"Game of Skills"
		qml:	"LSgameofskills.qml"
		func:	"LSgameofskills"
	}
}
