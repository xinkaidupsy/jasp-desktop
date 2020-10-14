#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# When making changes to this file always mention @koenderks as a 
# reviewer in the Pull Request

################################################################################
################## The Audit Workflow ##########################################
################################################################################

.auditWorkflow <- function(options, jaspResults){

  ### PROCEDURE STAGE ###
  .auditProcedureStage(options, jaspResults)
  ready <- .auditReadyForNextStage(options, jaspResults, stage = "procedure")
  if(!ready) return() # Stop if no sampling objective is selected

  ### PLANNING STAGE ###
  .auditPlanningStage(options, jaspResults, workflow = TRUE)

  ready <- .auditReadyForNextStage(options, jaspResults, stage = "planning")
  if(!ready) return() # Stop if "To Selection" is not pressed

  ### SELECTION STAGE ###
  .auditSelectionStage(options, jaspResults, workflow = TRUE)

  ### EXECUTION STAGE ###
  .auditExecutionStage(options, jaspResults)

  ready <- .auditReadyForNextStage(options, jaspResults, stage = "execution")
  if(!ready) return() # Stop if "To Evaluation" is not pressed

  ### EVALUATION STAGE ###
  .auditEvaluationStage(options, jaspResults, workflow = TRUE)

  ### CONCLUSION STAGE ###
  .auditConclusionStage(options, jaspResults)
}

################################################################################
################## The Separate Stages of the Audit Workflow ###################
################################################################################

#####################################
######### PROCEDURE STAGE ###########
#####################################

.auditProcedureStage <- function(options, jaspResults){

  # Extract the record number and book value columns
  dataset <- .auditReadDataset(options, jaspResults, stage = "procedure")
  
  # Check for errors due to incompatible options (variables)
  .auditErrorCheckInputOptions(options, dataset, analysisContainer = NULL, 
                              stage = "procedure")

  # Deduct the nessecary values from the input options
  planningOptions <- .auditInputOptions(options, dataset, jaspResults,
                                        stage = "planning", rawData = TRUE)

  # Create the procedure paragraph
  .auditExplanatoryText(options, planningOptions, stageContainer = NULL, stageState = NULL, 
                        jaspResults, stage = "procedure", positionInContainer = 1)

  # Create the audit risk model paragraph
  .auditRiskModelParagraph(options, jaspResults, position = 2)

  # --- TABLES

  .auditCreateTableNumber(jaspResults) # Initialize table numbers
  
  # Create a table containing descriptive statistics for the book values
  .auditBookValueDescriptiveTable(options, planningOptions, jaspResults,
                                  positionInContainer = 2)
  
  # --- PLOTS

  .auditCreateFigureNumber(jaspResults) # Initialize figure numbers

  # Create a plot of the population book values (if the user wants it)
  .auditBookValueDistributionPlot(dataset, options, planningOptions, jaspResults, 
                                  positionInContainer = 3)

}

#####################################
######### PLANNING STAGE ############
#####################################

.auditPlanningStage <- function(options, jaspResults, workflow){

  if(workflow){

    .fillCriticalTransactions(options, jaspResults)

    # Deduct the nessecary values from the input options
    planningOptions <- .auditInputOptions(options, dataset = NULL, jaspResults,
                                          stage = "planning", rawData = TRUE)

  } else if(!workflow){

    .auditCreateTableNumber(jaspResults)  # Initialize table numbers
    .auditCreateFigureNumber(jaspResults) # Initialize figure numbers

    # Deduct the nessecary values from the input options
    planningOptions <- .auditInputOptions(options, dataset = NULL, jaspResults,
                                          stage = "planning", rawData = FALSE)
    
    # Create the procedure paragraph
    .auditExplanatoryText(options, planningOptions, stageContainer = NULL, stageState = NULL, 
                          jaspResults, stage = "procedure", positionInContainer = 1)

    # Create the audit risk model paragraph
    .auditRiskModelParagraph(options, jaspResults, position = 2)
  }

  # Check if the options have valid values for running the analysis
  ready <- .auditReadyForAnalysis(options, planningOptions, stage = "planning")
  if(!(options[['performanceMateriality']] || options[["minimumPrecision"]])) return() # Stop if no sampling objective is selected

  # Create the container that holds the planning output
  planningContainer <- .auditAnalysisContainer(jaspResults, stage = "planning",
                                               position = 3)

  # Perfrom early error checks
  .auditErrorCheckInputOptions(options, dataset = NULL, planningContainer, 
                              stage = "planning", ready, planningOptions)

  # Get the planning state if it exists, otherwise make one
  planningState <- .auditPlanningState(options, planningOptions, planningContainer, 
                                       ready, jaspResults)

  # Create explanatory text for the planning
  .auditExplanatoryText(options, planningOptions, planningContainer, planningState, 
                        jaspResults, stage = "planning", positionInContainer = 1)

  # --- TABLES

  # Create the summary table
  .auditPlanningSummaryTable(options, planningOptions, planningState, planningContainer, 
                             jaspResults, ready, positionInContainer = 2)

  if(options[["bayesianAnalysis"]]){
    # Create the implicit sample table
    .auditImplicitSampleTable(options, planningState, planningContainer, jaspResults,
                              ready, positionInContainer = 3)

    # Cerate the prior and posterior statistics table
    .auditPriorAndExpectedPosteriorStatisticsTable(options, planningState, planningContainer, 
                                                   jaspResults, ready, positionInContainer = 4)
  }

  # --- PLOTS

  # Create the sample size comparison plot
  .sampleSizeComparisonPlot(options, planningOptions, planningState, planningContainer, 
                            jaspResults, ready, positionInContainer = 5)

  if(!options[["bayesianAnalysis"]]){
    # Create the implied sampling distribution plot
    .auditSamplingDistributionPlot(options, planningOptions, planningState, planningContainer, 
                                   jaspResults, ready, positionInContainer = 7)
  } else if(options[["bayesianAnalysis"]]){
    # Create the prior and expected posterior plot
    .auditPlanningPlotPrior(options, planningOptions, planningState, planningContainer,
                            jaspResults, ready, positionInContainer = 7)
  }
}

#####################################
######### SELECTION STAGE ###########
#####################################

.auditSelectionStage <- function(options, jaspResults, workflow){

  if(workflow){

    # Create the container that holds the selection output
    selectionContainer <- .auditAnalysisContainer(jaspResults, stage = "selection-workflow", 
                                                  position = 4)

    # Read in additional variables
    dataset <- .auditAddSelectionColumns(options, jaspResults)

    # Import options and results from the planning stage 
    selectionOptions <- .auditInputOptions(options, dataset, jaspResults,
                                          stage = "planning", rawData = TRUE)

    planningContainer   <- jaspResults[["planningContainer"]]
    planningState       <- planningContainer[["planningState"]]$object

    error <- .auditErrorCheckInputOptions(options, dataset, selectionContainer, 
                                          stage = "selection-workflow")
    if(error) return() # Quit on errors

    if(is.null(planningState)) return() # Quit if no planning was done

    # Perform the sampling
    selectionState <- .auditSelectionState(dataset, options, planningState, selectionContainer)

  } else if(!workflow){

    .auditCreateFigureNumber(jaspResults) # Initialize figure numbers
    .auditCreateTableNumber(jaspResults) # Initialize table numbers

    # Create a custom container for the selection analysis
    selectionContainer <- .auditAnalysisContainer(jaspResults, stage = "selection",
                                                  position = 1)

    # Read in the relevant variables from the data set
    dataset <- .auditReadDataset(options, jaspResults, stage = "selection")

    # Deduce relevant quantities from input options
    selectionOptions <- .auditInputOptions(options, dataset, jaspResults,
                                          stage = "selection")

    # Check for errors due to incompatible options
    error <- .auditErrorCheckInputOptions(options, dataset, selectionContainer, 
                                          stage = "selection", analysisOptions = selectionOptions)
    if(error) return() # Quit on errors

    options[["materiality"]] <- ifelse(options[["selectionType"]] == "musSampling",
                                      yes = "materialityAbsolute",
                                      no = "materialityRelative")

    # Create a planning state
    planningState <- .auditBackwardsState(options, stage = "selection")

    # Perform error checks
    .auditErrorCheckInputOptions(options, dataset, analysisContainer = NULL, 
                                stage = "procedure")

    # Perform the sampling
    selectionState <- .auditSampling(dataset, options, planningState, selectionContainer)

    # Add the sample indicator to the data
    .auditAddSelectionIndicator(options, selectionOptions, selectionState, jaspResults)

  }

  # Create explanatory text for the selection
  .auditExplanatoryText(options, selectionOptions, selectionContainer, selectionState, 
                      jaspResults, stage = "selection", positionInContainer = 1, prevState = planningState)

  # --- TABLES

  # Create a table containing information about the selection process
  .auditSelectionSummaryTable(options, selectionOptions, planningState,
                              selectionState, selectionContainer, jaspResults,
                              positionInContainer = 2, dataset)

	# Create a table containing information about the monetary interval selection
	.auditSelectionMonetaryIntervalTable(options,
												selectionOptions, 
												planningState,
												selectionContainer, 
												selectionState,  
												jaspResults, 
												positionInContainer = 4, 
												dataset)

  # Create a table containing descriptive statistics of the sample
  .auditSelectionDescriptivesTable(options, selectionState, selectionContainer,
                                   jaspResults, positionInContainer = 5)

  # Create a table displaying the selection
  .auditSelectionSampleTable(options, selectionState, selectionContainer,
                             jaspResults, positionInContainer = 6, selectionOptions)

  # --- PLOTS

#   if(!workflow){
#     # Create a collection of plots comparing the population to the sample values
#     .auditSelectionHistograms(options, dataset, selectionState, selectionContainer,
#                               jaspResults, positionInContainer = 6)
#   }
}

#####################################
######### EXECUTION STAGE ###########
#####################################

.auditExecutionStage <- function(options, jaspResults){

  if(options[["pasteVariables"]]){  
    # Add the two computed colums to the data set
    planningOptions <- .auditInputOptions(options, dataset = NULL, jaspResults,
                                          stage = "planning", rawData = TRUE)
    selectionState <- .auditSelectionState(dataset, options, jaspResults[["planningState"]], 
                                           jaspResults[["selectionContainer"]])

    selectionState                <- data.frame(selectionState)
    dataset                       <- .readDataSetToEnd(columns.as.numeric = options[["recordNumberVariable"]])
    sampleFilter                  <- rep(0, planningOptions[["populationSize"]])
    rowNumber                     <- selectionState[["rowNumber"]]
    sampleFilter[rowNumber]       <- selectionState[["count"]]
    sampleFilter                  <- as.numeric(sampleFilter)
    auditDataVariable             <- rep(NA, planningOptions[["populationSize"]])
    auditDataVariable[options[["performAudit"]][[1]]$rowIndices] <- options[["performAudit"]][[1]]$values

    if(is.null(jaspResults[["sampleFilter"]]))  
      jaspResults[["sampleFilter"]] <- createJaspColumn(columnName = options[["sampleFilter"]], dependencies = "sampleFilter")
    if(is.null(jaspResults[["variableName"]]))  
      jaspResults[["variableName"]] <- createJaspColumn(columnName = options[["variableName"]], dependencies = "variableName")

    jaspResults[["sampleFilter"]]$setScale(sampleFilter)
    jaspResults[["variableName"]]$setScale(auditDataVariable)

  }
}

#####################################
######### EVALUATION STAGE ##########
#####################################

.auditEvaluationStage <- function(options, jaspResults, workflow){

  if(workflow){

    # Create the container that holds the selection output
    evaluationContainer <- .auditAnalysisContainer(jaspResults, stage = "evaluation-workflow", 
                                                  position = 5)
    # Read in additional variables
    dataset <- .auditAddEvaluationColumns(options, jaspResults)
    
    # See if analysis can be run
    ready <- options[["auditResult"]] != ""

    # Extract only the sample
    if(ready)
      sample <- subset(dataset, dataset[, .v(options[["sampleFilter"]])] != 0)

    # Import options and results from the planning and selection stages 
    evaluationOptions <- .auditInputOptions(options, dataset, jaspResults,
                                          stage = "planning", rawData = TRUE)

    planningContainer <- jaspResults[["planningContainer"]]
    planningState <- planningContainer[["planningState"]]$object

    selectionContainer <- jaspResults[["selectionContainer"]]
    selectionState <- selectionContainer[["selectionState"]]$object

    if(is.null(selectionState)) return()
      
    # Perform the evaluation
    evaluationState <- .auditEvaluationState(options, evaluationOptions, sample,
                                            evaluationContainer, selectionState)

    # Create explanatory text for the evaluation
    .auditExplanatoryTextEvaluation(options,
                                    evaluationOptions,
                                    planningState,
                                    selectionState,
                                    evaluationContainer, 
                                    positionInContainer = 1)
  } else if(!workflow){

    .auditCreateTableNumber(jaspResults) # Initialize table numbers
    .auditCreateFigureNumber(jaspResults) # Initialize figure numbers

    # Create an empty container for the evaluation analysis
    evaluationContainer <- .auditAnalysisContainer(jaspResults, stage = "evaluation",
                                                  position = 1)

    # Read in the relevant variables from the data set
    sample <- .auditReadDataset(options, jaspResults, stage = "evaluation")

	# Remove the critical transactions if wanted
	if(options[["flagCriticalTransactions"]] && options[["handleCriticalTransactions"]] == "remove")
		sample <- subset(sample, sample[, .v(options[["monetaryVariable"]])] >= 0)

      # Check for errors due to incompatible options
    error <- .auditErrorCheckInputOptions(options, sample, evaluationContainer, 
                                          stage = "evaluation")
    if(error) return()

    # Deduce relevant quantities from input options
    evaluationOptions <- .auditInputOptions(options, sample, jaspResults,
                                            stage = "evaluation")

    # Create the evaluation state that holds the results
    evaluationState <- .auditEvaluationAnalysisState(options, sample, evaluationOptions,
                                                    evaluationContainer)

    # Backwards create a planningstate and a selectionstate
    planningState <- .auditBackwardsPlanningState(options, sample, evaluationOptions)

    selectionState <- .auditBackwardsState(options, stage = "evaluation")

    # Create explanatory text for the evaluation
    .auditExplanatoryTextEvaluation(options, evaluationOptions, planningState,
                                    selectionState, evaluationContainer,
                                    positionInContainer = 1)

  }

  # --- TABLES

  # Create a table containing information about the evaluation process  
  .auditEvaluationSummaryTable(options, evaluationOptions, evaluationState,
                               evaluationContainer, jaspResults,
                               positionInContainer = 2)

  if(options[["bayesianAnalysis"]]){

    # Create a table containing assumption checks
    .auditEvaluationAssumptionChecks(options, sample, evaluationContainer,
                                     jaspResults, positionInContainer = 3)

    # Create a table containing information regarding the prior and posterior
    .auditPriorAndPosteriorStatisticsTable(options, evaluationOptions, evaluationState, 
                                           evaluationContainer, jaspResults, positionInContainer = 4)
  }

  # --- PLOTS

  if(options[["bayesianAnalysis"]]){
    # Create a plot containing the prior and posterior distribution
    .auditEvaluationPriorAndPosterior(options, evaluationOptions, planningState,
                                      evaluationState, evaluationContainer, jaspResults,
                                      positionInContainer = 5)
  }

  # Create a plot containing evaluation information
  .auditEvaluationInformationPlot(options, evaluationOptions, evaluationState,
                                  evaluationContainer, jaspResults,
                                  positionInContainer = 7)

  # Create a plot containing the correlation between the book and audit values
  if(options[["variableType"]] == "variableTypeAuditValues")
  .auditCorrelationPlot(options, evaluationOptions, sample, evaluationContainer,
                        jaspResults, positionInContainer = 9)
}

#####################################
######### CONCLUSION STAGE ##########
#####################################

.auditConclusionStage <- function(options, jaspResults){

  if(!is.null(jaspResults[["conclusionContainer"]]) || options[["auditResult"]] == "") 
    return()

  .auditExplanatoryText(options, stageOptions = NULL, stageContainer = NULL, stageState = NULL, 
                        jaspResults, stage = "conclusion", positionInContainer = 1)

}

################################################################################
################## Common functions for figure and table numbers ###############
################################################################################

.auditCreateFigureNumber <- function(jaspResults){
  # Initialize figure numbers
  jaspResults[["figNumber"]] <- createJaspState(0)
}

.auditCreateTableNumber <- function(jaspResults){
  # Initialize table numbers
  jaspResults[["tabNumber"]] <- createJaspState(0)
}

.updateTabNumber <- function(jaspResults){
  # Update table numbers + 1
  currentNumber <- jaspResults[["tabNumber"]]$object
  jaspResults[["tabNumber"]] <- createJaspState(currentNumber + 1)
}

.updateFigNumber <- function(jaspResults){
  # Update figure numbers + 1
  currentNumber <- jaspResults[["figNumber"]]$object
  jaspResults[["figNumber"]] <- createJaspState(currentNumber + 1)
}

################################################################################
################## Common functions for reading data and options ###############
################################################################################

.auditReadVariableFromOptions <- function(options, varType){
  if(varType == "recordNumber"){
    # Read in the transaction ID's
    recordNumberVariable <- options[["recordNumberVariable"]]
    if(recordNumberVariable == "")
      recordNumberVariable <- NULL
    return(recordNumberVariable)
  } else if(varType == "monetary"){
    # Read in the book values
    monetaryVariable <- options[["monetaryVariable"]]
    if(monetaryVariable == "")
      monetaryVariable <- NULL    
    return(monetaryVariable)
  } else if(varType == "auditResult"){
    # Read in the audit result
    auditResult <- options[["auditResult"]]
    if(auditResult == "")
      auditResult <- NULL
    return(auditResult)
  } else if(varType == "sampleCounter"){
    # Read in the sample counter
    sampleCounter <- options[["sampleCounter"]]
    if(sampleCounter == "")
      sampleCounter <- NULL
    return(sampleCounter)
  } else if(varType == "ranking"){
    # Read in the ranking variable
    rankingVariable <- options[["rankingVariable"]]
    if(rankingVariable == "")
      rankingVariable <- NULL
    return(rankingVariable)
  } else if(varType == "additional"){
    # Read in additional variables
    additionalVariables <- unlist(options[["additionalVariables"]])
    return(additionalVariables)
  } else if(varType == "critical"){
    criticalVariable <- options[["criticalTransactions"]]
    return(criticalVariable)
  }
}

.auditReadDataset <- function(options, jaspResults, stage){

  if(stage == "procedure"){

    recordNumberVariable  <- .auditReadVariableFromOptions(options, varType = "recordNumber")
    monetaryVariable      <- .auditReadVariableFromOptions(options, varType = "monetary")

    analysisOptions <- list()
  
    if(!is.null(recordNumberVariable)){
      dataset <- .readDataSetToEnd(columns.as.numeric = recordNumberVariable)
      analysisOptions[["populationSize"]] <- nrow(dataset)
      analysisOptions[["uniqueN"]] <- length(unique(dataset[, .v(options[["recordNumberVariable"]])]))

      if(!is.null(monetaryVariable)){
        variables <- c(recordNumberVariable, monetaryVariable)
        dataset <- .readDataSetToEnd(columns.as.numeric = variables)

        monetaryColumn <- dataset[, .v(monetaryVariable)]
        analysisOptions[["populationValue"]] <- sum(monetaryColumn, na.rm = TRUE)
        analysisOptions[["absPopulationValue"]] <- sum(abs(monetaryColumn), na.rm = TRUE)
        analysisOptions[["meanValue"]] <- mean(monetaryColumn, na.rm = TRUE)
        analysisOptions[["sigmaValue"]] <- sd(monetaryColumn, na.rm = TRUE)
        analysisOptions[["quantileValue"]] <- as.numeric(quantile(monetaryColumn, probs = c(0.25, 0.50, 0.75), na.rm = TRUE))
        analysisOptions[["ready"]] <- TRUE
      } else {
        analysisOptions[["populationValue"]] <- 0.01
        analysisOptions[["ready"]] <- ifelse(options[["materiality"]] == "materialityRelative",
                                              yes = TRUE, no = FALSE)
    }
  } else {
      dataset <- NULL
      analysisOptions[["populationSize"]] <- 0
      analysisOptions[["uniqueN"]] <- 0
      analysisOptions[["populationValue"]] <- 0.01
      analysisOptions[["ready"]] <- FALSE
  }

  materiality <- ifelse(options[["materiality"]] == "materialityRelative", 
                        yes = options[["materialityPercentage"]], 
                        no = options[["materialityValue"]])

  if(materiality == 0) analysisOptions[["ready"]] <- FALSE

  jaspResults[["procedureOptions"]] <- createJaspState(analysisOptions)
  jaspResults[["procedureOptions"]]$dependOn(c("recordNumberVariable", 
                                               "monetaryVariable", 
                                               "materiality",
                                               "materialityPercentage",
                                               "materialityValue"))
  return(dataset)

  } else if(stage == "selection"){
    recordNumberVariable  <- .auditReadVariableFromOptions(options, varType = "recordNumber")
    monetaryVariable      <- .auditReadVariableFromOptions(options, varType = "monetary")
    rankingVariable       <- .auditReadVariableFromOptions(options, varType = "ranking")
    additionalVariables   <- .auditReadVariableFromOptions(options, varType = "additional")
    variables             <- c(recordNumberVariable, monetaryVariable, rankingVariable, additionalVariables)
  } else if(stage == "evaluation"){
    recordNumberVariable  <- .auditReadVariableFromOptions(options, varType = "recordNumber")
    monetaryVariable      <- .auditReadVariableFromOptions(options, varType = "monetary")
    auditResult           <- .auditReadVariableFromOptions(options, varType = "auditResult")
    sampleCounter         <- .auditReadVariableFromOptions(options, varType = "sampleCounter")
    variables             <- c(recordNumberVariable, monetaryVariable, auditResult, sampleCounter)
  }

  if(!is.null(variables)){
    dataset <- .readDataSetToEnd(columns.as.numeric = variables)
    if(stage == "evaluation" && !is.null(sampleCounter)) # Apply sample filter
      dataset <- subset(dataset, dataset[, .v(options[["sampleCounter"]])] > 0)
    return(dataset)
  } else {
    return(NULL)
  }
}

.auditInputOptions <- function(options, dataset, jaspResults, stage, rawData = FALSE){

  inputOptions <- list()

  if(stage == "planning"){

    inputOptions[["valuta"]] <- base::switch(options[["valuta"]], 
                                             "euroValuta" = "\u20AC", 
                                             "dollarValuta" = "\u0024", 
                                             "otherValuta" = options[["otherValutaName"]])
  
    inputOptions[["confidence"]] <- options[["confidence"]]
    inputOptions[["confidenceLabel"]] <- paste0(round(options[["confidence"]] * 100, 2), "%")

    if(!rawData){

      inputOptions[["populationSize"]] <- options[["populationSize"]]
      inputOptions[["populationValue"]] <- ifelse(options[["populationValue"]] == 0, 
                                                  yes = 0.01, 
                                                  no = options[["populationValue"]])

    } else {
      
      procedureOptions <- jaspResults[["procedureOptions"]]$object
      inputOptions[["populationSize"]] <- procedureOptions[["populationSize"]]
      inputOptions[["populationValue"]] <- procedureOptions[["populationValue"]]

    }

  inputOptions[["absRel"]] <- ifelse(options[["materiality"]] == "materialityRelative", 
                                     yes = gettext("<b>percentage</b>"), 
                                     no = gettext("<b>amount</b>"))

  inputOptions[["materiality"]] <- ifelse(options[["materiality"]] == "materialityRelative",
                                          yes = options[["materialityPercentage"]], 
                                          no = options[["materialityValue"]] / inputOptions[["populationValue"]])

  inputOptions[["materialityLabel"]] <- ifelse(options[["materiality"]] == "materialityRelative",
                                               yes = paste0(round(inputOptions[["materiality"]] * 100, 2), "%"), 
                                               no = paste(inputOptions[["valuta"]], format(options[["materialityValue"]], 
                                                                  scientific = FALSE)))

  inputOptions[["expectedErrors"]] <- ifelse(options[["expectedErrors"]] == "expectedRelative",
                                              yes = options[["expectedPercentage"]],
                                              no = options[["expectedNumber"]])
  if(options[["expectedErrors"]] == "expectedAbsolute" && options[["materiality"]] == "materialityAbsolute")
	  inputOptions[["expectedErrors"]] <- inputOptions[["expectedErrors"]] / inputOptions[["populationValue"]]

  inputOptions[["expectedErrorsLabel"]] <- ifelse(options[["expectedErrors"]] == "expectedRelative",
                                                  yes = paste0(round(inputOptions[["expectedErrors"]] * 100, 2), "%"),
                                                  no = options[["expectedNumber"]])
  if(options[["materiality"]] == "materialityAbsolute" && options[["expectedErrors"]] == "expectedAbsolute")
    inputOptions[["expectedErrorsLabel"]] <- paste(inputOptions[["valuta"]], inputOptions[["expectedErrorsLabel"]])

  inputOptions[["likelihood"]] <- base::switch(options[["planningModel"]], 
                                               "Poisson" = "poisson", 
                                               "binomial" = "binomial", 
                                               "hypergeometric" = "hypergeometric")

  inputOptions[["minimumPrecision"]] <- options[["minimumPrecisionPercentage"]]
  inputOptions[["minimumPrecisionLabel"]] <- paste0(round(inputOptions[["minimumPrecision"]] * 100, 4), "%")

  } else if(stage == "selection"){

    inputOptions[["valuta"]] <- base::switch(options[["valuta"]], 
                                             "euroValuta" = "\u20AC", 
                                             "dollarValuta" = "\u0024", 
                                             "otherValuta" = options[["otherValutaName"]])
    inputOptions[["populationSize"]] <- ifelse(is.null(dataset),
                                               yes = 0,
                                               no = nrow(dataset))
    if(options[["monetaryVariable"]] != "")
      inputOptions[["populationValue"]] <- sum(dataset[, .v(options[["monetaryVariable"]])])

  } else if(stage == "evaluation"){

    confidence <- options[["confidence"]]
    confidenceLabel <- paste0(round(options[["confidence"]] * 100, 2), "%")
    populationSize <- options[["populationSize"]]
    populationValue <- ifelse(options[["populationValue"]] == 0, 
                              yes = 0.01, 
                              no = options[["populationValue"]])
    materiality <- ifelse(options[["materiality"]] == "materialityRelative",
                              yes = options[["materialityPercentage"]], 
                              no = options[["materialityValue"]] / 
                                    populationValue)
    materialityLabel <- ifelse(options[["materiality"]] == "materialityRelative",
                              yes = paste0(round(materiality * 100, 2), "%"), 
                              no = paste("$", format(options[["materialityValue"]], 
                                                scientific = FALSE)))
    expectedErrors <- ifelse(options[["expectedErrors"]] == "expectedRelative", 
                            yes = options[["expectedPercentage"]], 
                            no = options[["expectedNumber"]] / 
                                  populationValue)
    likelihood <- base::switch(options[["estimator"]],
                                "betaBound" = "binomial",
                                "gammaBound" = "poisson",
                                "betabinomialBound" = "hypergeometric")

    inputOptions[["materiality"]] <- materiality
    inputOptions[["materialityLabel"]] <- materialityLabel
    inputOptions[["populationSize"]] <- populationSize
    inputOptions[["populationValue"]] <- populationValue
    inputOptions[["valuta"]] <- "$"
    inputOptions[["confidence"]] <- confidence
    inputOptions[["confidenceLabel"]] <- confidenceLabel
    inputOptions[["expectedErrors"]] <- expectedErrors
    inputOptions[["likelihood"]] <- likelihood
    inputOptions[["minimumPrecision"]] <- options[["minimumPrecisionPercentage"]]

  }

  return(inputOptions)

}

.auditBackwardsState <- function(options, stage){
  if(stage == "selection"){
    state <- list("sampleSize" = options[["sampleSize"]])
  } else if(stage == "evaluation"){
    state <- data.frame(count = 1)
  }
  return(state)
}

################################################################################
################## Common functions for containers #############################
################################################################################

.auditAnalysisContainer <- function(jaspResults, stage, position = 1){

  if(stage == "procedure"){

    if(!is.null(jaspResults[["procedureContainer"]]))
      return(jaspResults[["procedureContainer"]])

    analysisContainer <- createJaspContainer(title = gettext("<u>Procedure</u>"))
    analysisContainer$position <- position
    analysisContainer$dependOn(options = c("explanatoryText", 
                                           "confidence", 
                                           "materiality", 
                                           "materialityValue", 
                                           "materialityPercentage", 
                                           "valuta",
                                           "otherValutaName",
                                           "monetaryVariable",
                                           "recordNumberVariable"))

    jaspResults[["procedureContainer"]] <- analysisContainer

  } else if(stage == "planning"){

    if(!is.null(jaspResults[["planningContainer"]]))
      return(jaspResults[["planningContainer"]])

    analysisContainer <- createJaspContainer(title = gettext("<u>Planning</u>"))
    analysisContainer$position <- position
    analysisContainer$dependOn(options = c("IR", 
                                           "irCustom",
                                           "CR",  
                                           "crCustom", 
                                           "confidence",
                                           "populationSize", 
                                           "populationValue",
                                           "materiality",
                                           "materialityPercentage", 
                                           "materialityValue",
                                           "expectedPercentage",
                                           "expectedErrors", 
                                           "expectedNumber",
                                           "planningModel",
                                           "recordNumberVariable",
                                           "monetaryVariable",
                                           "valuta",
                                           "otherValutaName",
                                           "separateKnownAndUnknownMisstatement",
                                           "minimumPrecisionPercentage",
                                           "minimumPrecision",
                                           "performanceMateriality",
                                           "sampleSizeIncrease",
                                           "priorConstructionMethod",
                                           "sampleN",
                                           "sampleK",
                                           "factor",
                                           "pHmin",
                                           "pHplus"))

    jaspResults[["planningContainer"]] <- analysisContainer

  } else if(stage == "selection"){

    if(!is.null(jaspResults[["selectionContainer"]]))
      return(jaspResults[["selectionContainer"]])

    analysisContainer <- createJaspContainer(title = "")
    analysisContainer$position <- position
    analysisContainer$dependOn(options = c("recordNumberVariable",
                                           "monetaryVariable",
                                           "additionalVariables",
                                           "rankingVariable",
                                           "selectionMethod",
                                           "selectionType",
                                           "seed",
                                           "intervalStartingPoint",
                                           "sampleSize",
										   "valuta",
										   "otherValutaName"))

    jaspResults[["selectionContainer"]] <- analysisContainer

  } else if(stage == "selection-workflow"){

    planningContainer <- jaspResults[["planningContainer"]]
    planningState <- planningContainer[["planningState"]]$object

    if(!is.null(jaspResults[["selectionContainer"]])){

      return(jaspResults[["selectionContainer"]])

    } else if(!is.null(planningState)){
                                          
      analysisContainer <- createJaspContainer(title = gettext("<u>Selection</u>"))
      analysisContainer$position <- position
      analysisContainer$dependOn(optionsFromObject = planningContainer,
                                  options = c("samplingChecked",
                                              "selectionMethod",
                                              "selectionType",
                                              "seed",
                                              "intervalStartingPoint",
                                              "additionalVariables",
                                              "rankingVariable",
                                              "valuta",
                                              "otherValutaName",
                                              "separateKnownAndUnknownMisstatement"))

      jaspResults[["selectionContainer"]] <- analysisContainer
    }
    
  } else if(stage == "evaluation"){

    if(!is.null(jaspResults[["evaluationContainer"]]))
      return(jaspResults[["evaluationContainer"]])

    analysisContainer <- createJaspContainer(title = "")
    analysisContainer$position <- position
    analysisContainer$dependOn(options = c("recordNumberVariable",
                                           "monetaryVariable",
                                           "auditResult",
                                           "sampleCounter",
                                           "variableType",
                                           "confidence",
                                           "populationSize",
                                           "populationValue",
                                           "IR",
                                           "irCustom",
                                           "CR",
                                           "crCustom",
                                           "expectedErrors",
                                           "expectedPercentage",
                                           "expectedNumber",
                                           "materiality",
                                           "materialityPercentage",
                                           "materialityValue",
                                           "useSumStats",
                                           "nSumStats",
                                           "kSumStats",
                                           "estimator",
                                           "estimator2",
                                           "areaUnderPosterior",
                                           "stringerBoundLtaAdjustment",
                                           "display",
                                           "priorType",
                                           "separateKnownAndUnknownMisstatement",
                                           "minimumPrecisionPercentage",
                                           "minimumPrecision",
                                           "performanceMateriality",
                                           "sampleSizeIncrease",
                                           "priorConstructionMethod",
                                           "sampleN",
                                           "sampleK",
                                           "factor",
                                           "pHmin",
                                           "pHplus",
										   "flagCriticalTransactions",
										   "criticalTransactions",
										   "flagNegativeValues",
										   "handleCriticalTransactions"))

    jaspResults[["evaluationContainer"]] <- analysisContainer

  } else if(stage == "evaluation-workflow"){
      
    selectionContainer <- jaspResults[["selectionContainer"]]
    selectionState <- selectionContainer[["selectionState"]]$object

    if(!is.null(jaspResults[["evaluationContainer"]])){
      return(jaspResults[["evaluationContainer"]])
    } else if(!is.null(selectionState)){                              
      analysisContainer <- createJaspContainer(title = gettext("<u>Evaluation</u>"))
      analysisContainer$position <- position
      analysisContainer$dependOn(options = c("evaluationChecked",
                                              "auditResult",
                                              "mostLikelyError",
                                              "estimator",
                                              "performAudit",
                                              "stringerBoundLtaAdjustment",
                                              "areaUnderPosterior",
                                              "display"))

      jaspResults[["evaluationContainer"]] <- analysisContainer

    }
  }

  return(analysisContainer)
}

################################################################################
################## Common functions for error checks ###########################
################################################################################

.auditErrorCheckInputOptions <- function(options, 
                                         dataset, 
                                         analysisContainer, 
                                         stage, 
                                         ready = NULL, 
                                         analysisOptions = NULL){

  if(stage == "procedure"){

      variables <- NULL
      if(options[["recordNumberVariable"]] != "")
        variables <- c(variables, options[["recordNumberVariable"]])
      if(options[["monetaryVariable"]] != "")
        variables <- c(variables, options[["monetaryVariable"]])
      if (length(variables) == 0) return()
      N <- nrow(dataset)
      # Check for infinity, zero variance, and any missing observations
      .hasErrors(dataset, type = c("infinity", "variance", "observations"),
                  all.target = variables, message = "short", 
                  observations.amount = paste0("< ", N),
                  exitAnalysisIfErrors = TRUE)

  } else if(stage == "planning"){
      if(ready){
        if(options[["materiality"]] == "materialityAbsolute" && options[["materialityValue"]] >= analysisOptions[["populationValue"]]){
          # Error if the value of the performance materiality exceeds the total population value
          analysisContainer$setError(gettext("Analysis not possible: Your materiality is higher than, or equal to the total value of the observations."))
          return(TRUE)
        }
        expTMP <- ifelse(options[['expectedErrors']] == "expectedRelative", 
                          yes = options[["expectedPercentage"]], 
                          no = options[["expectedNumber"]])
        if(options[["materiality"]] == "materialityAbsolute")
          expTMP <- expTMP / analysisOptions[["populationValue"]]
        if(expTMP >= analysisOptions[["materiality"]] && expTMP < 1 && !options[["minimumPrecision"]]){
          # Error if the expected errors exceed the performance materiality
          analysisContainer$setError(gettext("Analysis not possible: Your expected errors are higher than materiality."))
          return(TRUE)
        }
        if(.auditCalculateDetectionRisk(options) >= 1){
          # Error if the detection risk of the analysis is higher than one
          analysisContainer$setError(gettext("The detection risk is equal to or higher than 100%. Please re-specify your custom values for the Inherent risk and/or Control risk, or the confidence."))
          return(TRUE)
        }
        if(options[["bayesianAnalysis"]] && !options[["performanceMateriality"]] && !options[["priorConstructionMethod"]] %in% c("none", "sample", "factor")){
          # Error if the prior construction method does not match the sampling objective
          analysisContainer$setError(gettext("You cannot incorporate this prior information into your analysis because you are not testing against a performance materiality."))
          return(TRUE)          
        }
        if(options[["priorConstructionMethod"]] %in% c("median", "hypotheses") && options[["planningModel"]] == "hypergeometric"){
          # Error if equal prior probabilities are chosen with a beta-binomial distribution
          analysisContainer$setError(gettext("The prior distribution with equal prior probabilities cannot be constructed using the beta-binomial distribution. You can use the beta or the gamma distribution in this case."))
          return(TRUE)          
        }
        if(options[["priorConstructionMethod"]] %in% c("median", "hypotheses") && expTMP != 0 && options[["planningModel"]] == "Poisson"){
          # Error if equal prior probabilities are chosen in combination with a gamma prior and expected errors != 0
          analysisContainer$setError(gettext("The gamma prior distribution with equal prior probabilities cannot be constructed when you expect errors in the sample. You can revert to the beta distribution in this case."))
          return(TRUE)          
        }
      }
      # No error in the planning options
      return(FALSE)

  } else if(stage == "selection"){

    if(!is.null(dataset) && options[["sampleSize"]] >= nrow(dataset) && options[["selectionType"]] == "recordSampling"){
      # Error if the sample size is larger than the population size in case of record sampling.
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      analysisContainer$setError(gettext("Your sample size is larger than (or equal to) your population size. Cannot take a record sample larger than the population."))
      return(TRUE)
    } else if(!is.null(dataset) && options[["sampleSize"]] >= analysisOptions[["populationValue"]] && options[["selectionType"]] == "musSampling"){
      # Error if the sample size is larger than the population value in case of mus sampling.
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      analysisContainer$setError(gettext("Your sample size is larger than (or equal to) your population value. Cannot take a MUS sample larger than the population value."))
      return(TRUE)
    } else if(!is.null(dataset) && options[["sampleSize"]] == 1){
      # Error if the sample size is 1.
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      analysisContainer$setError(gettext("Your sample size must be larger than 1."))
      return(TRUE)
    } else if(options[["recordNumberVariable"]] != "" && !is.null(dataset) && nrow(dataset) != length(unique(dataset[, .v(options[["recordNumberVariable"]])]))){
      # Error if the transaction ID's are not unique
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      analysisContainer$setError(gettext("Your must specify unique transaction ID's. The row numbers of the data set are sufficient."))
      return(TRUE)
    } else {
      # No error in the selection options
      return(FALSE)
    }

  } else if(stage == "selection-workflow") {

    if(options[["recordNumberVariable"]] != "" && !is.null(dataset) && nrow(dataset) != length(unique(dataset[, .v(options[["recordNumberVariable"]])]))){
      # Error if the transaction ID's are not unique
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      analysisContainer$setError(gettext("Your must specify unique transaction ID's. The row numbers of the data set are sufficient."))
      return(TRUE)
    } else {
      # No error in the selection options
      return(FALSE)
    }
    
  } else if(stage == "evaluation"){

    if(options[["variableType"]] == "variableTypeCorrect" && 
        !options[["useSumStats"]] && 
        options[["auditResult"]] != "" &&
        !all(unique(dataset[, .v(options[["auditResult"]])]) %in% c(0, 1))){
      # Error if the audit result does not contain only zero's and one's.
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Evaluation summary"))
      analysisContainer$setError(gettext("Your audit result does not contain only 0's (correct) and 1's (incorrect). Select the Soll values annotation method from the options or use the summary statistics to evaluate your sample."))
      return(TRUE)
    } else if(!options[["bayesianAnalysis"]] && 
              options[["variableType"]] == "variableTypeCorrect" && 
              options[["estimator2"]] == "hyperBound" && 
              options[["populationSize"]] == 0){
      # Error if the population size is not defined when the hypergeometric bound is used.
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Evaluation summary"))
      analysisContainer$setError(gettext("The hypergeometric upper bound requires that you specify the population size."))
      return(TRUE)
    } else if((!options[["useSumStats"]] && !is.null(dataset) && options[["populationSize"]] < nrow(dataset)) || 
              (options[["useSumStats"]] && options[["populationSize"]] < options[["nSumStats"]])){
      # Error if the sample size is larger than the population size.
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Evaluation summary"))
      analysisContainer$setError(gettext("Your sample size is larger than (or equal to) your population size. Please adjust your population size accordingly."))
      return(TRUE)
    } else if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound") && 
              (options[["populationValue"]] == 0 || options[["populationSize"]] == 0)){
      # Error if the population size or the population value are zero when using direct, difference, ratio, or regression.
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Evaluation summary"))
      analysisContainer$setError(gettext("The direct, difference, ratio, and regression bounds require that you specify the population size and the population value."))
      return(TRUE)
    } else if(!options[["useSumStats"]] && options[["recordNumberVariable"]] != "" && !is.null(dataset) && nrow(dataset) != length(unique(dataset[, .v(options[["recordNumberVariable"]])]))){
      # Error if the transaction ID's are not unique
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      analysisContainer$setError(gettext("Your must specify unique transaction ID's. The row numbers of the data set are sufficient."))
      return(TRUE)
    } else if(.auditCalculateDetectionRisk(options) >= 1){
      # Error if the detection risk of the analysis is higher than one
      analysisContainer[["errorMessage"]] <- createJaspTable(gettext("Evaluation summary"))
      analysisContainer$setError(gettext("The detection risk is equal to or higher than 100%. Please re-specify your values for the Inherent risk and/or Control risk, or the confidence."))
      return(TRUE)
    } else {
      # No error in the evaluation options
      return(FALSE)
    }
  }
}

.auditReadyForNextStage <- function(options, jaspResults, stage){
  if(stage == "procedure"){
    # Check whether any of the two sampling objectives is selected
    ready <- options[["performanceMateriality"]] || options[["minimumPrecision"]]
  } else if(stage == "planning"){
    # Check whether the "To selection" button is pressed and no error occurred in the previous stage
    ready <- options[["samplingChecked"]] && !jaspResults[["planningContainer"]]$getError() && (options[["performanceMateriality"]] || options[["minimumPrecision"]])
  } else if(stage == "selection"){
    
  } else if(stage == "execution"){
    # Check whether the "To evaluation" button is pressed and no error occurred in the previous stage
    ready <- options[["evaluationChecked"]] && !jaspResults[["planningContainer"]]$getError() && !jaspResults[["selectionContainer"]]$getError()
  } else if(stage == "evaluation"){

  }
  return(ready)
}

.auditReadyForAnalysis <- function(options, planningOptions, stage){

  if(stage == "planning"){
    if(!(options[["performanceMateriality"]] || options[["minimumPrecision"]])){
      ready <- FALSE
    }
    if(options[["performanceMateriality"]] && !options[["minimumPrecision"]] && options[["materiality"]] == "materialityAbsolute"){
      ready <- options[["materialityValue"]] != 0 && planningOptions[["populationSize"]] != 0 && 
                planningOptions[["populationValue"]] != 0 && planningOptions[["populationValue"]] != 0.01 
    }
    if(options[["performanceMateriality"]] && !options[["minimumPrecision"]] && options[["materiality"]] == "materialityRelative"){
      ready <- options[["materialityPercentage"]] != 0 && planningOptions[["populationSize"]] != 0
    }
    if(options[["minimumPrecision"]] && !options[["performanceMateriality"]]){
      ready <- options[["minimumPrecisionPercentage"]] != 0 && planningOptions[["populationSize"]] != 0 && 
                planningOptions[["populationValue"]] != 0
    }
    if(options[["minimumPrecision"]] && options[["performanceMateriality"]]){
      if(options[["materiality"]] == "materialityAbsolute"){
        ready <- options[["materialityValue"]] != 0 && planningOptions[["populationSize"]] != 0 && 
                planningOptions[["populationValue"]] != 0 && planningOptions[["populationValue"]] != 0.01 &&
                options[["minimumPrecisionPercentage"]] != 0
      } else {
        ready <- options[["materialityPercentage"]] != 0 && planningOptions[["populationSize"]] != 0 && options[["minimumPrecisionPercentage"]] != 0
      }
    }
  }

  return(ready)

}

################################################################################
################## Common functions for the explanatory text ###################
################################################################################

.auditExplanatoryText <- function(options, stageOptions, stageContainer, stageState, 
                                  jaspResults, stage, positionInContainer, prevState = NULL){

  if(options[["explanatoryText"]]){

    if(stage == "procedure"){

      procedureContainer <- .auditAnalysisContainer(jaspResults, stage = "procedure",
                                                    position = 1)

      if(!options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
        procedureText <- gettextf("Select one or more sampling objectives from the top left corner to begin planning an audit sample.\n\n%1$s <b>Test against a performance materiality</b>\n\nEnable this objective if you want to <i>test</i> whether the total misstatement in the population exceeds a certain limit (i.e., the performance materiality) based on a sample. This approach allows you to plan a sample such that, when the sample meets your expectations, the maximum error is said to be below performance materiality. In the evaluation you will be able to quantify the evidence that your sample contains for or against the statement that the population misstatement does not exceed the performance materiality.\n\n%2$s <b>Obtain a required minimum precision</b>\n\nEnable this objective if you want to obtain a required minimum precision when <i>estimating</i> the total misstatement in the population based on a sample. This approach allows you to plan a sample such that, when the sample meets expectations, the accuracy of your estimate is below a tolerable percentage. In the evaluation you will be able to quantify the accuracy of your estimate of the population misstatement.", "\u25CF", "\u25CF")
      } else if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
        procedureText <- gettextf("The objective of this audit sampling procedure is to determine with a specified confidence <b>(%1$s)</b> whether the %2$s of misstatement in the population is lower than the specified performance materiality of <b>%3$s</b>.",
                                  stageOptions[["confidenceLabel"]],
                                  stageOptions[["absRel"]],
                                  stageOptions[["materialityLabel"]])
      } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
        procedureText <- gettextf("The objective of this audit sampling procedure is to determine the misstatement in the population with a specified confidence <b>(%1$s)</b> and a required minimum precision of <b>%2$s</b>.",
                                  stageOptions[["confidenceLabel"]],
                                  stageOptions[["minimumPrecisionLabel"]])
      } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
        procedureText <- gettextf("The objective of this audit sampling procedure is to determine with a specified confidence <b>(%1$s)</b> whether the %2$s of misstatement in the population is lower than the specified performance materiality of <b>%3$s</b> with a required minimum precision of <b>%4$s</b>.",
                                  stageOptions[["confidenceLabel"]],
                                  stageOptions[["absRel"]],
                                  stageOptions[["materialityLabel"]],
                                  stageOptions[["minimumPrecisionLabel"]])
      }                                           

      procedureContainer[["procedureParagraph"]] <- createJaspHtml(procedureText, "p")
      procedureContainer[["procedureParagraph"]]$position <- positionInContainer

    } else if(stage == "planning") {

      if(is.null(stageContainer[["planningParagraph"]]) && !stageContainer$getError()){

        if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
          samplingObjectivesMessage <- gettextf("a performance materiality of <b>%1$s</b>", stageOptions[["materialityLabel"]])
          samplingObjectivesMessage2 <- gettext("the maximum misstatement is determined to be below the performance materiality")
        } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
          samplingObjectivesMessage <- gettextf("a required minimum precision of <b>%1$s</b>", stageOptions[["minimumPrecisionLabel"]])
          samplingObjectivesMessage2 <- gettext("the misstatement is determined with the required minimum precision")
        } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
          samplingObjectivesMessage <- gettextf("a performance materiality of <b>%1$s</b> and a required minimum precision of <b>%2$s</b>", stageOptions[["materialityLabel"]], stageOptions[["minimumPrecisionLabel"]])
          samplingObjectivesMessage2 <- gettext("the maximum misstatement is determined with the required minimum precision to be below the performance materiality")
        }
        
        separateMisstatementMessage <- ifelse(options[["separateKnownAndUnknownMisstatement"]],
                                              yes = gettext("\n\nFurthermore, the uncertainty regarding the misstatement will only be extrapolated over the unseen part of the population according to Ref (2020). This requires the additional <i>post-hoc</i> assumption that the sample taints are homogeneous."),
                                              no = "")
        
        distribution <- options[["planningModel"]]
        if(options[["bayesianAnalysis"]])
          distribution <- base::switch(options[["planningModel"]], "Poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")

        if(options[["priorConstructionMethod"]] == "none"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, the <i>a priori</i> assumption that every value of the misstatement is equally likely, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%6$s</b>, %7$s. %8$s",
                                                                  stageOptions[["expectedErrorsLabel"]], 
                                                                  samplingObjectivesMessage,
                                                                  stageState[["expectedSampleError"]],
                                                                  stageState[["sampleSize"]],
                                                                  distribution,
                                                                  stageState[["expectedSampleError"]],
                                                                  samplingObjectivesMessage2,
                                                                  separateMisstatementMessage), "p")
        } else if(options[["priorConstructionMethod"]] == "arm"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, the <i>a priori</i> assessments of inherent risk <b>(%6$s)</b> and control risk <b>(%7$s)</b> from the Audit Risk Model, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%8$s</b>, %9$s. %10$s",
                                                                  stageOptions[["expectedErrorsLabel"]],
                                                                  samplingObjectivesMessage,
                                                                  stageState[["expectedSampleError"]],
                                                                  stageState[["sampleSize"]],
                                                                  distribution,
                                                                  options[["IR"]],
                                                                  options[["CR"]],
                                                                  stageState[["expectedSampleError"]],
                                                                  samplingObjectivesMessage2,
                                                                  separateMisstatementMessage), "p")
        } else if (options[["priorConstructionMethod"]] == "median"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, the <i>a priori</i> assumption that tolerable misstatement is equally likely to occur as intolerable misstatement, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%6$s</b>, %7$s. %8$s",
                                                                  stageOptions[["expectedErrorsLabel"]],
                                                                  samplingObjectivesMessage,
                                                                  stageState[["expectedSampleError"]],
                                                                  stageState[["sampleSize"]],
                                                                  distribution,
                                                                  stageState[["expectedSampleError"]],
                                                                  samplingObjectivesMessage2,
                                                                  separateMisstatementMessage), "p")
        } else if (options[["priorConstructionMethod"]] == "hypotheses"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, the <i>a priori</i> assumption that tolerable misstatement occurs with a probability <b>%6$s</b> of and intolerable misstatement occurs with a probability of <b>%7$s</b>, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%8$s</b>, %9$s. %10$s",
                                                                  stageOptions[["expectedErrorsLabel"]],
                                                                  samplingObjectivesMessage,
                                                                  stageState[["expectedSampleError"]],
                                                                  stageState[["sampleSize"]],
                                                                  distribution,
                                                                  options[["pHmin"]],
                                                                  1 - options[["pHmin"]],
                                                                  stageState[["expectedSampleError"]],
                                                                  samplingObjectivesMessage2,
                                                                  separateMisstatementMessage), "p")          
        } else if (options[["priorConstructionMethod"]] == "sample"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, the <i>a priori</i> assumption that an earlier sample of <b>%6$s</b> transactions containing <b>%7$s</b> errors is seen, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%8$s</b>, %9$s. %10$s",
                                                                  stageOptions[["expectedErrorsLabel"]],
                                                                  samplingObjectivesMessage,
                                                                  stageState[["expectedSampleError"]],
                                                                  stageState[["sampleSize"]],
                                                                  distribution,
                                                                  options[["sampleN"]],
                                                                  options[["sampleK"]],
                                                                  stageState[["expectedSampleError"]],
                                                                  samplingObjectivesMessage2,
                                                                  separateMisstatementMessage), "p")            
        } else if (options[["priorConstructionMethod"]] == "factor"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, an <i>a priori</i> assumption that an earlier sample of <b>%6$s</b> transactions containing <b>%7$s</b> errors weighted by a factor <b>%8$s</b> is seen, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%9$s</b>, %10$s. %11$s",
                                                                  stageOptions[["expectedErrorsLabel"]],
                                                                  samplingObjectivesMessage,
                                                                  stageState[["expectedSampleError"]],
                                                                  stageState[["sampleSize"]],
                                                                  distribution,
                                                                  options[["sampleN"]],
                                                                  options[["sampleK"]],
                                                                  options[["factor"]],
                                                                  stageState[["expectedSampleError"]],
                                                                  samplingObjectivesMessage2,
                                                                  separateMisstatementMessage), "p")           
        }

        stageContainer[["planningParagraph"]]$position <- positionInContainer
        stageContainer[["planningParagraph"]]$dependOn(options = "explanatoryText")
      }

    } else if(stage == "selection"){

      samplingLabel <- base::switch(options[["selectionMethod"]], 
                                    "randomSampling" = gettext("random"), 
                                    "systematicSampling" = gettext("fixed interval"), 
                                    "cellSampling" = gettext("cell"))

		if(options[["selectionType"]] == "musSampling"){
			samplingVariableText <- gettext("Ist position (<i>...</i>)")
			samplingUnitText <- base::switch(options[["valuta"]],
											"euroValuta" = "Euros",
											"dollarValuta" = "Dollars",
											"otherValuta" = options[["otherValutaName"]])
			if(options[["monetaryVariable"]] != "")
				samplingVariableText <- gettextf("Ist position (<i>%1$s</i>)", options[["monetaryVariable"]])
		} else if(options[["selectionType"]] == "recordSampling"){
			samplingUnitText <- "records"
			samplingVariableText <- gettext("record variable (<i>...</i>)")
			if(options[["recordNumberVariable"]] != "")
				samplingVariableText <- gettextf("record variable (<i>%1$s</i>)", options[["recordNumberVariable"]])
		}
	
	  if(!is.null(stageState) && !is.null(stageState[["musFailed"]])){
        # MUS has failed for some reason, fall back to record sampling

        message <- gettextf("From the population of <b>%1$s</b> transactions, <b>%2$s</b> sampling units (<i>%3$s</i>) are selected from the %4$s using a <b>%5$s record sampling</b> method. <br><b>Warning:</b> A monetary unit sampling method was tried but failed.",
                            stageOptions[["populationSize"]],
                            prevState[["sampleSize"]],
							samplingUnitText,
							samplingVariableText,
                            samplingLabel)

      } else {

        samplingLabel <- base::switch(options[["selectionType"]], 
                                      "recordSampling" = gettextf("%1$s record sampling", samplingLabel), 
                                      "musSampling" = gettextf("%1$s monetary unit sampling", samplingLabel))

        message <- gettextf("From the population of <b>%1$s</b> transactions, <b>%2$s</b> sampling units (<i>%3$s</i>) are selected from the %4$s using a <b>%5$s</b> method.",
                            stageOptions[["populationSize"]],
							prevState[["sampleSize"]],
							samplingUnitText,
							samplingVariableText,
                            samplingLabel)

      }

      if(!is.null(stageState) && sum(stageState[["count"]]) > nrow(stageState)){

        message <- gettextf("%1$s \n\n<b>Note:</b> The selected sample (%2$s) contains fewer transactions than the planned sample size (%3$s) because some transactions are selected more than once. The transactions containing these %4$s extra selected sampling units will be counted multiple times in the evaluation.",
                            message,
                            nrow(stageState),
                            prevState[["sampleSize"]],
                            prevState[["sampleSize"]] - nrow(stageState))
        
      }


      stageContainer[["samplingParagraph"]] <- createJaspHtml(message, "p")
      stageContainer[["samplingParagraph"]]$position <- positionInContainer
      stageContainer[["samplingParagraph"]]$dependOn(options = "explanatoryText")
        
    } else if(stage == "conclusion"){

      # Import options and results from the planning and selection stages 
      planningOptions <- .auditInputOptions(options, dataset = NULL, jaspResults,
                                            stage = "planning", rawData = TRUE)

      # Import result of analysis from jaspResults
      evaluationContainer   <- jaspResults[["evaluationContainer"]]
      evaluationState       <- evaluationContainer[["evaluationState"]]$object
      
      if(is.null(evaluationState)) return()

      # Create a container for the conclusion
      conclusionContainer <- createJaspContainer(title = gettext("<u>Conclusion</u>"))
      conclusionContainer$position <- 5
      conclusionContainer$dependOn(optionsFromObject = evaluationContainer)
      conclusionContainer$dependOn(options = "explanatoryText")

      # Produce relevant terms conditional on the analysis result
      approveMateriality <- TRUE
      if(options[["performanceMateriality"]]){
        if(evaluationState[["confBound"]] < planningOptions[["materiality"]]){
          aboveBelowMateriality <- gettext("below")
          lowerHigherMateriality <- gettext("lower")
          approveMateriality <- TRUE
        } else {
          aboveBelowMateriality <- gettext("above")
          lowerHigherMateriality <- gettext("higher")
          approveMateriality <- FALSE
        }
      } 

      approvePrecision <- TRUE
      if(options[["minimumPrecision"]]){
        if(evaluationState[["precision"]] <= planningOptions[["minimumPrecision"]]){
          lowerHigherPrecision <- gettext("lower")
          approvePrecision <- TRUE
        } else {
          lowerHigherPrecision <- gettext("higher")
          approvePrecision <- FALSE
        }
      }

      additionalMessage <- ifelse(approveMateriality && approvePrecision,
                                  yes = gettext("\n\n<b>Objectives:</b> You have achieved your specified sampling objectives."),
                                  no = gettext("\n\n<b>Objectives:</b> You have not achieved your specified sampling objectives. It is recommended to draw more samples from this population and to continue audit procedures."))

      if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
        message <- gettextf("The objective of this audit sampling procedure was to determine with %1$s confidence whether the misstatement in the population is lower than the specified performance materiality, in this case %2$s. For the current data, the %3$s upper bound on the misstatement is <b>%4$s</b> the performance materiality. \n\nThe conclusion on the basis of these results is that, with at least %5$s confidence, the misstatement in the population is <b>%6$s</b> than the performance materiality. %7$s",
                            planningOptions[["confidenceLabel"]],
                            planningOptions[["materialityLabel"]],
                            planningOptions[["confidenceLabel"]],
                            aboveBelowMateriality,
                            planningOptions[["confidenceLabel"]],
                            lowerHigherMateriality,
                            additionalMessage)
      } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
        message <- gettextf("The objective of this audit sampling procedure was to determine the misstatement in the population with %1$s confidence and a minimum precision of %2$s. For the current data, the obtained precision is <b>%3$s</b> than the required minimum precision. \n\nThe conclusion on the basis of these results is that the total misstatement in the population has been determined with at least %4$s confidence and a precision of %5$s. %6$s",
                            planningOptions[["confidenceLabel"]],
                            paste0(options[["minimumPrecisionPercentage"]] * 100, "%"),
                            lowerHigherPrecision,
                            planningOptions[["confidenceLabel"]],
                            paste0(round(evaluationState[["precision"]] * 100, 3), "%"),
                            additionalMessage)
      } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
        message <- gettextf("The objective of this audit sampling procedure was to determine with %1$s confidence, and a minimum precision of %2$s, whether the misstatement in the population is lower than the specified performance materiality, in this case %3$s. For the current data, the %4$s upper bound on the misstatement is <b>%5$s</b> the performance materiality and the obtained precision is <b>%6$s</b> than the required minimum precision. \n\nThe conclusion on the basis of these results is that, with at least %7$s confidence and a precision of %8$s, the misstatement in the population is <b>%9$s</b> than the performance materiality. %10$s",
                            planningOptions[["confidenceLabel"]],
                            paste0(options[["minimumPrecisionPercentage"]] * 100, "%"),
                            planningOptions[["materialityLabel"]],
                            planningOptions[["confidenceLabel"]],
                            aboveBelowMateriality,
                            lowerHigherPrecision,
                            planningOptions[["confidenceLabel"]],
                            paste0(round(evaluationState[["precision"]] * 100, 3), "%"),
                            lowerHigherMateriality,
                            additionalMessage)
      }  
      conclusionContainer[["conclusionParagraph"]] <- createJaspHtml(message, "p")
      conclusionContainer[["conclusionParagraph"]]$position <- 1
      conclusionContainer[["conclusionParagraph"]]$dependOn(optionsFromObject = conclusionContainer)

      # Finsh conclusion
      jaspResults[["conclusionContainer"]] <- conclusionContainer
    }
  }
}

################################################################################
################## Common functions for the procedure stage ####################
################################################################################

.auditBookValueDescriptiveTable <- function(options, planningOptions, jaspResults,
                                            positionInContainer){

  procedureContainer <- .auditAnalysisContainer(jaspResults, stage = "procedure",
                                                position = 1)

  if(!options[["bookValueDescriptives"]] || options[["monetaryVariable"]] == "") 
    return() 

  .updateTabNumber(jaspResults)

  if(is.null(procedureContainer[["bookValueDescriptives"]])){

    tableTitle <- gettextf("<b>Table %1$i.</b> Descriptive Statistics for Ist Values",
                           jaspResults[["tabNumber"]]$object)
    
    descriptiveTable <- createJaspTable(tableTitle)
    descriptiveTable$position <- positionInContainer
    descriptiveTable$dependOn(options = c("bookValueDescriptives",
                                          "sampleDescriptives",
                                          "displaySample",
                                          "samplingChecked",
                                          "evaluationChecked"))

    descriptiveTable$addColumnInfo(name = 'populationSize',   title = gettext("Population size"),   type = 'string')
    descriptiveTable$addColumnInfo(name = 'populationValue',  title = gettext("Total value"),       type = 'string')
    descriptiveTable$addColumnInfo(name = 'absValue',         title = gettext("Absolute value"),    type = 'string')
    descriptiveTable$addColumnInfo(name = 'meanValue',        title = gettext("Mean"),              type = 'string')
    descriptiveTable$addColumnInfo(name = 'sigmaValue',       title = gettext("Std. deviation"),    type = 'string')
    descriptiveTable$addColumnInfo(name = 'q1',               title = gettext("1"),                 type = 'string',  overtitle = gettext("Quartile"))
    descriptiveTable$addColumnInfo(name = 'q2',               title = gettext("2"),                 type = 'string',  overtitle = gettext("Quartile"))
    descriptiveTable$addColumnInfo(name = 'q3',               title = gettext("3"),                 type = 'string',  overtitle = gettext("Quartile"))

    procedureContainer[["bookValueDescriptives"]] <- descriptiveTable

    if(options[["monetaryVariable"]] == "" || options[["recordNumberVariable"]] == "")
      return()

    procedureOptions <- jaspResults[["procedureOptions"]]$object
    valuta <- planningOptions[["valuta"]]

    row <- data.frame(populationSize    = procedureOptions[["populationSize"]], 
                      populationValue   = paste(valuta, round(procedureOptions[["populationValue"]], 2)), 
                      absValue          = paste(valuta, round(procedureOptions[["absPopulationValue"]], 2)),
                      meanValue         = paste(valuta, round(procedureOptions[["meanValue"]], 2)), 
                      sigmaValue        = paste(valuta, round(procedureOptions[["sigmaValue"]], 2)), 
                      q1                = paste(valuta, round(procedureOptions[["quantileValue"]][1], 2)), 
                      q2                = paste(valuta, round(procedureOptions[["quantileValue"]][2], 2)), 
                      q3                = paste(valuta, round(procedureOptions[["quantileValue"]][3], 2)))
    
    descriptiveTable$addRows(row)
  }
}

.auditBookValueDistributionPlot <- function(dataset, options, planningOptions, 
                                            jaspResults, positionInContainer){

  procedureContainer <- .auditAnalysisContainer(jaspResults, stage = "procedure",
                                                position = 1)
  
  if(!options[["bookValueDistribution"]] || options[["monetaryVariable"]] == "") 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(procedureContainer[["bookValueDistribution"]])){

    bookValuePlot <- createJaspPlot(plot = NULL, title = gettext("Distribution of Ist Values"), 
                                    width = 600, height = 300)

    bookValuePlot$position <- positionInContainer
    bookValuePlot$dependOn(options = c("bookValueDistribution", "valuta"))

    procedureContainer[["bookValueDistribution"]] <- bookValuePlot

    if(options[["recordNumberVariable"]] == "") return()

    procedureOptions  <- jaspResults[["procedureOptions"]]$object
    bookValue         <- dataset[, .v(options[["monetaryVariable"]])]
    mean              <- procedureOptions[["meanValue"]]
    stdev             <- procedureOptions[["sigmaValue"]]
    quantiles         <- procedureOptions[["quantileValue"]]

    legendData <- data.frame(x = c(0, 0, 0), y = c(0, 0, 0), l = c("1", "2", "3"))

    p <- .auditBarPlot(column = bookValue, variableName = options[["monetaryVariable"]], 
                        valuta = planningOptions[["valuta"]])

    p <- p + ggplot2::geom_point(mapping = ggplot2::aes(x = quantiles[1], y = 0), 
                                  shape = 21, fill = "orange", stroke = 2, size = 2) +
            ggplot2::geom_point(mapping = ggplot2::aes(x = quantiles[2], y = 0), 
                                  shape = 21, fill = "orange", stroke = 2, size = 2) +
            ggplot2::geom_point(mapping = ggplot2::aes(x = quantiles[3], y = 0), 
                                  shape = 21, fill = "orange", stroke = 2, size = 2) +
            ggplot2::geom_point(mapping = ggplot2::aes(x = mean, y = 0), 
                                  shape = 21, fill = "red", stroke = 2, size = 4) +
            ggplot2::geom_point(mapping = ggplot2::aes(x = mean + stdev, y = 0), 
                                  shape = 21, fill = "dodgerblue1", stroke = 2, size = 3) +
            ggplot2::geom_point(mapping = ggplot2::aes(x = mean - stdev, y = 0), 
                                  shape = 21, fill = "dodgerblue1", stroke = 2, size = 3) +
            ggplot2::geom_point(data = legendData, mapping = ggplot2::aes(x = x, y = y, shape = l), 
                                  size = 0, color = rgb(0, 1, 0, 0)) +
            ggplot2::scale_shape_manual(name = "", values = c(21, 21, 21), 
                                        labels = c(gettext("Mean"), gettextf("Mean %1$s sd", "\u00B1"), gettext("Quartile"))) +
            ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = c(4, 3, 2), shape = c(21, 21, 21), 
                                                          fill = c("red", "dodgerblue1", "orange"), stroke = 2, color = "black")), order = 1) 

    p <- JASPgraphs::themeJasp(p, legend.position = "top") + 
          ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -10, r = 50)),
                          panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", size = 0.5))

    bookValuePlot$plotObject <- p
  }

  if(options[["explanatoryText"]]){

      bookValuePlotText <- createJaspHtml(gettextf("<b>Figure %1$i.</b> The distribution of Ist values in the population. The red and blue dots respectively represent the mean and the values exactly one standard deviation from the mean. The orange dots represent the first, second (median) and third quartiles of the Ist values.", jaspResults[["figNumber"]]$object), "p")
      
      bookValuePlotText$position <- positionInContainer + 1
      bookValuePlotText$dependOn(optionsFromObject = procedureContainer[["bookValueDistribution"]])
      bookValuePlotText$dependOn(options = "explanatoryText")
      procedureContainer[["bookValuePlotText"]] <- bookValuePlotText
  }
}

.auditBarPlot <- function(column, variableName, valuta){

  h <- hist(column, plot = FALSE)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, h$counts), min.n = 4)
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(column, h$breaks), min.n = 4)

  p <- ggplot2::ggplot(data = data.frame(column), mapping = ggplot2::aes(x = column, y = ..count..)) + 
        ggplot2::scale_x_continuous(name = gettextf("Ist value (%1$s)", valuta), breaks = xBreaks, limits = range(xBreaks)) +
        ggplot2::scale_y_continuous(name = gettext("Frequency"), breaks = yBreaks, limits = c(0, max(yBreaks))) + 
        ggplot2::geom_histogram(binwidth = (h$breaks[2] - h$breaks[1]), fill = "grey", col = "black", size = .7, center = ((h$breaks[2] - h$breaks[1])/2))
  p <- JASPgraphs::themeJasp(p, axisTickWidth = .7, bty = list(type = "n", ldwX = .7, lwdY = 1))

  return(p)
}

################################################################################
################## Common functions for the Audit Risk Model ###################
################################################################################

.auditCalculateDetectionRisk <- function(options){

  inherentRisk <- base::switch(options[["IR"]],
                                "High" = 1,
                                "Medium" = 0.60,
                                "Low" = 0.50,
                                "Custom" = options[["irCustom"]])

  controlRisk <- base::switch(options[["CR"]],
                                "High" = 1,
                                "Medium" = 0.60,
                                "Low" = 0.50,
                                "Custom" = options[["crCustom"]])

  detectionRisk <- (1 - options[["confidence"]]) / inherentRisk / controlRisk
  return(detectionRisk)
}

.auditRiskModelParagraph <- function(options, 
                                     jaspResults, 
                                     position){

  if(!is.null(jaspResults[["ARMcontainer"]]) || 
    (!options[["performanceMateriality"]] && !options[["minimumPrecision"]])) 
    return()

  if(options[["priorConstructionMethod"]] != "arm" || !options[["performanceMateriality"]])
    return()

  ARMcontainer <- createJaspContainer(title = gettext("<u>Audit Risk Model</u>"))
  ARMcontainer$position <- position
  ARMcontainer$dependOn(options = c("confidence", 
                                    "IR",
                                    "irCustom", 
                                    "CR", 
                                    "crCustom",
                                    "materiality", 
                                    "materialityPercentage", 
                                    "materialityValue", 
                                    "explanatoryText", 
                                    "valuta",
                                    "otherValutaName",
                                    "performanceMateriality",
                                    "minimumPrecision",
                                    "priorConstructionMethod"))
  
  jaspResults[["ARMcontainer"]] <- ARMcontainer

  auditRisk <- 1 - options[["confidence"]]

  inherentRisk <- base::switch(options[["IR"]],
                                "High" = 1,
                                "Medium" = 0.60,
                                "Low" = 0.50,
                                "Custom" = options[["irCustom"]])

  controlRisk <- base::switch(options[["CR"]],
                                "High" = 1,
                                "Medium" = 0.60,
                                "Low" = 0.50,
                                "Custom" = options[["crCustom"]])

  if(options[["explanatoryText"]]){
    irLabel <- paste0(options[["IR"]], " = " , round(inherentRisk * 100, 2))
    crLabel <- paste0(options[["CR"]], " = " , round(controlRisk * 100, 2))
  } else {
    irLabel <- round(inherentRisk * 100, 2)
    crLabel <- round(controlRisk * 100, 2)
  }

  detectionRisk <- auditRisk / inherentRisk / controlRisk

  textARM <- gettextf("Audit risk (%1$s%%) = Inherent risk (%2$s%%) x Control risk (%3$s%%) x Detection risk (%4$s%%)",
                      round(auditRisk * 100, 2),
                      irLabel,
                      crLabel,
                      round(detectionRisk * 100, 2))
  
  ARMcontainer[["ARMformula"]] <- createJaspHtml(textARM, "h3", "21cm")
  ARMcontainer[["ARMformula"]]$position <- 2

  if(options[["explanatoryText"]]){

    irLabel <- paste0(options[["IR"]], " (", round(inherentRisk * 100, 2), "%)")
    crLabel <- paste0(options[["CR"]], " (", round(controlRisk * 100, 2), "%)")
    auditRiskLabel <- paste0(round(auditRisk * 100, 2), "%")
    dectectionRiskLabel <- paste0(round(detectionRisk * 100, 2), "%")

    message <- gettextf("Prior to the sampling procedure, the inherent risk was determined to be <b>%1$s</b>. The internal control risk was determined to be <b>%2$s</b>. According to the Audit Risk Model, the required detection risk to maintain an audit risk of <b>%3$s</b> should be <b>%4$s</b>.",
                        irLabel,
                        crLabel,
                        auditRiskLabel,
                        dectectionRiskLabel)

    if(options[["IR"]] == "Custom" || options[["CR"]] == "Custom"){

      message <- gettextf("%1$s The translation of High, Medium and Low to probabilities is done according custom preferences</b>.",
                          message)
    
    } else {

      message <- gettextf("%1$s The translation of High, Medium and Low to probabilities is done according to <b>IODAD (2007)</b>.",
                        message)
    
    }

    ARMcontainer[["AuditRiskModelParagraph"]] <- createJaspHtml(message, "p")
    ARMcontainer[["AuditRiskModelParagraph"]]$position <- 1
  }
}

################################################################################
################## Common functions for the planning stage #####################
################################################################################

.auditPlanningState <- function(options, 
                                planningOptions, 
                                planningContainer, 
                                ready, 
                                jaspResults){
                                  
  if(!is.null(planningContainer[["planningState"]])){

    return(planningContainer[["planningState"]]$object)

  } else if(ready && !planningContainer$getError()){

    if(options[["workflow"]])
      dataset <- .auditReadDataset(options, jaspResults, stage = "procedure")

    confidence <- options[["confidence"]]

	minPrecision <- NULL
    if(options[["minimumPrecision"]])
      minPrecision <- options[["minimumPrecisionPercentage"]]

    performanceMateriality <- NULL
    if(options[["performanceMateriality"]])
      performanceMateriality <- planningOptions[["materiality"]]

	N <- planningOptions[["populationSize"]]
	if(options[["workflow"]] && options[["monetaryVariable"]] != "")
		N <- ceiling(planningOptions[["populationValue"]])
	if(!options[["workflow"]] && options[["populationValue"]] > 0)
		N <- ceiling(planningOptions[["populationValue"]])

    if(options[["priorConstructionMethod"]] == "arm"){

      auditRisk <- 1 - options[["confidence"]]

      inherentRisk <- base::switch(options[["IR"]],
                                    "High" = 1,
                                    "Medium" = 0.60,
                                    "Low" = 0.50,
                                    "Custom" = options[["irCustom"]])

      controlRisk <- base::switch(options[["CR"]],
                                    "High" = 1,
                                    "Medium" = 0.60,
                                    "Low" = 0.50,
                                    "Custom" = options[["crCustom"]])

      detectionRisk <- auditRisk / inherentRisk / controlRisk

      if(detectionRisk >= 1){
        planningContainer$setError(gettextf("The detection risk is equal to or higher than 100%. Please re-specify your custom values for the Inherent risk and/or Control risk."))  
        return()
      }

      confidence <- 1 - detectionRisk
    }

    if(!options[["bayesianAnalysis"]]){

      result <- try({
        jfa::planning(materiality = performanceMateriality, 
                      confidence = confidence, 
                      expectedError = planningOptions[["expectedErrors"]], 
                      minPrecision = minPrecision,
                      likelihood = planningOptions[["likelihood"]], 
                      N = N,
                      increase = options[["sampleSizeIncrease"]])
                    })

    } else if(options[["bayesianAnalysis"]]){

      prior <- jfa::auditPrior(materiality = performanceMateriality, 
                    confidence = planningOptions[["confidence"]],
                    expectedError = planningOptions[["expectedErrors"]], 
                    likelihood = planningOptions[["likelihood"]], 
                    N = N, 
                    ir = inherentRisk, 
                    cr = controlRisk,
                    method = options[["priorConstructionMethod"]],
                    sampleN = options[["sampleN"]],
                    sampleK = options[["sampleK"]],
                    factor = options[["factor"]],
                    pHplus = 1 - options[["pHmin"]],
                    pHmin = options[["pHmin"]])

      result <- try({

        if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != ""){

          .auditUvbPlanning(options, planningOptions, dataset, prior, N)

        } else {

          jfa::planning(materiality = performanceMateriality, 
                        confidence = planningOptions[["confidence"]], 
                        expectedError = planningOptions[["expectedErrors"]], 
                        likelihood = planningOptions[["likelihood"]],
                        N = N, 
                        prior = prior, 
                        minPrecision = minPrecision,
                        increase = options[["sampleSizeIncrease"]],
                        maxSize = 10000)
        }
      })

    }

    if(isTryError(result)){

      if(JASP:::.extractErrorMessage(result) == "Sample size could not be calculated, please increase the maxSize argument"){
        planningContainer$setError(gettext("You cannot achieve your current sampling objectives with this population. The resulting sample size exceeds 5000. Adjust your sampling objectives."))
        return()
      }

      planningContainer$setError(gettextf("An error occurred: %1$s", JASP:::.extractErrorMessage(result)))
      return()
    }

    # This causes an error in the planning stage [needs a further look]
    if(options[["workflow"]]){
      if(options[["selectionType"]] == "recordSampling" && result[["sampleSize"]] > planningOptions[["populationSize"]]){
        planningContainer$setError(gettext("You cannot achieve your current sampling objectives with this population. The resulting sample size is larger than the population size. Adjust your sampling objectives."))
        return()
      } else if(options[["selectionType"]] == "musSampling" && result[["sampleSize"]] > planningOptions[["populationValue"]]){
        planningContainer$setError(gettext("You cannot achieve your current sampling objectives with this population. The resulting sample size is larger than the population value. Adjust your sampling objectives."))
        return()
      }
    }

    planningContainer[["planningState"]] <- createJaspState(result)
    planningContainer[["planningState"]]$dependOn(options = c("planningModel"))
    
    return(result)

  } else {

	N <- planningOptions[["populationSize"]]
	if(options[["workflow"]] && options[["monetaryVariable"]] != "")
		N <- ceiling(planningOptions[["populationValue"]])
	if(!options[["workflow"]] && options[["populationValue"]] > 0)
		N <- ceiling(planningOptions[["populationValue"]])

    bPrior <- ifelse(options[["planningModel"]] == "Poisson", yes = 0, no = 1)

    noResults <- list(sampleSize = 0, 
                      materiality = planningOptions[["materiality"]], 
                      N = N, 
                      expectedSampleError = 0,
                      prior = list(aPrior = 1, 
                                   bPrior = bPrior, 
                                   nPrior = 0, 
                                   kPrior = 0))

    return(noResults)

  }
}

.auditPlanningSummaryTable <- function(options, 
                                       planningOptions, 
                                       planningState, 
                                       planningContainer, 
                                       jaspResults,
                                       ready, 
                                       positionInContainer){

  .updateTabNumber(jaspResults)

  if(!is.null(planningContainer[["summaryTable"]]))
    return()

  tableTitle <- gettextf("<b>Table %1$i.</b> Planning Summary",
                         jaspResults[["tabNumber"]]$object)                                        

  summaryTable <- createJaspTable(tableTitle)
  summaryTable$position <- positionInContainer
  summaryTable$dependOn(options = c("bookValueDescriptives",
                                    "sampleDescriptives",
                                    "displaySample",
                                    "samplingChecked",
                                    "evaluationChecked",
                                    "planningModel",
                                    "expectedEvidenceRatio",
                                    "expectedBayesFactor",
                                    "expectedErrors",
                                    "minimumPrecision",
                                    "performanceMateriality",
                                    "separateKnownAndUnknownMisstatement",
                                    "sampleSizeIncrease"))

  if(options[["performanceMateriality"]])
    summaryTable$addColumnInfo(name = 'materiality',          
                              title = gettext("Performance materiality"),          
                              type = 'string')

  if(options[["minimumPrecision"]])
    summaryTable$addColumnInfo(name = 'precision',          
                              title = gettext("Required precision"),          
                              type = 'string')
  
  if(options[["performanceMateriality"]] && options[["priorConstructionMethod"]] == "arm"){
    summaryTable$addColumnInfo(name = 'IR',                   
                              title = gettext("Inherent risk"),        
                              type = 'string')
    summaryTable$addColumnInfo(name = 'CR',                   
                              title = gettext("Control risk"),         
                              type = 'string')
    summaryTable$addColumnInfo(name = 'DR',                   
                              title = gettext("Detection risk"),       
                              type = 'string')
  } else {
    summaryTable$addColumnInfo(name = 'AR',                   
                              title = gettext("Audit risk"),        
                              type = 'string')
  }

  summaryTable$addColumnInfo(name = 'k',                    
                             title = gettext("Expected errors"),       
                             type = ifelse(options[["expectedErrors"]] == "expectedAllPossible", yes = "string", no = "number"))
  summaryTable$addColumnInfo(name = 'n',                    
                             title = gettext("Required sample size"), 
                             type = 'integer')

  if(options[["bayesianAnalysis"]] && options[["expectedEvidenceRatio"]] && options[["performanceMateriality"]]){
    summaryTable$addColumnInfo(name = 'expectedEvidenceRatio',              
                               title = gettext("Expected evidence ratio"), 
                               type = 'number')
  }

  if(options[["bayesianAnalysis"]] && options[["expectedBayesFactor"]] && options[["performanceMateriality"]]){
    summaryTable$addColumnInfo(name = 'expectedBayesFactor',              
                               title = gettextf("Expected %1$s", "BF\u208B\u208A"), 
                               type = 'number')
  }

  planningContainer[["summaryTable"]] <- summaryTable

  auditRisk <- 1 - options[["confidence"]]

  if(options[["priorConstructionMethod"]] == "arm"){
    inherentRisk <- base::switch(options[["IR"]],
                                  "High" = 1,
                                  "Medium" = 0.60,
                                  "Low" = 0.50,
                                  "Custom" = options[["irCustom"]])

    controlRisk <- base::switch(options[["CR"]],
                                  "High" = 1,
                                  "Medium" = 0.60,
                                  "Low" = 0.50,
                                  "Custom" = options[["crCustom"]])
    detectionRisk <- auditRisk / inherentRisk / controlRisk
  }

  	N <- planningOptions[["populationSize"]]
	if(options[["workflow"]]){
		if(options[["monetaryVariable"]] == "")
			N <- ceiling(planningOptions[["populationValue"]])
	} else {
		if(options[["populationValue"]] != 0)
			N <- ceiling(planningOptions[["populationValue"]])
	}

  if(!ready || planningContainer$getError()){

    if(!options[["bayesianAnalysis"]]){

      message <- base::switch(options[["planningModel"]],
                              "Poisson" = gettext("The required sample size is based on the <b>Poisson</b> distribution."),
                              "binomial" =  gettext("The required sample size is based on the <b>binomial</b> distribution."),
                              "hypergeometric" = gettext("The required sample size is based on the <b>hypergeometric</b> distribution."))
    
    } else if(options[["bayesianAnalysis"]]){

      message <- base::switch(options[["planningModel"]],
                              "Poisson" = gettext("The required sample size is based on the <b>gamma</b> distribution."),
                              "binomial" = gettext("The required sample size is based on the <b>beta</b> distribution."),
                              "hypergeometric" = gettextf("The required sample size is based on the <b>beta-binomial</b> distribution (N = %1$s).", N))
    
    }

    summaryTable$addFootnote(message)

    row <- data.frame(k = ".", n = ".")

    if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
      row <- cbind(row, materiality = planningOptions[["materialityLabel"]])
    } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
      row <- cbind(row, precision = paste0(round(options[["minimumPrecisionPercentage"]] * 100, 2), "%"))
    } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
      row <- cbind(row, materiality = planningOptions[["materialityLabel"]], 
                        precision = paste0(round(options[["minimumPrecisionPercentage"]] * 100, 2), "%"))      
    } 

    if(options[["performanceMateriality"]] && options[["priorConstructionMethod"]] == "arm"){
      row <- cbind(row, 
                   IR = paste0(round(inherentRisk * 100, 2), "%"),
                   CR = paste0(round(controlRisk * 100, 2), "%"),
                   DR = paste0(round(detectionRisk * 100, 2), "%"))
    } else {
      row <- cbind(row, AR = paste0(round(auditRisk * 100, 2), "%"))
    }

    if(options[["bayesianAnalysis"]] && options[["expectedEvidenceRatio"]] && options[["performanceMateriality"]])
      row <- cbind(row, expectedEvidenceRatio = ".")
    if(options[["bayesianAnalysis"]] && options[["expectedBayesFactor"]] && options[["performanceMateriality"]])
      row <- cbind(row, expectedBayesFactor = ".")
    
    summaryTable$addRows(row)
    summaryTable$addFootnote(message = gettext("Either the materiality (if applicable), the population size, or the population value is defined as zero."), 
                             symbol = gettextf("%1$s <b>Results could not be calculated.</b>", "\u26A0"))
    
    return()
  }

  if(!options[["bayesianAnalysis"]]){

    message <- base::switch(options[["planningModel"]],
                            "Poisson" = gettextf("The required sample size is based on the <b>Poisson</b> distribution <i>(%1$s = %2$s)</i>.",
                                                 "\u03BB", 
                                                  round(planningState[["materiality"]] * planningState[["sampleSize"]], 4)),
                            "binomial" =  gettextf("The required sample size is based on the <b>binomial</b> distribution <i>(p = %1$s)</i>", 
                                                   round(planningState[["materiality"]], 2)),
                            "hypergeometric" = gettextf("The required sample size is based on the <b>hypergeometric</b> distribution <i>(N = %1$s, K = %2$s)</i>.", 
                                                      planningState[["N"]], 
                                                      ceiling(planningState[["N"]] * planningState[["materiality"]])))
  
  } else if(options[["bayesianAnalysis"]]){

    message <- base::switch(options[["planningModel"]],
                            "Poisson" = gettextf("The required sample size is based on the <b>gamma</b> distribution <i>(%1$s = %2$s, %3$s = %4$s)</i>",
                                                  "\u03B1", 
                                                  planningState[["prior"]]$aPrior,
                                                  "\u03B2", 
                                                  planningState[["prior"]]$bPrior),
                            "binomial" = gettextf("The required sample size is based on the <b>beta</b> distribution <i>(%1$s = %2$s, %3$s = %4$s)</i>.", 
                                                  "\u03B1",
                                                  planningState[["prior"]]$aPrior,
                                                  "\u03B2", 
                                                  planningState[["prior"]]$bPrior),
                            "hypergeometric" = gettextf("The required sample size is based on the <b>beta-binomial</b> distribution <i>(N = %1$s, %2$s = %3$s, %4$s = %5$s)</i>.", 
                                                        planningState[["N"]] - planningState[["sampleSize"]] + planningState[["expectedSampleError"]],
                                                        "\u03B1", 
                                                        planningState[["prior"]]$aPrior, 
                                                        "\u03B2", 
                                                        planningState[["prior"]]$bPrior))
  
  }

  summaryTable$addFootnote(message)

  n <- planningState[["sampleSize"]]

  k <- base::switch(options[["expectedErrors"]], 
                    "expectedRelative" = planningState[["expectedSampleError"]], 
                    "expectedAbsolute" = options[["expectedNumber"]],
                    "expectedAllPossible" = paste0("0 - ", n))

  if(options[["performanceMateriality"]] && options[["materiality"]] == "materialityValue")
    k <- paste(planningOptions[["valuta"]], k)

  row <- data.frame(k = k, n = n)

  if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
    row <- cbind(row, materiality = planningOptions[["materialityLabel"]])
  } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
    row <- cbind(row, precision = paste0(round(options[["minimumPrecisionPercentage"]] * 100, 2), "%"))
  } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
    row <- cbind(row, materiality = planningOptions[["materialityLabel"]], 
                      precision = paste0(round(options[["minimumPrecisionPercentage"]] * 100, 2), "%"))      
  } 

  if(options[["performanceMateriality"]] && options[["priorConstructionMethod"]] == "arm"){
    row <- cbind(row, 
                  IR = paste0(round(inherentRisk * 100, 2), "%"),
                  CR = paste0(round(controlRisk * 100, 2), "%"),
                  DR = paste0(round(detectionRisk * 100, 2), "%"))
  } else {
    row <- cbind(row, AR = paste0(round(auditRisk * 100, 2), "%"))
  }

  if(options[["bayesianAnalysis"]] && options[["performanceMateriality"]] &&
      (options[["expectedEvidenceRatio"]] || options[["expectedBayesFactor"]])){

    expResult <- .auditExpectedEvidenceRatio(planningState)

    if(options[["expectedEvidenceRatio"]])
      row <- cbind(row, expectedEvidenceRatio = expResult[["posteriorEvidenceRatio"]])

    if(options[["expectedBayesFactor"]])
      row <- cbind(row, expectedBayesFactor = expResult[["expectedShift"]])

  }

  summaryTable$addRows(row)

  if(options[["bayesianAnalysis"]] && options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != ""){
    message <- gettextf("The value %1$s is automatically used as a starting point for the fixed interval selection.", planningState[["startingPoint"]])
    summaryTable$addFootnote(message, symbol = gettextf("%1$s", "\u26A0"))
  }
}

.fillCriticalTransactions <- function(options, jaspResults){

  if(options[["recordNumberVariable"]] == "" || options[["monetaryVariable"]] == "")
    return()

    dataset <- .auditReadDataset(options, jaspResults, stage = "procedure")
    
    if(options[["flagCriticalTransactions"]]){

      criticalTransactions <- rep(0, nrow(dataset))

      if(options[["flagNegativeValues"]])
        criticalTransactions[which(dataset[, .v(options[["monetaryVariable"]])] < 0)] <- 1

    } else {
      
      criticalTransactions          <- rep(NA, nrow(dataset))

    }
    
    if(is.null(jaspResults[["criticalTransactions"]]))  
      jaspResults[["criticalTransactions"]] <- createJaspColumn(columnName = options[["criticalTransactions"]], dependencies = "criticalTransactions")

    jaspResults[["criticalTransactions"]]$setScale(criticalTransactions)
}

.auditAddCriticalTransactions <- function(options, sample){

	if(options[["workflow"]] && options[["flagCriticalTransactions"]] && options[["handleCriticalTransactions"]] == "inspect"){
		monetaryVariable <- .auditReadVariableFromOptions(options, varType = "monetary")
		auditResult <- .auditReadVariableFromOptions(options, varType = "auditResult")
		criticalVariable <- .auditReadVariableFromOptions(options, varType = "critical")
		variables <- c(monetaryVariable, auditResult, criticalVariable)
		criticalTransactions <- .readDataSetToEnd(columns.as.numeric = variables)
		criticalTransactions <- subset(criticalTransactions, criticalTransactions[, .v(criticalVariable)] > 0)
		criticalData <- data.frame(x = as.numeric(rownames(criticalTransactions)), 
											y = criticalTransactions[, .v(options[["monetaryVariable"]])], 
											l = rep(1, nrow(criticalTransactions)),
											m = criticalTransactions[, .v(options[["auditResult"]])],
											z = rep(1, nrow(criticalTransactions)))
		colnames(criticalData) <- colnames(sample)
		rownames(criticalData) <- rownames(criticalTransactions)
		sample <- rbind(sample, criticalData) 
		return(sample)
	} else {
		return(sample)
	}
}

.sampleSizeComparisonPlot <- function(options, 
                                      planningOptions, 
                                      planningState, 
                                      planningContainer, 
                                      jaspResults,
                                      ready, 
                                      positionInContainer){

  if(!options[["decisionPlot"]]) 
    return()

  if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != "")
    return()

  if(!options[["priorConstructionMethod"]] %in% c("none", "arm", "sample", "factor"))
    return()

  .updateFigNumber(jaspResults)

  if(is.null(planningContainer[["decisionPlot"]])){

    decisionPlot <- createJaspPlot(plot = NULL, 
                                  title = gettext("Comparison of Required Sample Sizes"), 
                                  width = 600, 
                                  height = 300)

    decisionPlot$position <- positionInContainer
    decisionPlot$dependOn(options = c("decisionPlot"))

    planningContainer[["decisionPlot"]] <- decisionPlot

    if(!ready || planningContainer$getError()) 
      return()

    confidence <- options[["confidence"]]
    if(!options[["bayesianAnalysis"]])
      confidence <- 1 - .auditCalculateDetectionRisk(options)

    minPrecision <- NULL
    if(options[["minimumPrecision"]])
      minPrecision <- planningOptions[["minimumPrecision"]]

    performanceMateriality <- NULL
    if(options[["performanceMateriality"]])
      performanceMateriality <- planningOptions[["materiality"]]

	N <- planningOptions[["populationSize"]]
	if(options[["workflow"]]){
		if(options[["monetaryVariable"]] == "")
			N <- ceiling(planningOptions[["populationValue"]])
	} else {
		if(options[["populationValue"]] != 0)
			N <- ceiling(planningOptions[["populationValue"]])
	}

    if(!options[["bayesianAnalysis"]]){

      startProgressbar(3)

      n1 <- jfa::planning(materiality = performanceMateriality, 
                          confidence = confidence, 
                          expectedError = planningOptions[["expectedErrors"]], 
                          likelihood = "binomial", 
                          minPrecision = minPrecision,
                          N = N,
                          increase = options[["sampleSizeIncrease"]])

      progressbarTick() 

      n2 <- jfa::planning(materiality = performanceMateriality, 
                          confidence = confidence, 
                          expectedError = planningOptions[["expectedErrors"]], 
                          likelihood = "poisson",
                          minPrecision = minPrecision, 
                          N = N,
                          increase = options[["sampleSizeIncrease"]])

      progressbarTick()      

      n3 <- jfa::planning(materiality = performanceMateriality, 
                          confidence = confidence, 
                          expectedError = planningOptions[["expectedErrors"]], 
                          likelihood = "hypergeometric", 
                          minPrecision = minPrecision,
                          N = N,
                          increase = options[["sampleSizeIncrease"]])

      progressbarTick()
      
      n <- c(n1$sampleSize, n2$sampleSize, n3$sampleSize)   
      k <- c(n1$expectedSampleError, n2$expectedSampleError, n3$expectedSampleError)

      d <- data.frame(y = c(n, k), 
                      dist = rep(c("Binomial", "Poisson", "Hypergeometric"), 2),
                      nature = rep(c(gettext("Expected error-free"), gettext("Expected errors")), each = 3))
      d$dist <- factor(x = d$dist, levels = levels(d$dist)[c(2, 3, 1)])
      d$nature <- factor(d$nature, levels = levels(d$nature)[c(1, 2)])

    } else if(options[["bayesianAnalysis"]]){

      inherentRisk <- base::switch(options[["IR"]],
                                    "High" = 1,
                                    "Medium" = 0.60,
                                    "Low" = 0.50,
                                    "Custom" = options[["irCustom"]])

      controlRisk <- base::switch(options[["CR"]],
                                    "High" = 1,
                                    "Medium" = 0.60,
                                    "Low" = 0.50,
                                    "Custom" = options[["crCustom"]])

      startProgressbar(3)

      p1 <- jfa::auditPrior(materiality = performanceMateriality, 
                            confidence = confidence,
                            expectedError = planningOptions[["expectedErrors"]], 
                            likelihood = "binomial", 
                            N = N, 
                            ir = inherentRisk, 
                            cr = controlRisk,
                            method = options[["priorConstructionMethod"]],
                            sampleN = options[["sampleN"]],
                            sampleK = options[["sampleK"]],
                            factor = options[["factor"]],
                            pHplus = 1 - options[["pHmin"]],
                            pHmin = options[["pHmin"]])

      n1 <- jfa::planning(materiality = performanceMateriality, 
                          confidence = confidence, 
                          expectedError = planningOptions[["expectedErrors"]], 
                          N = N,
                          minPrecision = minPrecision, 
                          prior = p1,
                          increase = options[["sampleSizeIncrease"]])
      
      progressbarTick()
      
      p2 <- jfa::auditPrior(materiality = performanceMateriality, 
                            confidence = confidence,
                            expectedError = planningOptions[["expectedErrors"]], 
                            likelihood = "poisson", 
                            N = N, 
                            ir = inherentRisk, 
                            cr = controlRisk,
                            method = options[["priorConstructionMethod"]],
                            sampleN = options[["sampleN"]],
                            sampleK = options[["sampleK"]],
                            factor = options[["factor"]],
                            pHplus = 1 - options[["pHmin"]],
                            pHmin = options[["pHmin"]])

      n2 <- jfa::planning(materiality = performanceMateriality, 
                          confidence = confidence, 
                          expectedError = planningOptions[["expectedErrors"]], 
                          N = N,
                          minPrecision = minPrecision, 
                          prior = p2,
                          increase = options[["sampleSizeIncrease"]])
      
      progressbarTick()

      p3 <- jfa::auditPrior(materiality = performanceMateriality, 
                            confidence = confidence,
                            expectedError = planningOptions[["expectedErrors"]], 
                            likelihood = "hypergeometric", 
                            N = N, 
                            ir = inherentRisk, 
                            cr = controlRisk,
                            method = options[["priorConstructionMethod"]],
                            sampleN = options[["sampleN"]],
                            sampleK = options[["sampleK"]],
                            factor = options[["factor"]],
                            pHplus = 1 - options[["pHmin"]],
                            pHmin = options[["pHmin"]])

      n3 <- jfa::planning(materiality = performanceMateriality, 
                          confidence = confidence, 
                          expectedError = planningOptions[["expectedErrors"]], 
                          N = N, 
                          minPrecision = minPrecision,
                          prior = p3,
                          increase = options[["sampleSizeIncrease"]])

      progressbarTick()

      n <- c(n1$sampleSize, n2$sampleSize, n3$sampleSize)  
      k <- c(n1$expectedSampleError, n2$expectedSampleError, n3$expectedSampleError)

      d <- data.frame(y = c(n, k), 
                      dist = rep(c("Beta", "Gamma", "Beta-binomial"), 2),
                      nature = rep(c(gettext("Expected error-free"), gettext("Expected errors")), each = 3))
      d$dist <- factor(x = d$dist, levels = levels(d$dist)[c(2, 3, 1)])
      d$nature <- factor(x = d$nature, levels = levels(d$nature)[c(1, 2)])
    }

    yBreaks <- JASPgraphs::getPrettyAxisBreaks(0:(ceiling(1.1 * max(n))), min.n = 4)
    yLimits <- c(0, ceiling(1.2 * max(n)))

    myTheme <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), 
                              axis.ticks.y = ggplot2::element_blank(), 
                              axis.text.y = ggplot2::element_text(hjust = 0),
                              panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb", size = 0.5),
                              legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30)))

    p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = dist, y = y, fill = nature)) +
          ggplot2::geom_bar(stat = "identity", col = "black", size = 1) +
          ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
          ggplot2::coord_flip() +
          ggplot2::annotate("text", y = k, x = c(3, 2, 1), label = k, 
                            size = 6, vjust = 0.5, hjust = -0.3) + 
          ggplot2::annotate("text", y = n, x = c(3, 2, 1), label = n, 
                            size = 6, vjust = 0.5, hjust = -0.5) +
          ggplot2::xlab("") +
          ggplot2::ylab(gettext("Required sample size")) +
          ggplot2::labs(fill = "") +
          ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), guide = ggplot2::guide_legend(reverse = TRUE))

    p <- JASPgraphs::themeJasp(p, sides = "", legend.position = "top") + myTheme

    decisionPlot$plotObject <- p
  }

  if(options[["explanatoryText"]] && ready){

    decisionPlotText <- createJaspHtml(gettextf("<b>Figure %1$i.</b> Comparison of the required sample sizes under different probability distributions. The bars represent the required sample sizes for the current planning options. The number of expected errors in each bar is colored in red and the number of expected error-free transactions is colored in green.",
                                                jaspResults[["figNumber"]]$object), "p")
    
    decisionPlotText$position <- positionInContainer + 1
    decisionPlotText$dependOn(optionsFromObject = planningContainer[["decisionPlot"]])
    decisionPlotText$dependOn(options = "explanatoryText")
    planningContainer[["decisionPlotText"]] <- decisionPlotText  
  }
}

.auditSamplingDistributionPlot <- function(options, 
                                           planningOptions, 
                                           planningState, 
                                           planningContainer, 
                                           jaspResults,
                                           ready, 
                                           positionInContainer){

  if(!options[["samplingDistribution"]]) 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(planningContainer[["samplingDistribution"]])){

    likelihood <- base::switch(options[["planningModel"]], 
                               "Poisson" = "Poisson", 
                               "binomial" = "Binomial", 
                               "hypergeometric" = "Hypergeometric")

    plotTitle <- gettextf("Implied %1$s Distribution of Errors", likelihood)

    samplingDistribution <- createJaspPlot(plot = NULL, 
                                           title = plotTitle, 
                                           width = 600, 
                                           height = 300)

    samplingDistribution$position <- positionInContainer
    samplingDistribution$dependOn(options = c("planningModel", 
                                              "samplingDistribution"))

    planningContainer[["samplingDistribution"]] <- samplingDistribution

    if(!ready || planningContainer$getError()) 
      return()

    xVals <- 0:planningState[["sampleSize"]]
    limx <- planningState[["sampleSize"]] + 1
    if(limx > 31)
        limx <- 31
    xVals <- xVals[1:limx]

    if(planningState[["likelihood"]] == "poisson"){

      dErrorFree <- stats::dpois(x = xVals, 
                                 lambda = planningState[["materiality"]] * 
                                          planningState[["sampleSize"]])

      dError <- stats::dpois(x = 0:planningState[["expectedSampleError"]], 
                             lambda = planningState[["materiality"]] * 
                                      planningState[["sampleSize"]])

    } else if(planningState[["likelihood"]] == "binomial"){

      dErrorFree <- stats::dbinom(x = xVals, 
                                  size = planningState[["sampleSize"]], 
                                  prob = planningState[["materiality"]])

      dError <- stats::dbinom(x = 0:planningState[["expectedSampleError"]], 
                              size = planningState[["sampleSize"]], 
                              prob = planningState[["materiality"]])

    } else if(planningState[["likelihood"]] == "hypergeometric"){

      dErrorFree <- stats::dhyper(x = xVals, 
                                  m = planningState[["populationK"]], 
                                  n = planningState[["N"]] - 
                                      planningState[["populationK"]], 
                                  k = planningState[["sampleSize"]])

      dError <- stats::dhyper(x = 0:planningState[["expectedSampleError"]], 
                              m = planningState[["populationK"]], 
                              n = planningState[["N"]] - 
                                  planningState[["populationK"]], 
                              k = planningState[["sampleSize"]])

    }

    dataErrorFree <- data.frame(x = xVals, 
                                y = dErrorFree)
    dataError <- data.frame(x = 0:planningState[["expectedSampleError"]], 
                            y = dError)

    dataLegend <- data.frame(x = c(0, 0), 
                             y = c(0, 0), 
                             type = c(gettext("Expected error-free"), gettext("Expected errors")))
    dataLegend$type <- factor(x = dataLegend[["type"]], 
                              levels = levels(dataLegend[["type"]])[c(2,1)])

    xTicks <- JASPgraphs::getPrettyAxisBreaks(c(0, xVals))
    yTicks <- JASPgraphs::getPrettyAxisBreaks(c(0, dataErrorFree[["y"]]))

    myLegend <- ggplot2::guide_legend(override.aes=list(size = 12, 
                                                       shape = 22, 
                                                       fill = c("#FF6666", 
                                                                "#7FE58B"), 
                                                       stroke = 1.5, 
                                                       color = "black"))
    
    myTheme <- ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", size = 0.5),
                              legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30)))

    p <- ggplot2::ggplot(data = dataLegend, 
                         mapping = ggplot2::aes(x = x, y = y, fill = type)) +
          ggplot2::geom_point(shape = 2, 
                              alpha = 0) +
          ggplot2::scale_x_continuous(name = gettext("Errors"), 
                                      labels = xTicks, 
                                      breaks = xTicks) +
          ggplot2::scale_y_continuous(name = gettext("Probability"), 
                                      labels = yTicks, 
                                      breaks = yTicks,
                                      limits = range(yTicks)) +
          ggplot2::geom_bar(data = dataErrorFree, 
                            mapping = ggplot2::aes(x = x, y = y), 
                            stat = "identity", 
                            fill = "#7FE58B", 
                            size = 0.5, 
                            color = "black") +
          ggplot2::geom_bar(data = dataError, 
                            mapping = ggplot2::aes(x = x, y = y), 
                            stat = "identity", 
                            fill = "#FF6666", 
                            size = 0.5, 
                            color = "black") +
          ggplot2::geom_point(data = dataLegend, 
                              mapping = ggplot2::aes(x = x, y = y, fill = type), 
                              size = 0) +
          ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), 
                                     guide = ggplot2::guide_legend(reverse = TRUE)) +
          ggplot2::labs(fill = "") +
          ggplot2::guides(fill = myLegend)
    
    p <- JASPgraphs::themeJasp(p, legend.position = "top") + myTheme

    samplingDistribution$plotObject <- p

  }

  if(options[["explanatoryText"]] && ready){

    samplingDistributionText <- createJaspHtml(gettextf("<b>Figure %1$i.</b> The implied <b>%2$s</b> sampling distribution. The number of expected errors in the selection is colored in red and the number of expected error-free observations is colored in green. The total probability of the errors does not exceed the detection risk as specified through the audit risk model.",
                                                      jaspResults[["figNumber"]]$object,
                                                      options[["planningModel"]]), "p")
    
    samplingDistributionText$position <- positionInContainer + 1
    samplingDistributionText$dependOn(optionsFromObject = planningContainer[["samplingDistribution"]])
    samplingDistributionText$dependOn(options = "explanatoryText")
    planningContainer[["samplingDistributionText"]] <- samplingDistributionText
  }
}

################################################################################
################## Common functions for the selection stage ####################
################################################################################

.auditAddSelectionColumns <- function(options, 
                                      jaspResults){

  dataset <- .auditReadDataset(options, jaspResults, stage = "procedure")
  
  rankingVariable <- options[["rankingVariable"]]
  if(rankingVariable == "")
    rankingVariable <- NULL

  additionalVariables <- unlist(options[["additionalVariables"]])
  variables <- c(rankingVariable, additionalVariables)

  if(!is.null(variables)){

    additionalColumns <- .readDataSetToEnd(columns.as.numeric = variables)
    dataset <- cbind(dataset, additionalColumns)
    return(dataset)

  } else {

    return(dataset)

  }
}

.auditAddSelectionIndicator <- function(options, 
                                     planningOptions,
                                     selectionState, 
                                     jaspResults){
  
  if(!options[["addSampleIndicator"]] || options[["sampleIndicatorColumn"]] == "")  
    return()

  if(is.null(jaspResults[["sampleIndicatorColumn"]])){

    sampleIndicatorColumn <- numeric(length = planningOptions[["populationSize"]])
    sampleRowNumbers <- selectionState[["rowNumber"]]
    sampleCounts <- selectionState[["count"]]
    sampleIndicatorColumn[sampleRowNumbers] <- sampleCounts
    jaspResults[["sampleIndicatorColumn"]] <- createJaspColumn(columnName = options[["sampleIndicatorColumn"]])
    jaspResults[["sampleIndicatorColumn"]]$dependOn(options = c("recordNumberVariable",
                                                                "monetaryVariable",
                                                                "additionalVariables",
                                                                "rankingVariable",
                                                                "selectionMethod",
                                                                "selectionType",
                                                                "seed",
                                                                "intervalStartingPoint",
                                                                "sampleSize",
                                                                "addSampleIndicator", 
                                                                "sampleIndicatorColumn"))
    jaspResults[["sampleIndicatorColumn"]]$setNominal(sampleIndicatorColumn)

  }  
}

.auditSelectionState <- function(dataset,
                                 options, 
                                 planningState, 
                                 selectionContainer){
                               
  if(!is.null(selectionContainer[["selectionState"]])){

    return(selectionContainer[["selectionState"]]$object)

  } else if(!is.null(planningState)){

    result <- try({

      .auditSampling(dataset,
                     options,
                     planningState,
                     selectionContainer)

    })

    if(isTryError(result)){

      if(options[["selectionType"]] == "musSampling"){
        # MUS has failed for some reason, fall back to record sampling

        result <- try({

          .auditSampling(dataset,
                        options,
                        planningState,
                        selectionContainer,
                        unitsExtra = "records")

        }) 

      }

      if(isTryError(result)){

        selectionContainer$setError(gettextf("An error occurred: %1$s", 
                                             JASP:::.extractErrorMessage(result)))
        return()

      } else {
        # MUS has failed for some reason, return an indication for this
        result[["musFailed"]] <- TRUE

      }
    }

    selectionContainer[["selectionState"]] <- createJaspState(result)
    
    return(result)
  }
}

.auditSampling <- function(dataset,
                           options,
                           planningState,
                           selectionContainer,
                           unitsExtra = NULL){
  
  if(!is.null(unitsExtra)){
    units <- unitsExtra
  } else {
    units <- base::switch(options[["selectionType"]],
                          "recordSampling" = "records",
                          "musSampling" = "mus")
  }

  algorithm <- base::switch(options[["selectionMethod"]],
                            "randomSampling" = "random",
                            "cellSampling" = "cell",
                            "systematicSampling" = "interval")

  if(options[["rankingVariable"]] != ""){
    rankingColumn <- dataset[, .v(options[["rankingVariable"]])]
    dataset <- dataset[order(rankingColumn), ]
  }

  if(options[["monetaryVariable"]] != ""){
    bookValues <- .v(options[["monetaryVariable"]])
  } else {
    bookValues <- NULL
  }

  if(planningState[["sampleSize"]] == 0 || is.null(dataset))
    return()

  if(units == "records" && algorithm == "interval"){

    interval <- (nrow(dataset) / planningState[["sampleSize"]])
    if(options[["seed"]] > interval){
      selectionContainer$setError(gettext("Your specified starting point lies outside the selection interval."))
      return()
    }

  } else if (units == "mus" && algorithm == "interval"){

    interval <- (sum(dataset[, bookValues]) / planningState[["sampleSize"]])
    if(options[["seed"]] > interval){
      selectionContainer$setError("Your specified starting point lies outside the selection interval.")
      return()
    }

  }

  startingPointSeed <- options[["seed"]]
  if(!is.null(planningState[["startingPoint"]])){
    startingPointSeed <- planningState[["startingPoint"]]
  }

  sample <- jfa::sampling(population = dataset, 
                          sampleSize = planningState[["sampleSize"]], 
                          algorithm = algorithm, 
                          units = units, 
                          seed = options[["seed"]],
                          ordered = FALSE,
                          bookValues = bookValues,
                          intervalStartingPoint = startingPointSeed)                                

  sample <- data.frame(sample[["sample"]])
  sample[, 1:2] <- apply(X = sample[, 1:2], MARGIN = 2, as.numeric)

  return(sample)
}

.auditSelectionSummaryTable <- function(options, 
                                        planningOptions,
                                        planningState,
                                        selectionState,
                                        selectionContainer,
                                        jaspResults, 
                                        positionInContainer,
                                        dataset){

  .updateTabNumber(jaspResults)

  if(!is.null(selectionContainer[["selectionInformationTable"]])) 
    return()

  tableTitle <- gettextf("<b>Table %1$i.</b> Selection Summary", 
                         jaspResults[["tabNumber"]]$object)
  
  selectionInformationTable <- createJaspTable(tableTitle)
  selectionInformationTable$position <- positionInContainer
  selectionInformationTable$dependOn(options = c("bookValueDescriptives",
                                                 "sampleDescriptives",
                                                 "displaySample",
                                                 "samplingChecked",
                                                 "evaluationChecked"))
  
  selectionInformationTable$addColumnInfo(name = "size", 
                                          title = gettext("Selected sampling units"), 
                                          type = "integer")
  selectionInformationTable$addColumnInfo(name = "transactions", 
                                          title = gettext("Selected transactions"), 
                                          type = "integer")

  if(options[["selectionType"]] == "musSampling"){

    selectionInformationTable$addColumnInfo(name = "value", 
                                            title = gettext("Selection value"), 
                                            type = "string")
    selectionInformationTable$addColumnInfo(name = "percentage", 
                                            title = gettextf("%% of population value"),
                                            type = "string")  
  } else {

    selectionInformationTable$addColumnInfo(name = "percentage", 
                                            title = gettextf("%% of total observations"),
                                            type = "string")  

  }

  if(options[["selectionMethod"]] != "randomSampling")
    selectionInformationTable$addColumnInfo(name = "interval", 
                                            title ="Interval", 
                                            type = "string")
  
  if(options[["selectionMethod"]] != "systematicSampling"){
    message <- gettextf("The sample is drawn with random number generator <i>seed %1$s</i>.", options[["seed"]])
  } else {
    startingPointSeed <- options[["seed"]]
    if(!is.null(planningState[["startingPoint"]])){
      startingPointSeed <- planningState[["startingPoint"]]
    }
    message <- gettextf("Sampling unit %1$s is selected from each interval.", startingPointSeed)
  }

  selectionInformationTable$addFootnote(message)

  selectionContainer[["selectionInformationTable"]] <- selectionInformationTable

  if(is.null(selectionState))
    return()

  if(options[["selectionType"]] == "recordSampling" || 
      !is.null(selectionState[["musFailed"]])){

    interval <- (planningOptions[["populationSize"]] / planningState[["sampleSize"]])
  } else {
    interval <- (planningOptions[["populationValue"]] / planningState[["sampleSize"]])
  }

  sampleSize <- sum(selectionState[["count"]])

  if(options[["selectionType"]] == "musSampling"){

    value <- round(sum(abs(selectionState[, .v(options[["monetaryVariable"]])])), 2)
    percentage <- paste0(round(value / planningOptions[["populationValue"]] * 100, 2), "%")

    row  <- data.frame("size" = sampleSize,
						"transactions" = nrow(selectionState), 
                       "value" = paste(planningOptions[["valuta"]], value), 
                       "percentage" = percentage)

  } else {

    percentage <- paste0(round(sampleSize / planningOptions[["populationSize"]] * 100, 2), "%")
    row <- data.frame("size" = sampleSize, "transactions" = nrow(selectionState), "percentage" = percentage)
  
  }

  if(options[["selectionMethod"]] != "randomSampling"){
    if(options[["selectionType"]] == "musSampling" && is.null(selectionState[["musFailed"]])){
      row <- cbind(row, interval = paste(planningOptions[["valuta"]], round(interval, 2)))
    } else {
      row <- cbind(row, interval = round(interval, 2))
    }
  }

  selectionInformationTable$addRows(row)
}

.auditSelectionMonetaryIntervalTable <- function(options,
												planningOptions, 
												planningState,
												selectionContainer, 
												selectionState,  
												jaspResults,  
												positionInContainer,
												dataset){

	if(options[["selectionType"]] == "musSampling" && options[["selectionMethod"]] != "randomSampling"){

		.updateTabNumber(jaspResults)

		if(!is.null(selectionContainer[["stratumTable"]])) 
		return()

		tableTitle <- gettextf("<b>Table %1$i.</b> Information about Monetary Interval Selection", 
							jaspResults[["tabNumber"]]$object)
		
		stratumTable <- createJaspTable(tableTitle)
		stratumTable$position <- positionInContainer + 1
		stratumTable$dependOn(options = c("bookValueDescriptives",
										"sampleDescriptives",
										"displaySample",
										"samplingChecked",
										"evaluationChecked"))
		
		stratumTable$addColumnInfo(name = "stratum", 
								title = "", 
								type = "string")   
		stratumTable$addColumnInfo(name = "size", 
								title = gettext("Transactions"), 
								type = "integer")  
		stratumTable$addColumnInfo(name = "value", 
								title = gettext("Total value"), 
								type = "string")    
		stratumTable$addColumnInfo(name = "unitSize", 
								title = gettext("Selected units"), 
								type = "integer")                            
		stratumTable$addColumnInfo(name = "sampleSize", 
								title = gettext("Selected transactions"), 
								type = "integer") 
		stratumTable$addColumnInfo(name = "sampleValue", 
								title = gettext("Selection value"), 
								type = "string")  
		stratumTable$addColumnInfo(name = "stratumPercentage", 
								title = gettext("% of total value"), 
								type = "string")                               
		selectionContainer[["stratumTable"]] <- stratumTable

		sampleSize <- planningState[["sampleSize"]]

		if(sampleSize == 0){
			row <- data.frame(stratum = c("Total population", "Ist value > Interval", "Ist value < Interval"),
							 	size = c(nrow(dataset), ".", "."),
								value = c(paste(planningOptions[["valuta"]], round(planningOptions[["populationValue"]], 2)),".", "."),
								unitSize = rep(".", 3),
								sampleSize = rep(".", 3),
								sampleValue = rep(".", 3),
								stratumPercentage = rep(".", 3))
			stratumTable$setData(row)
			return()
		}

		interval <- round(planningOptions[["populationValue"]] / planningState[["sampleSize"]], 2)

		topStratum <- dataset[which(dataset[, .v(options[["monetaryVariable"]])] > interval), ]
		bottomStratum <- dataset[which(dataset[, .v(options[["monetaryVariable"]])] <= interval), ]
		bottomStratumSample <- selectionState[which(selectionState[, .v(options[["monetaryVariable"]])] <= interval), ]

		topStratumValue <- round(sum(topStratum[, .v(options[["monetaryVariable"]])]), 2)
		bottomStratumValuePopulation <- round(sum(bottomStratum[, .v(options[["monetaryVariable"]])]), 2)
		bottomStratumValueSample <- round(sum(bottomStratumSample[, .v(options[["monetaryVariable"]])]), 2)

		row <- data.frame(stratum = c("Total population", "Ist value > Interval", "Ist value < Interval"), 
						size = c(nrow(dataset), nrow(topStratum), nrow(dataset) - nrow(topStratum)), 
						value = c(paste(planningOptions[["valuta"]], round(planningOptions[["populationValue"]], 2)), paste(planningOptions[["valuta"]], topStratumValue), paste(planningOptions[["valuta"]], bottomStratumValuePopulation)),
						unitSize = c(sampleSize, sampleSize - nrow(bottomStratumSample), nrow(bottomStratumSample)),
						sampleSize = c(nrow(topStratum) + nrow(bottomStratumSample), nrow(topStratum), nrow(bottomStratumSample)), 
						sampleValue = c(paste(planningOptions[["valuta"]], bottomStratumValueSample + topStratumValue), paste(planningOptions[["valuta"]], topStratumValue), paste(planningOptions[["valuta"]], bottomStratumValueSample)),
						stratumPercentage = c(paste0(round((bottomStratumValueSample + topStratumValue) / planningOptions[["populationValue"]] * 100, 2), "%"), "100%", paste0(round(bottomStratumValueSample / bottomStratumValuePopulation * 100, 2), "%")))
		stratumTable$setData(row)
	}
}

.auditSelectionSampleTable <- function(options,
                                       selectionState,
                                       selectionContainer,
                                       jaspResults,
                                       positionInContainer,
                                       planningOptions){

  if(!options[["displaySample"]]) 
    return()

  .updateTabNumber(jaspResults)

  if(is.null(selectionContainer[["selectionSampleTable"]])){

    tableTitle <- gettextf("<b>Table %1$i.</b> Selected Transactions",
                           jaspResults[["tabNumber"]]$object)
    
    sampleTable <- createJaspTable(tableTitle)
    sampleTable$position <- positionInContainer
    sampleTable$dependOn(options = c("bookValueDescriptives",
                                    "sampleDescriptives",
                                    "displaySample",
                                    "samplingChecked",
                                    "evaluationChecked"))

    recordNumberVariable  <- .auditReadVariableFromOptions(options, varType = "recordNumber")
    monetaryVariable      <- .auditReadVariableFromOptions(options, varType = "monetary")
    rankingVariable       <- .auditReadVariableFromOptions(options, varType = "ranking")
    additionalVariables   <- .auditReadVariableFromOptions(options, varType = "additional")
    columnNames           <- c("Row number", "Count", recordNumberVariable, monetaryVariable, rankingVariable, additionalVariables)

    for(i in columnNames){

      sampleTable$addColumnInfo(name = i,     
                                type = "string",
                                title = i)

    }

    selectionContainer[["sampleTable"]] <- sampleTable

    if(is.null(selectionState) || selectionContainer$getError())
        return()
    
    dat <- as.data.frame(selectionState)
    colnames(dat) <- columnNames

    sampleTable$setData(dat)
  }
}

.auditSelectionDescriptivesTable <- function(options, 
                                             selectionState, 
                                             selectionContainer,
                                             jaspResults,
                                             positionInContainer){

  if(!options[["sampleDescriptives"]]) 
    return()
  
  .updateTabNumber(jaspResults)

  if(is.null(selectionContainer[["sampleDescriptivesTable"]])){

    recordVariable <- options[["recordNumberVariable"]]
    if(recordVariable == "")        
      recordVariable <- NULL
    rankingVariable <- options[["rankingVariable"]]
    if(rankingVariable == "")        
      rankingVariable <- NULL
    monetaryVariable <- unlist(options[["monetaryVariable"]])
    if(monetaryVariable == "")      
      monetaryVariable <- NULL

    variables <- unlist(options[["additionalVariables"]])

    all.variables <- c(rankingVariable, 
                        monetaryVariable, 
                        variables)

    tableTitle <- gettextf("<b>Table %1$i.</b> Descriptive Statistics for Selected Transactions",
                           jaspResults[["tabNumber"]]$object)
    
    sampleDescriptivesTable <- createJaspTable(tableTitle)
    sampleDescriptivesTable$transpose <- TRUE
    sampleDescriptivesTable$position <- positionInContainer

    sampleDescriptivesTable$dependOn(options = c("sampleDescriptives", 
                                                  "mean", 
                                                  "sd", 
                                                  "var", 
                                                  "range", 
                                                  "min", 
                                                  "max", 
                                                  "median",
                                                  "bookValueDescriptives",
                                                  "sampleDescriptives",
                                                  "displaySample",
                                                  "samplingChecked",
                                                  "evaluationChecked"))

    sampleDescriptivesTable$addColumnInfo(name = "name",                        
                                          type = "string", 
                                          title = "")
    sampleDescriptivesTable$addColumnInfo(name = gettext("Valid cases"),                 
                                          type = "integer")
    if (options[["mean"]])               
      sampleDescriptivesTable$addColumnInfo(name = "Mean",
                                            type = "number",
                                            title = gettext("Mean"))
    if (options[["median"]])             
      sampleDescriptivesTable$addColumnInfo(name = "Median",
                                            type = "number",
                                            title = gettext("Median"))
    if (options[["sd"]])                 
      sampleDescriptivesTable$addColumnInfo(name = "Std. Deviation",
                                            type = "number", 
                                            title = gettext("Std. Deviation"))
    if (options[["var"]])                
      sampleDescriptivesTable$addColumnInfo(name = "Variance",                    
                                            type = "number", 
                                            title = gettext("Variance"))
    if (options[["range"]])              
      sampleDescriptivesTable$addColumnInfo(name = "Range", 
                                            type = "number", 
                                            title = gettext("Range"))
    if (options[["min"]])                
      sampleDescriptivesTable$addColumnInfo(name = "Minimum", 
                                            type = "number",
                                            title = gettext("Minimum"))
    if (options[["max"]])                
      sampleDescriptivesTable$addColumnInfo(name = "Maximum", 
                                            type = "number",
                                            title = gettext("Maximum"))

    selectionContainer[["sampleDescriptivesTable"]]   <- sampleDescriptivesTable

    if(is.null(selectionState) ||  
      selectionContainer$getError())
        return()

    for (variable in all.variables) {

      column    <- selectionState[[ .v(variable) ]]
      row <- list()

      row[["name"]]                   <- variable
      row[["Valid cases"]]            <- base::length(column)

      if(!is.factor(column))
      {
      if(options[["mean"]])              row[["Mean"]]                   <- base::mean(column, na.rm = TRUE)
      if(options[["sd"]])                row[["Std. Deviation"]]         <- stats::sd(column, na.rm = TRUE)
      if(options[["var"]])               row[["Variance"]]               <- stats::var(column, na.rm = TRUE)
      if(options[["median"]])            row[["Median"]]                 <- stats::median(column, na.rm = TRUE)
      if(options[["range"]])             row[["Range"]]                  <- base::abs(base::range(column, na.rm = TRUE)[1] - base::range(column, na.rm = TRUE)[2])
      if(options[["min"]])               row[["Minimum"]]                <- base::min(column, na.rm = TRUE)
      if(options[["max"]])               row[["Maximum"]]                <- base::max(column, na.rm = TRUE)
      }
      sampleDescriptivesTable$addRows(row)
    }
  }
}

.auditSelectionHistograms <- function(options,
                                      dataset,
                                      selectionState,
                                      selectionContainer,
                                      jaspResults,
                                      positionInContainer){

  if (!is.null(selectionContainer[["plotHistograms"]]) || !options[["plotHistograms"]]) return()

  .updateFigNumber(jaspResults)

	plotHistograms <- createJaspContainer(gettext("Population and sample histograms"))
  plotHistograms$dependOn(options = c("recordNumberVariable",
                                      "monetaryVariable",
                                      "additionalVariables",
                                      "rankingVariable",
                                      "selectionMethod",
                                      "selectionType",
                                      "seed",
                                      "intervalStartingPoint",
                                      "sampleSize",
                                      "plotHistograms"))
  plotHistograms$position <- positionInContainer

  selectionContainer[["plotHistograms"]] <- plotHistograms

  if(options[["recordNumberVariable"]] == "" || 
      options[["monetaryVariable"]] == "" || 
      options[["sampleSize"]] == 0)
    return()

  variables <- colnames(selectionState)[-(1:3)]

  for(i in 1:length(variables)){

    if(i == 1){
      popData <- dataset[, .v(options[["monetaryVariable"]])]
    } else {
      popData <- dataset[, variables[i]]
    }

    sampleData <- selectionState[, variables[i]]

    if(!is.numeric(popData) || !is.numeric(sampleData)){
      next
    }
  
    plotData <- data.frame(x = c(popData, sampleData), 
                          type = c(rep("Population", length(popData)), 
                                   rep("Sample", length(sampleData))))
  
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(popData, min.n = 4)
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(hist(popData, plot = FALSE, breaks = 50)$counts, min.n = 4)

    p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, fill = type)) +
            ggplot2::geom_histogram(bins = 50, col = "black", position = "identity") +
            ggplot2::labs(fill = "") +
            ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = c(min(xBreaks), max(xBreaks))) +
            ggplot2::scale_y_continuous(name = "Frequency", breaks = yBreaks, limits = c(min(yBreaks), max(yBreaks))) +
            ggplot2::scale_fill_manual(values = c("#0063B2FF", "#9CC3D5FF"))

    p <- JASPgraphs::themeJasp(p, legend.position = "top")

    if(i == 1){
      plotHistograms[[variables[i]]] <- createJaspPlot(plot = p, title = "Book values", height = 300, width = 500)
      plotHistograms[[variables[i]]]$dependOn(optionContainsValue = list("monetaryVariable" = variables[i]))
    } else{
      plotHistograms[[variables[i]]] <- createJaspPlot(plot = p, title = options[["additionalVariables"]][i - 1], height = 300, width = 500)
      plotHistograms[[variables[i]]]$dependOn(optionContainsValue = list("additionalVariables" = variables[i]))
    }
  }

  explanatoryText <- TRUE # Will be added as an option later
  if(explanatoryText){

    histogramPlotText <- createJaspHtml(gettextf("<b>Figure %1$i.</b> The distributions of numeric variables in the population compared to the distributions in the sample.", jaspResults[["figNumber"]]$object), "p")
    
    histogramPlotText$position <- positionInContainer + 1
    histogramPlotText$dependOn(optionsFromObject = selectionContainer[["plotHistograms"]])
    histogramPlotText$dependOn(options = "explanatoryText")
    selectionContainer[["histogramPlotText"]] <- histogramPlotText
  }
}

################################################################################
################## Common functions for the evaluation #########################
################################################################################

.auditAddEvaluationColumns <- function(options, 
                                       jaspResults){

  dataset <- .auditAddSelectionColumns(options, 
                                       jaspResults)

  sampleFilter <- options[["sampleFilter"]]
  auditResult <- options[["auditResult"]]
  critical <- options[["criticalTransactions"]]

  variables <- c(sampleFilter, auditResult, critical)

  if(!("" %in% variables)){

    additionalColumns <- .readDataSetToEnd(columns.as.numeric = variables)
    dataset <- cbind(dataset, additionalColumns)
    return(dataset)

  } else {

    return(dataset)

  }
}

.auditEvaluationState <- function(options, 
                                  planningOptions,
                                  sample, 
                                  evaluationContainer, 
                                  selectionState){

  if(options[["auditResult"]] == "")
    return()
                                  
  if(!is.null(evaluationContainer[["evaluationState"]])){

    return(evaluationContainer[["evaluationState"]]$object)

  } else {

	  # Add critical transactions to the sample
	  sample <- .auditAddCriticalTransactions(options, sample)

    auditRisk <- 1 - options[["confidence"]]
    prior <- NULL
    
    materiality <- NULL
    if(options[["performanceMateriality"]])
      materiality <- planningOptions[["materiality"]]

    if(options[["priorConstructionMethod"]] == "arm"){

      inherentRisk <- base::switch(options[["IR"]],
                                    "High" = 1,
                                    "Medium" = 0.60,
                                    "Low" = 0.50,
                                    "Custom" = options[["irCustom"]])

      controlRisk <- base::switch(options[["CR"]],
                                    "High" = 1,
                                    "Medium" = 0.60,
                                    "Low" = 0.50,
                                    "Custom" = options[["crCustom"]])

      detectionRisk <- auditRisk / inherentRisk / controlRisk
      confidence <- 1 - detectionRisk
    }

    if(options[["bayesianAnalysis"]]){

      prior <- jfa::auditPrior(materiality = materiality, 
                        confidence = planningOptions[["confidence"]],
                        expectedError = planningOptions[["expectedErrors"]], 
                        likelihood = planningOptions[["likelihood"]], 
                        N = planningOptions[["populationSize"]], 
                        ir = inherentRisk, 
                        cr = controlRisk,
                        method = options[["priorConstructionMethod"]],
                        sampleN = options[["sampleN"]],
                        sampleK = options[["sampleK"]],
                        factor = options[["factor"]],
                        pHplus = 1 - options[["pHmin"]],
                        pHmin = options[["pHmin"]])
                        
      confidence <- options[["confidence"]]
      
      if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != ""){

		  result <- .auditUvbEvaluation(options, sample, prior, 
		  								planningOptions, selectionState, evaluationContainer)
    
        return(result)
      }
    }

    # Select evaluation method
    if(options[["variableType"]] == "variableTypeCorrect"){

      method <- options[["planningModel"]]
      if(method == "Poisson")
        method <- "poisson"

      result <- try({
  
        # call jfa evaluation
        jfa::evaluation(sample = sample,
                        counts = selectionState[["count"]],
                        confidence = confidence,
                        nSumstats = nrow(sample),
                        kSumstats = length(which(sample[, .v(options[["auditResult"]])] == 1)),
                        method = method,
                        materiality = materiality,
                        N = planningOptions[["populationSize"]],
                        prior = prior)

      })

    } else if(options[["variableType"]] == "variableTypeAuditValues"){

      method <- base::switch(options[["estimator"]],
                              "stringerBound"     = "stringer",
                              "regressionBound"   = "regression",
                              "directBound"       = "direct",
                              "differenceBound"   = "difference",
                              "ratioBound"        = "quotient",
                              "coxAndSnellBound"  = "coxsnell",
                              "betaBound"         = "binomial",
                              "gammaBound"        = "poisson",
                              "betabinomialBound" = "hypergeometric")

      if(method == "stringer" && options[["stringerBoundLtaAdjustment"]])
        method <- "stringer-lta"

      # Adjust the confidence since jfa only returns a confidence interval
      if(method %in% c("direct", "difference", "quotient", "regression")){
        confidence <- confidence + ((1 - confidence) / 2)
      }

      # Bayesian regression is not implemented in jfa R package (will become WASEM)
      if(options[["bayesianAnalysis"]] && method == "regression"){

        result <- try({
      
          .auditBayesianRegression(sample, 
                                   confidence,
                                   options,
                                   planningOptions)

        })

      } else {

        result <- try({
        
          # call jfa evaluation
          jfa::evaluation(sample = sample,
                          counts = selectionState[["count"]],
                          confidence = confidence,
                          bookValues = .v(options[["monetaryVariable"]]),
                          auditValues = .v(options[["auditResult"]]),
                          method = method,
                          materiality = materiality,
                          N = planningOptions[["populationSize"]],
                          populationBookValue = planningOptions[["populationValue"]],
                          prior = prior)

        })

      }
    }

    if(isTryError(result)){

      evaluationContainer$setError(paste0("An error occurred: ", JASP:::.extractErrorMessage(result)))
      return()
    }

    if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){

      result[["confBound"]] <- (planningOptions[["populationValue"]] - result[["lowerBound"]]) / planningOptions[["populationValue"]]
                               
      if(result[["confBound"]] < planningOptions[["materiality"]]){
        result[["conclusion"]] <- "Approve population"
      } else {
        result[["conclusion"]] <- "Do not approve population"
      }
    }

    evaluationContainer[["evaluationState"]] <- createJaspState(result)
    
    return(result)
  }
}

.auditExplanatoryTextEvaluation <- function(options,
                                            planningOptions,
                                            planningState,
                                            selectionState,
                                            evaluationContainer, 
                                            positionInContainer = 1){

  if(options[["explanatoryText"]]){

    if(options[["variableType"]] == "variableTypeCorrect" && !options[["useSumStats"]])
      ready <- options[["auditResult"]] != "" && 
                options[["recordNumberVariable"]] != "" && 
                (planningOptions[["materiality"]] != 0 || options[["minimumPrecisionPercentage"]] != 0)
    if(options[["variableType"]] == "variableTypeAuditValues" && !options[["useSumStats"]])
      ready <- options[["auditResult"]] != "" && 
                options[["recordNumberVariable"]] != "" &&
                options[["monetaryVariable"]] != "" && 
                (planningOptions[["materiality"]] != 0 || options[["minimumPrecisionPercentage"]] != 0)

    if(options[["variableType"]] == "variableTypeCorrect" && options[["useSumStats"]])
      ready <- options[["nSumStats"]] > 0 && (planningOptions[["materiality"]] != 0 || options[["minimumPrecisionPercentage"]] != 0)

    if(ready){

      evaluationState <- evaluationContainer[["evaluationState"]]$object

      if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){

        confidenceBound <- (planningOptions[["populationValue"]] - evaluationState[["lowerBound"]]) / planningOptions[["populationValue"]]

      } else {

        confidenceBound <- evaluationState[["confBound"]]

      }

      errorLabel <- round(evaluationState[["t"]], 2)

      if(options[["display"]] == "displayNumbers"){
        boundLabel <- round(confidenceBound, 3)
        mleLabel <- round(evaluationState[["mle"]], 3)
        precisionLabel <- round(evaluationState[["precision"]], 3)
      } else if(options[["display"]] == "displayPercentages"){
        boundLabel <- paste0(round(confidenceBound * 100, 3), "%")
        mleLabel <- paste0(round(evaluationState[["mle"]] * 100, 3), "%")
        precisionLabel <- paste0(round(evaluationState[["precision"]] * 100, 3), "%")
      } else if(options[["display"]] == "displayValues"){
        boundLabel <- paste(planningOptions[["valuta"]], round(confidenceBound * planningOptions[["populationValue"]], 2))
        mleLabel <- paste(planningOptions[["valuta"]], round(evaluationState[["mle"]] * planningOptions[["populationValue"]], 2))
        precisionLabel <- paste(planningOptions[["valuta"]], round(evaluationState[["precision"]] * planningOptions[["populationValue"]], 2))
      }

    } else {

      boundLabel <- errorLabel <- mleLabel <- precisionLabel <- "..."

    }

    if(sum(selectionState[["count"]]) > nrow(selectionState)){
      sampleSizeMessage <- paste0(planningState[["sampleSize"]], 
                                  " (",
                                  nrow(selectionState),
                                  " + ",
                                  sum(selectionState[["count"]][which(selectionState[["count"]] != 1)] - 1),
                                  ")")
    } else {
      sampleSizeMessage <- planningState[["sampleSize"]]
    }

    if(!options[["bayesianAnalysis"]]){
      additionalMessage <- gettextf("\n\nThe cumulative knowledge states that there is a %1$s probability that, when one would repeatedly sample from this population, the upper bound on the misstatement in the population is lower than <b>%2$s</b> with a precision of <b>%3$s</b>.", 
                                    planningOptions[["confidenceLabel"]],
                                    boundLabel,
                                    precisionLabel)
    } else if(options[["bayesianAnalysis"]]){
      additionalMessage <- gettextf("\n\nThe cumulative knowledge states that there is a %1$s probability that the misstatement in the population is lower than <b>%2$s</b> with a precision of <b>%3$s</b>.", 
                                    planningOptions[["confidenceLabel"]],
                                    boundLabel,
                                    precisionLabel)
    }

    message <- gettextf("The selection consisted of <b>%1$s</b> sampling units, of which a total of <b>%2$s</b> were misstated. The information from this sample combined with the prior information results in a most likely error in the population of <b>%3$s</b> and an %4$s upper bound of <b>%5$s</b>. %6$s",
                        sampleSizeMessage,
                        errorLabel,
                        mleLabel,
                        planningOptions[["confidenceLabel"]],
                        boundLabel,
                        additionalMessage)

    evaluationContainer[["evaluationParagraph"]] <- createJaspHtml(message, "p")
    evaluationContainer[["evaluationParagraph"]]$position <- positionInContainer
    evaluationContainer[["evaluationParagraph"]]$dependOn(options = "explanatoryText")
  }
}

.auditBackwardsPlanningState <- function(options, 
                                         dataset, 
                                         evaluationOptions){

  if(evaluationOptions[["materiality"]] != 0 && 
      ((options[["variableType"]] == "variableTypeAuditValues" && options[["recordNumberVariable"]] != "" && options[["monetaryVariable"]] != "" && options[["auditResult"]] != "") || 
      (options[["variableType"]] == "variableTypeCorrect" && options[["recordNumberVariable"]] != "" && options[["auditResult"]] != "") || 
      (options[["variableType"]] == "variableTypeCorrect" && options[["useSumStats"]] && options[["nSumStats"]] > 0))){

    if(!options[["bayesianAnalysis"]]){

      planningState <- list()
      planningState[["sampleSize"]] <- ifelse(options[["useSumStats"]], 
                                              yes = options[["nSumStats"]],
                                              no = nrow(dataset))
      return(planningState)

    } else if(options[["bayesianAnalysis"]]){

      inherentRisk <- base::switch(options[["IR"]],
                                    "High" = 1,
                                    "Medium" = 0.60,
                                    "Low" = 0.50,
                                    "Custom" = options[["irCustom"]])

      controlRisk <- base::switch(options[["CR"]],
                                    "High" = 1,
                                    "Medium" = 0.60,
                                    "Low" = 0.50,
                                    "Custom" = options[["crCustom"]])

      minPrecision <- NULL
      if(options[["minimumPrecision"]])
        minPrecision <- options[["minimumPrecisionPercentage"]]

      performanceMateriality <- NULL
      if(options[["performanceMateriality"]])
        performanceMateriality <- evaluationOptions[["materiality"]]

      prior <- jfa::auditPrior(materiality = performanceMateriality, 
                    confidence = evaluationOptions[["confidence"]],
                    expectedError = evaluationOptions[["expectedErrors"]], 
                    likelihood = evaluationOptions[["likelihood"]], 
                    N = evaluationOptions[["populationSize"]], 
                    ir = inherentRisk, 
                    cr = controlRisk,
                    method = options[["priorConstructionMethod"]],
                    sampleN = options[["sampleN"]],
                    sampleK = options[["sampleK"]],
                    factor = options[["factor"]],
                    pHplus = 1 - options[["pHmin"]],
                    pHmin = options[["pHmin"]])

        if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != "" && options[["auditResult"]] != ""){

          # Plan a sample for the efficiency technique Separate known and unknown misstatement
          for(n in seq(5, nrow(dataset), by = 1)){
            
              interval <- (evaluationOptions[["populationValue"]] / n) 
              topStratum <- subset(dataset, dataset[, .v(options[["monetaryVariable"]])] > interval)
              bottomStratum <- subset(dataset, dataset[, .v(options[["monetaryVariable"]])] <= interval)

              m_seen <- sum(topStratum[, .v(options[["monetaryVariable"]])]) 
              set.seed(rnorm(1) + evaluationOptions[["populationValue"]])
              intervalStartingPoint <- sample(1:(interval - 1), size = 1)

              intervalSelection <- intervalStartingPoint + 0:(n - 1) * interval
              index <- NULL
              for(i in 1:n){
                index <- c(index, which(intervalSelection[i] < cumsum(dataset[, .v(options[["monetaryVariable"]])]))[1])
              }
              sample <- dataset[index, ]
              sample <- unique(sample)

              bottomStratumSample <- sample[which(sample[, .v(options[["monetaryVariable"]])] <= interval), ]

              m_seen <- m_seen + sum(bottomStratumSample[, .v(options[["monetaryVariable"]])])
              m_seen_percentage <- m_seen / evaluationOptions[["populationValue"]]

              m_unseen <- evaluationOptions[["populationValue"]] - m_seen

              if(options[["expectedErrors"]] == "expectedAllPossible"){
                a <- prior$aPrior + 0:n
                b <- prior$bPrior + n - 0:n
              } else if(options[["expectedErrors"]] == "expectedRelative"){
                a <- prior$aPrior + 0:ceiling(n * (evaluationOptions[["expectedErrors"]] * 2))
                b <- prior$bPrior + n - 0:ceiling(n * (evaluationOptions[["expectedErrors"]] * 2))
              } else if(options[["expectedErrors"]] == "expectedAbsolute"){
                a <- prior$aPrior + 0:ceiling(evaluationOptions[["expectedErrors"]] * 2)
                b <- prior$bPrior + n - 0:ceiling(evaluationOptions[["expectedErrors"]] * 2)
              }

              v95 <- qbeta(options[["confidence"]], a, b)
              v <- ((a - 1) / (a + b - 2))
              relativeInaccuracy <- v95 - v
              correctedInaccuracy <- options[["minimumPrecisionPercentage"]] * (1 / (1 - m_seen_percentage))
              diff <- relativeInaccuracy - correctedInaccuracy
              
              # Sampling objectives
              if(options[["minimumPrecision"]] && !options[["performanceMateriality"]]){
                if(all(diff <= 0))
                  break
              } else if(!options[["minimumPrecision"]] && options[["performanceMateriality"]]){
                if(all(v95 < (evaluationOptions[["materiality"]] / (1 - (m_seen / evaluationOptions[["populationValue"]])))))
                  break
              } else if(options[["minimumPrecision"]] && options[["performanceMateriality"]]){
                if(all(diff <= 0) && all(v95 < (evaluationOptions[["materiality"]] / (1 - (m_seen / evaluationOptions[["populationValue"]])))))
                  break
              }
            }

            expErrors <- ceiling(n * (evaluationOptions[["expectedErrors"]] * 2))

            result <- list(sampleSize = n, 
                 confidence = options[["confidence"]],
                 materiality = evaluationOptions[["materiality"]], 
                 adjustedMateriality = (evaluationOptions[["materiality"]] / (1 - (m_seen / evaluationOptions[["populationValue"]]))),
                 N = evaluationOptions[["populationSize"]], 
                 expectedSampleError = expErrors,
                 likelihood = "binomial",
                 prior = list(aPrior = prior$aPrior, 
                              bPrior = prior$bPrior, 
                              nPrior = prior$nPrior, 
                              kPrior = prior$kPrior),
                 startingPoint = intervalStartingPoint)

            return(result)
        }

      planningState <- jfa::planning(materiality = evaluationOptions[["materiality"]], 
                                    confidence = evaluationOptions[["confidence"]], 
                                    expectedError = evaluationOptions[["expectedErrors"]], 
                                    N = evaluationOptions[["populationSize"]], 
                                    minPrecision = minPrecision,
                                    prior = prior,
                                    increase = 1)

      planningState[["sampleSize"]] <- ifelse(options[["useSumStats"]], 
                                              yes = options[["nSumStats"]],
                                              no = nrow(dataset))

      return(planningState)

    }

  } else {

    planningState <- list()
    planningState[["sampleSize"]] <- "..."
    return(planningState)

  }
}

.auditEvaluationAnalysisState <- function(options,
                                          sample,
                                          planningOptions,
                                          evaluationContainer){

  if(options[["variableType"]] == "variableTypeCorrect" && !options[["useSumStats"]] &&
    (options[["auditResult"]] == "" || options[["recordNumberVariable"]] == "")){
      return()
  } else if(options[["variableType"]] == "variableTypeAuditValues" && !options[["useSumStats"]] &&
            (options[["auditResult"]] == "" || options[["recordNumberVariable"]] == "" || options[["monetaryVariable"]] == "")){
      return()
  }

  if(options[["performanceMateriality"]] && planningOptions[["materiality"]] == 0)
    return()

  if(options[["useSumStats"]] && options[["nSumStats"]] == 0)
    return()
                                    
    if(!is.null(evaluationContainer[["evaluationState"]])){

      return(evaluationContainer[["evaluationState"]]$object)

    } else {

      auditRisk <- 1 - options[["confidence"]]

      inherentRisk <- base::switch(options[["IR"]],
                                    "High" = 1,
                                    "Medium" = 0.60,
                                    "Low" = 0.50,
                                    "Custom" = options[["irCustom"]])

      controlRisk <- base::switch(options[["CR"]],
                                    "High" = 1,
                                    "Medium" = 0.60,
                                    "Low" = 0.50,
                                    "Custom" = options[["crCustom"]])

        minPrecision <- NULL
        if(options[["minimumPrecision"]])
          minPrecision <- options[["minimumPrecisionPercentage"]]

        performanceMateriality <- NULL
        if(options[["performanceMateriality"]])
          performanceMateriality <- planningOptions[["materiality"]]

		N <- planningOptions[["populationSize"]]
		if(options[["workflow"]]){
			if(options[["monetaryVariable"]] == "")
				N <- ceiling(planningOptions[["populationValue"]])
		} else {
			if(options[["populationValue"]] != 0)
				N <- ceiling(planningOptions[["populationValue"]])
		}

      if(!options[["bayesianAnalysis"]]){

        detectionRisk <- (1 - options[["confidence"]]) / inherentRisk / controlRisk
        confidence <- 1 - detectionRisk
        prior <- FALSE

      } else if(options[["bayesianAnalysis"]]){

        confidence <- options[["confidence"]]

        prior <- jfa::auditPrior(materiality = performanceMateriality, 
                      confidence = planningOptions[["confidence"]],
                      expectedError = planningOptions[["expectedErrors"]], 
                      likelihood = planningOptions[["likelihood"]], 
                      N = N, 
                      ir = inherentRisk, 
                      cr = controlRisk,
                      method = options[["priorConstructionMethod"]],
                      sampleN = options[["sampleN"]],
                      sampleK = options[["sampleK"]],
                      factor = options[["factor"]],
                      pHplus = 1 - options[["pHmin"]],
                      pHmin = options[["pHmin"]])

      }

    # Select evaluation method
    if(options[["variableType"]] == "variableTypeCorrect"){

      if(!options[["bayesianAnalysis"]]){
        method <- base::switch(options[["estimator2"]],
                        "binomialBound" = "binomial",
                        "poissonBound" = "poisson",
                        "hyperBound" = "hypergeometric")
      } else if(options[["bayesianAnalysis"]]){
        method <- base::switch(options[["estimator"]],
                        "betaBound" = "binomial",
                        "gammaBound" = "poisson",
                        "betabinomialBound" = "hypergeometric")
      }

      if(!options[["useSumStats"]]){
        nSumstats <- nrow(sample)
        kSumstats <- length(which(sample[, .v(options[["auditResult"]])] == 1))
      } else {
        nSumstats <- options[["nSumStats"]]
        kSumstats <- options[["kSumStats"]]
      }

      result <- try({
  
        # call jfa evaluation
        jfa::evaluation(sample = sample,
                        confidence = confidence,
                        nSumstats = nSumstats,
                        kSumstats = kSumstats,
                        method = method,
                        materiality = planningOptions[["materiality"]],
                        N = planningOptions[["populationSize"]],
                        prior = prior)

      })

    } else if(options[["variableType"]] == "variableTypeAuditValues"){

      method <- base::switch(options[["estimator"]],
                              "stringerBound"     = "stringer",
                              "regressionBound"   = "regression",
                              "directBound"       = "direct",
                              "differenceBound"   = "difference",
                              "ratioBound"        = "quotient",
                              "coxAndSnellBound"  = "coxsnell",
                              "betaBound"         = "binomial",
                              "gammaBound"        = "poisson",
                              "betabinomialBound" = "hypergeometric")

      if(method == "stringer" && options[["stringerBoundLtaAdjustment"]])
        method <- "stringer-lta"

      # Adjust the confidence since jfa only returns a confidence interval
      if(method %in% c("direct", "difference", "quotient", "regression")){
        confidence <- confidence + ((1 - confidence) / 2)
      }

      # Bayesian regression is not implemented in jfa R package
      if(options[["bayesianAnalysis"]] && method == "regression"){

        result <- try({
      
          .auditBayesianRegression(sample, 
                                   confidence,
                                   options,
                                   planningOptions)

        })

      } else {

        if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != "" && options[["auditResult"]] != ""){

			result <- .auditUvbEvaluation(options, sample, prior, 
								planningOptions, selectionState, evaluationContainer)
      
          return(result)
        }

        result <- try({
          
          counter <- NULL
          if(options[["sampleCounter"]] != "")
            counter <- sample[, .v(options[["sampleCounter"]])]
          # call jfa evaluation
          jfa::evaluation(sample = sample,
                          counts = counter,
                          confidence = confidence,
                          bookValues = .v(options[["monetaryVariable"]]),
                          auditValues = .v(options[["auditResult"]]),
                          method = method,
                          materiality = performanceMateriality,
                          minPrecision = minPrecision,
                          N = planningOptions[["populationSize"]],
                          populationBookValue = planningOptions[["populationValue"]],
                          prior = prior)

        })

      }
    }

    if(isTryError(result)){

      evaluationContainer$setError(paste0("An error occurred: ", 
                                        JASP:::.extractErrorMessage(result)))
      return()
    }

    if(options[["variableType"]] == "variableTypeAuditValues" && 
        options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){

      result[["confBound"]] <- (planningOptions[["populationValue"]] - result[["lowerBound"]]) / 
                               planningOptions[["populationValue"]]
                               
      if(result[["confBound"]] < planningOptions[["materiality"]]){
        result[["conclusion"]] <- "Approve population"
      } else {
        result[["conclusion"]] <- "Do not approve population"
      }
    }

    evaluationContainer[["evaluationState"]] <- createJaspState(result)
    
    return(result)
  }
}

.auditEvaluationSummaryTable <- function(options,
                                         planningOptions,
                                         evaluationState,
                                         evaluationContainer,
                                         jaspResults,
                                         positionInContainer){

  .updateTabNumber(jaspResults)

  if(!is.null(evaluationContainer[["evaluationTable"]])) 
    return()

  tableTitle <- gettextf("<b>Table %1$i.</b> Evaluation Summary",
                         jaspResults[["tabNumber"]]$object)
  
  evaluationTable <- createJaspTable(tableTitle)
  evaluationTable$position  <- positionInContainer
  evaluationTable$dependOn(options = c("bookValueDescriptives",
                                        "sampleDescriptives",
                                        "displaySample",
                                        "samplingChecked",
                                        "evaluationChecked",
                                        "auditResult",
                                        "evidenceRatio",
                                        "bayesFactor",
                                        "valuta",
                                        "otherValutaName",
                                        "mostLikelyError",
                                        "IR",
                                        "irCustom",
                                        "CR",
                                        "crCustom",
                                        "obtainedPrecision",
                                        "display"))

  if(options[["performanceMateriality"]])
    evaluationTable$addColumnInfo(name = 'materiality',   
                                  title = gettext("Performance materiality"),
                                  type = 'string')

  if(options[["minimumPrecision"]])
    evaluationTable$addColumnInfo(name = 'minPrecision',   
                                  title = gettext("Required precision"),
                                  type = 'string')

  evaluationTable$addColumnInfo(name = 'sampleSize', 
                                title = gettext("Sample size"), 
                                type = 'integer')
  evaluationTable$addColumnInfo(name = 'fullErrors',  
                                title = gettext("Errors"), 
                                type = 'integer')
  evaluationTable$addColumnInfo(name = 'totalTaint',             
                                title = gettext("Total tainting"),
                                type = 'string')

  if(options[["mostLikelyError"]])
    evaluationTable$addColumnInfo(name = 'mle',         
                                  title = gettext("Most likely error"), 
                                  type = 'string')

  if(!options[["bayesianAnalysis"]]){

    auditRisk <- 1 - options[["confidence"]]
    detectionRisk <- .auditCalculateDetectionRisk(options)

    boundTitle <- gettextf("%1$s%% Upper bound", round((1 - detectionRisk) * 100, 2))

    evaluationTable$addColumnInfo(name = 'bound',         
                              title = boundTitle, 
                              type = 'string')

  } else if(options[["bayesianAnalysis"]]){

    if(options[["areaUnderPosterior"]] == "displayCredibleBound"){

      boundTitle <- paste0(options[["confidence"]] * 100,"% Upper bound")
      evaluationTable$addColumnInfo(name = 'bound',         
                              title = boundTitle, 
                              type = 'string')

    } else if (options[["areaUnderPosterior"]] == "displayCredibleInterval"){

      boundTitle <- paste0(options[["confidence"]] * 100,"% Credible interval")
      evaluationTable$addColumnInfo(name = 'lowerBound',  
                              title = gettext("Lower"),       
                              overtitle = boundTitle, 
                              type = 'string')
       evaluationTable$addColumnInfo(name = 'upperBound',  
                              title = gettext("Upper"),       
                              overtitle = boundTitle, 
                              type = 'string')                                                         
    }
  }

  if(options[["obtainedPrecision"]])
    evaluationTable$addColumnInfo(name = 'precision',         
                                  title = gettext("Obtained precision"), 
                                  type = 'string')

  if(options[["bayesianAnalysis"]] && options[["performanceMateriality"]] && options[["evidenceRatio"]])
    evaluationTable$addColumnInfo(name = 'evidenceRatio',
                                  title = gettext("Evidence ratio"),     
                                  type = 'number')
  if(options[["bayesianAnalysis"]] && options[["performanceMateriality"]] && options[["bayesFactor"]])
    evaluationTable$addColumnInfo(name = 'bayesFactor',
                                  title = gettextf("BF%1$s", "\u208B\u208A"),     
                                  type = 'number')

  criterion <- options[["estimator"]]
  if(!options[["workflow"]] && options[["variableType"]] == "variableTypeCorrect" && !options[["bayesianAnalysis"]])
    criterion <- options[["estimator2"]]

  message <- base::switch(criterion,
                          "poissonBound" = gettext("The upper bound is calculated according to the <b>Poisson</b> distributon."),
                          "binomialBound" = gettext("The upper bound is calculated according to the <b>binomial</b> distributon."),
                          "hyperBound" = gettext("The upper bound is calculated according to the <b>hypergeometric</b> distribution."),
                          "stringerBound" = gettext("The upper bound is calculated according to the <b>Stringer</b> method."),
                          "regressionBound" = gettext("The upper bound is calculated according to the <b>regression</b> method."),
                          "directBound" = gettext("The upper bound is calculated according to the <b>direct</b> method."),
                          "differenceBound" = gettext("The upper bound is calculated according to the <b>difference</b> method."),
                          "ratioBound" = gettext("The upper bound is calculated according to the <b>ratio</b> method."),
                          "betaBound" = gettext("The upper bound is calculated according to the <b>beta</b> distribution and requires the assumption that the sample taints are interchangeable."),
                          "gammaBound" = gettext("The upper bound is calculated according to the <b>gamma</b> distribution and requires the assumption that the sample taints are interchangeable."),
                          "betabinomialBound" = gettext("The upper bound is calculated according to the <b>beta-binomial</b> distribution and requires the assumption that the sample taints are interchangeable."),
                          "coxAndSnellBound" = gettext("The upper bound is calculated according to the <b>Cox and Snell</b> method and requires the assumption that the population taints are uniformly distributed."))

  if(options[["estimator"]] == "stringerBound" && options[["stringerBoundLtaAdjustment"]] && options[["variableType"]] == "variableTypeAuditValues")
    message <- gettext("The upper bound is calculated according to the <b>Stringer</b> method with <b>LTA adjustment</b>.")

  if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != "")
    message <- gettext("The upper bound is calculated according to the <b>beta</b> distribution. It requires the assumptions that the sample taints are interchangeable and that the sample taints are homogeneous.")

  evaluationTable$addFootnote(message)

  evaluationContainer[["evaluationTable"]] <- evaluationTable

  if(is.null(evaluationState) || (options[["auditResult"]] == "" && !options[["useSumStats"]])){

    if(options[["workflow"]]){
      evaluationTable$addFootnote(message = gettext("The audit result column is empty."), 
                          symbol = gettextf("%1$s <b>Results cound not be calculated.</b>", "\u26A0"))
    } else {
      evaluationTable$addFootnote(message = gettext("Either the materiality, the population size, or the population value is defined as zero, or one of the required variables is missing."), 
                          symbol = gettextf("%1$s <b>Results could not be calculated.</b>", "\u26A0"))
    }
    return()
  }

  taintLabel <- base::switch(options[["display"]],
                            "displayNumbers" = round(evaluationState[["t"]], 3),
                            "displayPercentages" = paste0(round(evaluationState[["t"]] / evaluationState[["n"]] * 100, 3), "%"),
                            "displayValues" = paste(planningOptions[["valuta"]], round(evaluationState[["t"]], 2)))

  if(options[["bayesianAnalysis"]] && options[["areaUnderPosterior"]] == "displayCredibleInterval"){

    credibleInterval <- .auditCalculateCredibleInterval(options, evaluationState)
    lowerBound <- credibleInterval[["lowerBound"]]
    upperBound <- credibleInterval[["upperBound"]]
    
    LowerBoundLabel <- base::switch(options[["display"]],
                                    "displayNumbers" = round(lowerBound, 3),
                                    "displayPercentages" = paste0(round(lowerBound * 100, 3), "%"),
                                    "displayValues" = paste(planningOptions[["valuta"]], round(lowerBound * planningOptions[["populationValue"]], 2)))
    UpperBoundLabel <- base::switch(options[["display"]],
                                    "displayNumbers" = round(upperBound, 3),
                                    "displayPercentages" = paste0(round(upperBound * 100, 3), "%"),
                                    "displayValues" = paste(planningOptions[["valuta"]], round(upperBound * planningOptions[["populationValue"]], 2)))

    row <- data.frame(sampleSize = evaluationState[["n"]],
                      fullErrors = evaluationState[["k"]],
                      totalTaint = taintLabel,
                      lowerBound = LowerBoundLabel,
                      upperBound = UpperBoundLabel)    

  } else {

    boundLabel <- base::switch(options[["display"]],
                               "displayNumbers" = round(evaluationState[["confBound"]], 3),
                               "displayPercentages" = paste0(round(evaluationState[["confBound"]] * 100, 3), "%"),
                               "displayValues" = paste(planningOptions[["valuta"]], round(evaluationState[["confBound"]] * planningOptions[["populationValue"]], 2)))

    row <- data.frame(sampleSize = evaluationState[["n"]],
                      fullErrors = evaluationState[["k"]],
                      totalTaint = taintLabel,
                      bound = boundLabel)

  }

  if(options[["performanceMateriality"]]){
    materiality <- base::switch(options[["display"]],
                                "displayNumbers" = round(planningOptions[["materiality"]], 3),
                                "displayPercentages" = paste0(round(planningOptions[["materiality"]] * 100, 3), "%"),
                                "displayValues" = paste(planningOptions[["valuta"]], round(planningOptions[["materiality"]] * planningOptions[["populationValue"]], 2)))
    row <- cbind(row, materiality = materiality)
  }

  if(options[["minimumPrecision"]]){
    minPrecision <- base::switch(options[["display"]],
                                 "displayNumbers" = round(planningOptions[["minimumPrecision"]], 3),
                                 "displayPercentages" = paste0(round(planningOptions[["minimumPrecision"]] * 100, 3), "%"),
                                 "displayValues" = paste(planningOptions[["valuta"]], round(planningOptions[["minimumPrecision"]] * planningOptions[["populationValue"]], 2)))
    row <- cbind(row, minPrecision = minPrecision)
  }

  if(options[["mostLikelyError"]]){
    mleLabel <- base::switch(options[["display"]],
                             "displayNumbers" = round(evaluationState[["mle"]], 3),
                             "displayPercentages" = paste0(round(evaluationState[["mle"]] * 100, 3), "%"),
                             "displayValues" = paste(planningOptions[["valuta"]], round(evaluationState[["mle"]] * planningOptions[["populationValue"]], 2)))
    row <- cbind(row, mle = mleLabel)
  }

  if(options[["obtainedPrecision"]]){
    precisionLabel <- base::switch(options[["display"]],
                                   "displayNumbers" = round(evaluationState[["precision"]], 3),
                                   "displayPercentages" = paste0(round(evaluationState[["precision"]] * 100, 3), "%"),
                                   "displayValues" = paste(planningOptions[["valuta"]], round(evaluationState[["precision"]] * planningOptions[["populationValue"]], 2)))
    row <- cbind(row, precision = precisionLabel)
  }

  if(options[["bayesianAnalysis"]] && options[["performanceMateriality"]] && (options[["evidenceRatio"]] || options[["bayesFactor"]])){

    expResult <- .auditEvidenceRatio(planningOptions, evaluationState)

    if(options[["evidenceRatio"]])
      row <- cbind(row, evidenceRatio = expResult[["posteriorEvidenceRatio"]])

    if(options[["bayesFactor"]])
      row <- cbind(row, bayesFactor = expResult[["shift"]])

  }

  evaluationTable$addRows(row)
}

.auditEvaluationAssumptionChecks <- function(options,
                                             sample,
                                             evaluationContainer,
                                             jaspResults,
                                             positionInContainer = 3){

  if(!options[["evaluationAssumptionChecks"]] || !options[["separateKnownAndUnknownMisstatement"]] || options[["monetaryVariable"]] == "") 
    return()

  .updateTabNumber(jaspResults)

  tableTitle <- gettextf("<b>Table %1$i.</b> Assumption Checks",
                         jaspResults[["tabNumber"]]$object)
  
  assumptionTable <- createJaspTable(tableTitle)
  assumptionTable$position  <- positionInContainer
  assumptionTable$dependOn(options = c("evaluationAssumptionChecks"))

  assumptionTable$addColumnInfo(name = 'type',   
                                title = "",
                                type = 'string')
  assumptionTable$addColumnInfo(name = 'n',   
                                title = "n",
                                type = 'integer')
  assumptionTable$addColumnInfo(name = 'correlation', 
                                title = gettext("Pearson's <i>r</i>"), 
                                type = 'number')
  assumptionTable$addColumnInfo(name = 'lowerCI', 
                                title = gettext("Lower"), 
                                type = 'number',
                                overtitle = gettextf("%1$s%% Confidence interval", options[["evaluationAssumptionChecksConfidence"]] * 100))
  assumptionTable$addColumnInfo(name = 'upperCI', 
                                title = gettext("Upper"), 
                                type = 'number',
                                overtitle = gettextf("%1$s%% Confidence interval", options[["evaluationAssumptionChecksConfidence"]] * 100))
  assumptionTable$addColumnInfo(name = 'pvalue', 
                                title = gettext("p"), 
                                type = 'pvalue')
   assumptionTable$addColumnInfo(name = 'bayesfactor', 
                                title = gettextf("BF%1$s", "\u2081\u2080"), 
                                type = 'number')  

  # assumptionTable$addFootnote(gettextf("<i>p</i> < .05 or %1$s > 19 implies that an assumption might be violated.", "BF\u2081\u2080"), symbol = gettext("<i>Note.</i>"))

  evaluationContainer[["assumptionTable"]] <- assumptionTable

  if(options[["auditResult"]] == ""){
        row <- list(type = gettext("The sample taints are homogeneous"))
    assumptionTable$addRows(row)
    return()
  }

  bookValues 	<- sample[, .v(options[["monetaryVariable"]])]
  auditValues 	<- sample[, .v(options[["auditResult"]])]
  taints 		<- (bookValues - auditValues) / bookValues
  
  # Enable this to only do the taints that are non-zero
  # bookValues <- bookValues[taints != 0]
  # taints <- taints[taints != 0]

#   if(all(taints == 0)){
#       row <- list(type = gettext("The sample taints are homogeneous"), 
#                   n = length(taints),
#                   correlation = 1,
#                   lowerCI = "-",
#                   upperCI = "-",
#                   pvalue = "-",
#                   bayesfactor = "-")
#     assumptionTable$addRows(row)
#     return()
#   }
  
  corTest   <- cor.test(bookValues, taints, alternative = "two.sided", 
                        method = "pearson", conf.level = options[["evaluationAssumptionChecksConfidence"]])
  estimate  <- corTest[["estimate"]]
  lowerCI   <- as.numeric(corTest[["conf.int"]])[1]
  upperCI   <- as.numeric(corTest[["conf.int"]])[2]
  pval      <- corTest[["p.value"]]
  bf10 		<- try({ 1 / .audit_jzs_corbf(r = estimate, n = nrow(sample)) })

  if(isTryError(bf10)){
	  bf10 <- NA
	  assumptionTable$addFootnote("An error occurred while calculating the Bayes factor", symbol = "<b>Warning.</b>")
  }

  row <- list(type = gettext("The sample taints are homogeneous"), 
              n = length(taints),
              correlation = estimate,
              lowerCI = lowerCI,
              upperCI = upperCI, 
              pvalue = pval,
              bayesfactor = bf10)

  assumptionTable$addRows(row)                       
}

.auditEvaluationInformationPlot <- function(options,
                                            planningOptions,
                                            evaluationState,
                                            evaluationContainer,
                                            jaspResults,
                                            positionInContainer = 3){

  if(!options[["evaluationInformation"]]) 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(evaluationContainer[["evaluationInformation"]])){

    evaluationInformation <- createJaspPlot(plot = NULL, 
                                            title = gettext("Evaluation of Sampling Objectives"), 
                                            width = 600, 
                                            height = 300)
    evaluationInformation$position <- positionInContainer
    evaluationInformation$dependOn(options = c("evaluationInformation", 
                                                "display"))

    evaluationContainer[["evaluationInformation"]] <- evaluationInformation

    if(((options[["auditResult"]] == "" || options[["recordNumberVariable"]] == "") && !options[["useSumStats"]]) ||
        (options[["useSumStats"]] && options[["nSumStats"]] == 0) || 
        (planningOptions[["materiality"]] == 0 && options[["performanceMateriality"]]) ||
        evaluationContainer$getError()) 
      return()

    materiality <- evaluationState[["materiality"]]
    bound <- evaluationState[["confBound"]]
    mle <- evaluationState[["mle"]]
    precision <- evaluationState[["precision"]]
    minPrecision <- options[["minimumPrecisionPercentage"]]
    
    if(options[["variableType"]] == "variableTypeAuditValues" && 
        options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){
      
      mle <- (planningOptions[["populationValue"]] - evaluationState[["pointEstimate"]]) / 
              planningOptions[["populationValue"]]
    
    } 

    objectiveColor <- "orange"
    boundColor <- ifelse(bound < materiality, yes = rgb(0, 1, .7, 1), no = rgb(1, 0, 0, 1))
    precisionColor <- ifelse(precision < minPrecision, yes = rgb(0, 1, .7, 1), no = rgb(1, 0, 0, 1))
  
    if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
      label <- rev(c(gettext("Performance materiality"), gettext("Maximum error"), gettext("Most likely error")))
      values <- rev(c(materiality, bound, mle))
      fillUp <- rev(c(objectiveColor, boundColor, "#1380A1"))
    } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
      label <- rev(c(gettext("Required precision"), gettext("Obtained precision"), gettext("Maximum error"), gettext("Most likely error")))
      values <- rev(c(minPrecision, precision, bound, mle))
      fillUp <- rev(c(objectiveColor, precisionColor, "#1380A1", "#1380A1"))
    } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
      label <- rev(c(gettext("Required precision"), gettext("Obtained precision"), gettext("Performance materiality"), gettext("Maximum error"), gettext("Most likely error")))
      values <- rev(c(minPrecision, precision, materiality, bound, mle))
      fillUp <- rev(c(objectiveColor, precisionColor, objectiveColor, boundColor, "#1380A1"))
    }
    
    if(options[["display"]] == "displayValues")
      values <- values * planningOptions[["populationValue"]]
  
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, values), min.n = 4)
    
    if(options[["display"]] == "displayValues"){

      x.labels <- format(JASPgraphs::getPrettyAxisBreaks(
                          seq(0, 1.1 * max(values), length.out = 100), 
                          min.n = 4), 
                        scientific = FALSE)
      values.labels <- paste(planningOptions[["valuta"]], ceiling(values))

    } else {

      x.labels <- paste0(round(JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1 * max(values), length.out = 100), min.n = 4) * 100, 4), "%")
      values.labels <- paste0(round(values * 100, 2), "%")

    }

    plotData <- data.frame(x = label, 
                          y = values)
    plotData$x <- factor(plotData$x, levels = plotData$x)

    yLimits <- c(0, 1.1 * max(values))
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1 * max(values), length.out = 100), 
                                            min.n = 4)

    if(mle < 0 || bound < 0){
      # Here we adjust the axes if the mle turns out to be negative
      yBreaks <- JASPgraphs::getPrettyAxisBreaks(seq(min(values), 1.1 * max(values), length.out = 100), 
                                                min.n = 4)
      x.labels <- format(JASPgraphs::getPrettyAxisBreaks(seq(min(values), 1.1 * max(values), length.out = 100), 
                          min.n = 4), scientific = FALSE)
      yLimits <- c(min(values), 1.1 * max(values))
    }

    p <- ggplot2::ggplot(data = plotData, 
                        mapping = ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_bar(stat = "identity", 
                          col = "black", 
                          size = 1, 
                          fill = fillUp) +
        ggplot2::coord_flip() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        ggplot2::annotate("text", 
                          y = values, 
                          x = 1:length(values), 
                          label = values.labels, 
                          size = 6, 
                          vjust = 0.5, 
                          hjust = -0.3) + 
        ggplot2::scale_y_continuous(breaks = yBreaks, 
                                    limits = yLimits, 
                                    labels = x.labels)
    
    myTheme <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), 
                              axis.ticks.y = ggplot2::element_blank(), 
                              axis.text.y = ggplot2::element_text(hjust = 0),
                              panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb", size = 0.5))

    p <- JASPgraphs::themeJasp(p, 
                               sides = "") + myTheme

    evaluationInformation$plotObject <- p

  }

  if(options[["explanatoryText"]]){

    evaluationInformationText <- createJaspHtml(gettextf("<b>Figure %1$i.</b> Evaluation information for the current annotated selection. The materiality is compared with the maximum misstatement and the most likely error. The most likely error (MLE) is an estimate of the true misstatement in the population. The maximum error is an estimate of the maximum error in the population.",
                                                          jaspResults[["figNumber"]]$object), "p")
    
    evaluationInformationText$position <- positionInContainer + 1
    evaluationInformationText$dependOn(optionsFromObject = evaluationContainer[["evaluationInformation"]])
    evaluationInformationText$dependOn(options = "explanatoryText")
    evaluationContainer[["evaluationInformationText"]] <- evaluationInformationText 
  }
}

.auditCorrelationPlotAddLine <- function(fit, 
                                         plot = NULL, 
                                         line = FALSE, 
                                         xMin, 
                                         xMax, 
                                         lwd) {
  # create function formula
  f <- vector("character", 0)
  for (i in seq_along(coef(fit))) {
    if (i == 1) {
      temp <- paste(coef(fit)[[i]])
      f <- paste(f, temp, sep="")
    }

    if (i > 1) {
      temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep="")
      f <- paste(f, temp, sep="+")
    }
  }

  x <- seq(xMin, xMax, length.out = 100)
  predY <- eval(parse(text=f))

  if (line == FALSE) {
    return(predY)
  }

  if (line) {
    plot <- plot + ggplot2::geom_line(data = data.frame(x, predY),
                                      mapping = ggplot2::aes(x = x, y = predY), 
                                      size=lwd, 
                                      lty = 1)
    return(plot)
  }
}

.auditCorrelationPlot <- function(options,
                                  planningOptions,
                                  sample,
                                  evaluationContainer,
                                  jaspResults,
                                  positionInContainer){

  if(!options[["correlationPlot"]]) 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(evaluationContainer[["correlationPlot"]])){

    correlationPlot <- createJaspPlot(plot = NULL, title = gettext("Scatter Plot of Ist and Soll Values"), 
                                      width = 500, height = 400)

    correlationPlot$position <- positionInContainer
    correlationPlot$dependOn(options = c("correlationPlot", 
                                        "valuta",
                                        "correlationPlotShowIds",
                                        "correlationPlotShowCorrelation"))

    evaluationContainer[["correlationPlot"]] <- correlationPlot

    if(options[["auditResult"]] == "" || evaluationContainer$getError()) 
      return()

    plotData <- data.frame(ist = sample[,.v(options[["monetaryVariable"]])], 
                            soll = sample[,.v(options[["auditResult"]])])
    plotData <- na.omit(plotData) # Just to be sure

    ist <- plotData[["ist"]]
    soll <- plotData[["soll"]]
    corResult <- cor(x = ist, y = soll, method = "pearson")

    fit <- vector("list", 1)
    fit[[1]] <- lm(soll ~ poly(ist, 1, raw = TRUE), data = plotData)
    bestModel <- 1 # which.min(Bic)

    # format ticks and labels
    ticks       <- pretty(c(ist, soll), min.n = 4)
    minTicks    <- min(ticks)
    maxTicks    <- max(ticks)
    labs        <- format(ticks, digits = 3, scientific = FALSE)

    corResult <- round(corResult, 3)

    cols <- rep("gray", nrow(plotData))
    cols[which(plotData$ist != plotData$soll)] <- rgb(0.9, 0, 0, 1)

    p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = ist, y = soll)) +
          ggplot2::geom_line(data = data.frame(x = c(minTicks, maxTicks), y = c(minTicks, maxTicks)), 
                              mapping = ggplot2::aes(x = x, y = y), size = 0.35, linetype = "dashed") +
          ggplot2::scale_x_continuous(name = gettextf("Ist value (%1$s)", planningOptions[["valuta"]]),
                                      breaks = ticks, labels = labs, limits = range(ticks)) +
          ggplot2::scale_y_continuous(name = gettextf("Soll value (%1$s)", planningOptions[["valuta"]]),
                                      breaks = ticks, labels = labs, limits = range(ticks)) + 
          JASPgraphs::geom_point(size = 3, fill = cols)

    if(options[["correlationPlotShowCorrelation"]]){

      p <- .auditCorrelationPlotAddLine(fit = fit[[bestModel]], 
                                        plot = p, 
                                        line = TRUE, 
                                        xMin = minTicks, 
                                        xMax = maxTicks, 
                                        lwd = 1)

      p <- p + ggplot2::annotate("text", 
                                x = ticks[1], 
                                y = (ticks[length(ticks)] - ((ticks[length(ticks)] - ticks[length(ticks) - 1]) / 2)),
                                label = paste0("italic(r) == ", corResult), 
                                size = 8, 
                                parse = TRUE, 
                                hjust = -0.5, 
                                vjust = 0.5)

    }

    if(options[["correlationPlotShowIds"]])
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(label=sample[, .v(options[["recordNumberVariable"]])], x = ist, y = soll), hjust=-1, vjust=1, data = plotData)

    myTheme <- ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb", size = 0.5), 
                            panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", size = 0.5))
    
    p <- JASPgraphs::themeJasp(p) + myTheme

    correlationPlot$plotObject <- p

  }

  if(options[["explanatoryText"]]){

    correLationPlotText <- createJaspHtml(gettextf("<b>Figure %1$i.</b> Scatter plot of the Ist values in the selection and their corresponding Soll values. Grey dots on the diagonal (dashed line) indicate matching Ist and Soll values. Red dots off the diagonal indicate transactions whose Soll value did not match their original Ist value. If these red dots lie below the diagonal, the transactions are overstated. If these red dots lie above the diagonal they are understated. %2$s",
                                          jaspResults[["figNumber"]]$object,
                                          ifelse(options[["correlationPlotShowCorrelation"]], yes = "The value <i>r</i> is the Pearson correlation coefficient of the Ist values and the Soll values, an indicator of the strength of the linear relationship (solid line) between the two.", no = "")), "p")
    
    correLationPlotText$position <- positionInContainer + 1
    correLationPlotText$dependOn(optionsFromObject = evaluationContainer[["correlationPlot"]])
    correLationPlotText$dependOn(options = "explanatoryText")
    evaluationContainer[["correLationPlotText"]] <- correLationPlotText
  }
}

################################################################################
################## Common functions for the UVB methods ########################
################################################################################

.auditUvbPlanning <- function(options, planningOptions, dataset, prior, N){

	# Plan a sample for the efficiency technique Separate known and unknown misstatement
	for(n in seq(5, nrow(dataset), by = options[["sampleSizeIncrease"]])){
	
		interval <- (planningOptions[["populationValue"]] / n) 
		topStratum <- subset(dataset, dataset[, .v(options[["monetaryVariable"]])] > interval)
		bottomStratum <- subset(dataset, dataset[, .v(options[["monetaryVariable"]])] <= interval)

		m_seen <- sum(topStratum[, .v(options[["monetaryVariable"]])]) 
		set.seed(rnorm(1) + options[["sampleSizeIncrease"]] + planningOptions[["populationValue"]])
		intervalStartingPoint <- sample(1:(interval - 1), size = 1)

		intervalSelection <- intervalStartingPoint + 0:(n - 1) * interval
		index <- NULL
		for(i in 1:n){
		index <- c(index, which(intervalSelection[i] < cumsum(dataset[, .v(options[["monetaryVariable"]])]))[1])
		}
		sample <- dataset[index, ]
		sample <- unique(sample)

		bottomStratumSample <- sample[which(sample[, .v(options[["monetaryVariable"]])] <= interval), ]

		m_seen <- m_seen + sum(bottomStratumSample[, .v(options[["monetaryVariable"]])])
		m_seen_percentage <- m_seen / planningOptions[["populationValue"]]

		m_unseen <- planningOptions[["populationValue"]] - m_seen

		if(options[["expectedErrors"]] == "expectedAllPossible"){
			a <- prior$aPrior + 0:n
			b <- prior$bPrior + n - 0:n
		} else if(options[["expectedErrors"]] == "expectedRelative"){
			a <- prior$aPrior + 0:ceiling(n * (planningOptions[["expectedErrors"]] * 2))
			b <- prior$bPrior + n - 0:ceiling(n * (planningOptions[["expectedErrors"]] * 2))
		} else if(options[["expectedErrors"]] == "expectedAbsolute"){
			a <- prior$aPrior + 0:ceiling(planningOptions[["expectedErrors"]] * 2)
			b <- prior$bPrior + n - 0:ceiling(planningOptions[["expectedErrors"]] * 2)
		}

		v95 <- qbeta(options[["confidence"]], a, b)
		v <- ((a - 1) / (a + b - 2))
		relativeInaccuracy <- v95 - v
		correctedInaccuracy <- options[["minimumPrecisionPercentage"]] * (1 / (1 - m_seen_percentage))
		diff <- relativeInaccuracy - correctedInaccuracy
		
		# Sampling objectives
		if(options[["minimumPrecision"]] && !options[["performanceMateriality"]]){
		if(all(diff <= 0))
			break
		} else if(!options[["minimumPrecision"]] && options[["performanceMateriality"]]){
		if(all(v95 < (planningOptions[["materiality"]] / (1 - (m_seen / planningOptions[["populationValue"]])))))
			break
		} else if(options[["minimumPrecision"]] && options[["performanceMateriality"]]){
		if(all(diff <= 0) && all(v95 < (planningOptions[["materiality"]] / (1 - (m_seen / planningOptions[["populationValue"]])))))
			break
		}
	}

	expErrors <- ceiling(n * (planningOptions[["expectedErrors"]] * 2))

	result <- list(sampleSize = n, 
					confidence = options[["confidence"]],
					materiality = planningOptions[["materiality"]], 
					adjustedMateriality = (planningOptions[["materiality"]] / (1 - (m_seen / planningOptions[["populationValue"]]))),
					N = planningOptions[["populationSize"]], 
					expectedSampleError = expErrors,
					likelihood = "binomial",
					prior = list(aPrior = prior$aPrior, 
								bPrior = prior$bPrior, 
								nPrior = prior$nPrior, 
								kPrior = prior$kPrior),
					startingPoint = intervalStartingPoint)

	return(result)
}

.auditUvbEvaluation <- function(options, 
								sample, 
								prior,
								planningOptions, 
								selectionState, 
								evaluationContainer){

	k <- length(which(sample[, .v(options[["monetaryVariable"]])] != sample[, .v(options[["auditResult"]])]))
	if(options[["workflow"]]){
		n <- sum(selectionState[["count"]])
	} else{
		if(options[["sampleCounter"]] == ""){
            n <- nrow(sample)
        } else {
            n <- sum(sample[, .v(options[["sampleCounter"]])])
        }
	}

	overstatements <- (sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])])
	unseen_value <- planningOptions[["populationValue"]] - sum(sample[, .v(options[["monetaryVariable"]])])
	
	taintings <- overstatements / sample[, .v(options[["monetaryVariable"]])]
	if(options[["workflow"]]){
		totalTaint <- sum(taintings * selectionState[["count"]])
	} else {
		if(options[["sampleCounter"]] == ""){
            totalTaint <- sum(taintings)
        } else {
            totalTaint <- sum(taintings * sample[, .v(options[["sampleCounter"]])])
        }
	}
	avgTaint <- totalTaint / n 
	posteriorMode <- (prior$aPrior + totalTaint - 1) / (prior$aPrior + totalTaint + prior$bPrior + n - totalTaint - 2) 

	# Find out the total error in the critital transactions (if needed)
	if(options[["workflow"]] && options[["flagCriticalTransactions"]] && options[["handleCriticalTransactions"]] == "inspect"){
		criticalTransactions <- subset(sample, sample[, .v(options[["criticalTransactions"]])] > 0)
		if(nrow(criticalTransactions) == 0){
		Vk <- 0
		} else {
		Vk <- abs(sum(criticalTransactions[, .v(options[["monetaryVariable"]])] - criticalTransactions[, .v(options[["auditResult"]])])) # Absolute value
		}
		sample <- subset(sample, sample[, .v(options[["criticalTransactions"]])] == 0)
	} else if(!options[["workflow"]] && options[["flagCriticalTransactions"]] && options[["handleCriticalTransactions"]] == "inspect"){
		criticalTransactions <- subset(sample, sample[, .v(options[["monetaryVariable"]])] < 0)
		Vk <- abs(sum(criticalTransactions[, .v(options[["monetaryVariable"]])] - criticalTransactions[, .v(options[["auditResult"]])])) # Absolute value
	} else {
		Vk <- 0
	}

	Vs <- sum(overstatements)               # The total error in the sample (known error)
	Vt <- posteriorMode * unseen_value      # The total error in the unseen observations (unknown error)
	Vt95 <- qbeta(options[["confidence"]], shape1 = prior$aPrior + totalTaint, shape2 = prior$bPrior + n - totalTaint) * unseen_value # The upper bound on the total error in the unseen observations 

	# The inferred total error and upper bound for the population
	V <- Vk + Vs + Vt
	VAsFraction <- V / planningOptions[["populationValue"]]

	V95 <- Vk + Vs + Vt95  
	V95AsFraction <- V95 / planningOptions[["populationValue"]]               

	# The total obtained precision
	precisionAsFraction <- (V95 - V) / planningOptions[["populationValue"]]

	result <- list(confBound = V95AsFraction,
					confBoundUnseen = Vt95 / unseen_value,
					precision = precisionAsFraction,
					precisionUnseen = qbeta(options[["confidence"]], shape1 = prior$aPrior + totalTaint, shape2 = prior$bPrior + n - totalTaint) - posteriorMode,
					unseenValue = unseen_value,
					k = k,
					t = totalTaint,
					n = n,
					kPrior = prior$kPrior,
					nPrior = prior$nPrior,
					confidence = options[["confidence"]],
					method = "binomial",
					likelihood = "binomial",
					mle = VAsFraction,
					mleUnseen = posteriorMode,
					materiality = planningOptions[["materiality"]],
					populationValue = planningOptions[["populationValue"]],
					adjustedMateriality = (planningOptions[["materiality"]] / (1 - (sum(sample[, .v(options[["monetaryVariable"]])]) / planningOptions[["populationValue"]]))))

	evaluationContainer[["evaluationState"]] <- createJaspState(result)

	return(result)
}

################################################################################
################## End functions ###############################################
################################################################################
