#' @title Settings for association rule mining
#' 
#' @param parameters An object of class APparameter for apriori (or ECparameter for eclat) or a named list. Available slots
#'- APparameter: confidence, minval, smax, arem, aval, originalSupport, maxtime, support, minlen, maxlen, target, ext. 
#' - ECparameter: tidLists, support, minlen, maxlen, target, ext 
#' @param control An object of class APcontrol for apriori (or ECcontrol for ecalt) or named list. Available slots 
#' - APcontrol: filter, tree, heap, memopt, load, sort, verbose 
#' - ECcontrol: sparse, sort, verbose
#' @param removeLengthOnePatterns Whether to remove patterns (rules) with 1 item.
#' @param trasnactionsPlpData A plpData object to convert to transactions for mining
#' @param savePatterns Whether to save mined patterns (rules) or not. Default to FALSE.
#' 
#' @export
createAssociationRulesSettings <- function(parameters, 
                                           control, 
                                           removeLengthOnePatterns = FALSE, 
                                           transactionsPlpData, 
                                           savePatterns = FALSE){
  
  miningFPsSettings <- list(parameters, 
                            control,
                            removeLengthOnePatterns = removeLengthOnePatterns,
                            transactionsPlpData = transactionsPlpData, 
                            savePatterns = savePatterns
  )
  
  class(miningFPsSettings) <- "associationRuleMiningSettings"
  return(miningFPsSettings) 
}

#' @export
extractAssociationRulesSettings <- function(associationRulesMiningSettings, plpDataSettings, outputFolder = getwd(), fileName){
  #add checks
  
  featureEngineeringSettings <- list(
    parameters = associationRulesMiningSettings$parameters,
    control = associationRulesMiningSettings$control,
    transactionsPlpData = associationRulesMiningSettings$transactionsPlpData,
    removeLengthOnePatterns = associationRulesMiningSettings$removeLengthOnePatterns,
    savePatterns = associationRulesMiningSettings$savePatterns,
    plpDataSettings = plpDataSettings,
    outputFolder = outputFolder, 
    fileName = fileName
  )
  
  # specify the function that will implement the sampling
  attr(featureEngineeringSettings, "fun") <- "extractAssociationRules"
  
  # make sure the object returned is of class "sampleSettings"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
  
}

#' @export
extractAssociationRules <- function(trainData, featureEngineeringSettings, covariateIdsInclude = NULL){
  # frequent pattern mining settings
  parameters = featureEngineeringSettings$parameters
  control = featureEngineeringSettings$control
  outputFolder = featureEngineeringSettings$outputFolder
  fileName = featureEngineeringSettings$fileName
  removeLengthOnePatterns = featureEngineeringSettings$removeLengthOnePatterns
  transactionsPlpData = featureEngineeringSettings$transactionsPlpData
  savePatterns = featureEngineeringSettings$savePatterns
  
  # featureExtraction settings
  #covariateSettingsSequence <- featureEngineeringSettings$temporalSequenceFeatureExtractionSettings
  databaseDetails <- featureEngineeringSettings$plpDataSettings$databaseDetails
  restrictPlpDataSettings <- featureEngineeringSettings$plpDataSettings$restrictPlpDataSettings
  dirLocation <- file.path(outputFolder)
  
  if (!dir.exists(dirLocation)){
    dir.create(dirLocation)
  }
  
  if (is.null(covariateIdsInclude) == TRUE){
    
    trainDataRowId <- trainData$labels$rowId
    
    # sequencePlpData <- PatientLevelPrediction::getPlpData(
    #   databaseDetails = databaseDetails,
    #   covariateSettings = covariateSettingsSequence,
    #   restrictPlpDataSettings = restrictPlpDataSettings
    # )
    
    covariateData <- Andromeda::copyAndromeda(transactionsPlpData$covariateData)
    
    covariateData$covariates <- covariateData$covariates %>%
      filter(rowId %in% trainDataRowId)
    ParallelLogger::logTrace("\nPreparing data for Frequent Pattern mining...")
    start <- Sys.time()
    if (file.exists(file.path(dirLocation, paste0(fileName, "transactions.Rds"))) == FALSE){
      inputData <- AssociationRuleMining::getInputFileForAssociationRules(covariateData, 
                                                                fileToSave = file.path(dirLocation, paste0(fileName, ".txt")))
      
      saveRDS(inputData, file = file.path(dirLocation, paste0(fileName, "transactions.Rds")))
      
      } else {
        inputData <- readRDS(file = file.path(dirLocation, paste0(fileName, "transactions.Rds")))
      }
      
    inputData <- inputData %>% 
      group_by(rowId) %>% 
      arrange(rowId) %>%
      ungroup() %>% 
      as.data.frame(.data)
    
      transactions <- as(split(inputData[,"covariateLabel"], inputData[,"rowId"]), "transactions")
  
    
    delta <- Sys.time() - start
    ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
    s0 <- arules::apriori(data = transactions, 
                          parameter = parameters, 
                          control = control)
    
    tidLists <- arules::supportingTransactions(s0, transactions = transactions)
    
    if (savePatterns){
      saveRDS(s0, file.path(dirLocation, paste0(fileName, "_Ars_train.Rds")))
    }
    
    initialFPs <- as.numeric(length(s0))
    ParallelLogger::logInfo(paste("The set of ARs extracted were", initialFPs, "."))
    
    if (removeLengthOnePatterns == TRUE){
      s0 <- s0[size(s0) > 1]
      remainingFPs <- as.numeric(length(s0))
      ParallelLogger::logInfo(paste("After removing length one ARs there were", remainingFPs, "remaining."))
    }
    
    if(length(s0)== 0){
      cov <- trainData
      ParallelLogger::logInfo("AR mining returned 0 ARs therefore returning trainData.")
    } else {
      transactionsRowId <- unique(transactionInfo(transactions)$transactionID)
      
      cov <- AssociationRuleMining::addAssociationRulesToAndromeda(plpDataObject = trainData, 
                                                                   fileWithFPs = s0,
                                                                   objectWithIds = inputData,
                                                                   tidLists = tidLists, 
                                                                   transactionsRowId = transactionsRowId,
                                                                   fileToSave = file.path(dirLocation, fileName))
    }
    #drop(covariateData)
    covariateIdsInclude <- list(trainPatterns = s0, 
                                trainCovariateRef = cov$covariateData$covariateRef)
  } else {
    
    # Which of the FPs to extract
    # covariateDataTest <- data$Test$covariateData
    
    testDataRowId <- trainData$labels$rowId
    # sequencePlpData <- PatientLevelPrediction::getPlpData(
    #   databaseDetails = databaseDetails,
    #   covariateSettings = covariateSettingsSequence,
    #   restrictPlpDataSettings = restrictPlpDataSettings
    # )
    
    covariateDataTest <- Andromeda::copyAndromeda(transactionsPlpData$covariateData)
    
    covariateDataTest$covariates <- covariateDataTest$covariates %>%
      filter(rowId %in% testDataRowId)
    
    inputDataTest <- AssociationRuleMining::getInputFileForAssociationRules(covariateDataTest, 
                                                                        fileToSave = file.path(dirLocation, paste0(fileName, "test.txt")))
    
    inputDataTest <- inputDataTest %>% 
      group_by(rowId) %>% 
      arrange(rowId) %>%
      ungroup() %>% 
      as.data.frame(.data)
    
    transactions <- as(split(inputDataTest[,"covariateLabel"], inputDataTest[,"rowId"]), "transactions")
    
    # Extracting matching transactions
    patternsTrain <- covariateIdsInclude$trainPatterns
    patternsTest <- arules::supportingTransactions(patternsTrain, transactions = transactions)
    
    if (savePatterns){
      saveRDS(patternsTest, file.path(dirLocation, paste0(fileName, "_FPs_test.Rds")))
    }
    
    trainCovariateRef <- covariateIdsInclude$trainCovariateRef
    
    # transactionsRowId <- transactionInfo(transactions)$sequenceID
    transactionsRowId <- unique(transactionInfo(transactions)$transactionID)
    
    if(length(patternsTest) == 0){
      cov <- trainData
      ParallelLogger::logInfo("AR mining on the test set returned 0 ARs, therefore returning testData.")
    } else {
      cov <- addTestSetAssociationRulesToAndromeda(plpDataObject = trainData, 
                                                     fileWithFPs = patternsTest, 
                                                     objectWithIds = inputDataTest,
                                                     plpDataTrain = trainCovariateRef,
                                                     transactionsRowId = transactionsRowId,
                                                     fileToSave = file.path(dirLocation, fileName))
      
    }
    covariateIdsInclude <- list(trainPatterns = patternsTrain, 
                                trainCovariateRef = trainCovariateRef, 
                                testPatterns = patternsTest, 
                                testCovariateRef = cov$covariateData$covariateRef)
  }
  
  featureEngeering <- list(
    funct = 'extractAssociationRules',
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings, 
      covariateIdsInclude = covariateIdsInclude
    )
  )
  
  attr(cov, 'metaData')$featureEngineering = listAppend(
    attr(cov, 'metaData')$featureEngineering,
    featureEngeering
  )
  return(cov)
}
