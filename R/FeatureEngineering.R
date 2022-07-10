#' @export
createFrequentPatternMiningSettings <- function(minimumSupport, 
                                                maximumPatternLength,
                                                maximumItemSize, 
                                                removeLengthOnePatterns = FALSE, 
                                                temporalPlpData, 
                                                transactionsObject = NULL, 
                                                savePatterns = FALSE){
  
  miningFPsSettings <- list(support = minimumSupport, 
                            maxlen = maximumPatternLength, 
                            maxsize = maximumItemSize, 
                            removeLengthOnePatterns = removeLengthOnePatterns,
                            temporalPlpData = temporalPlpData, 
                            transactionsObject = transactionsObject,
                            savePatterns = savePatterns
  )
  
  class(miningFPsSettings) <- "frequentPatternMiningSettings"
  return(miningFPsSettings) 
}

#' @export
extractFrequentPatternsSettings <- function(frequentPatternMiningSettings, plpDataSettings, outputFolder = getwd(), fileName){
  #add checks
  
  featureEngineeringSettings <- list(
    support = frequentPatternMiningSettings$support,
    maxlen =frequentPatternMiningSettings$maxlen,
    maxsize = frequentPatternMiningSettings$maxsize,
    temporalPlpData = frequentPatternMiningSettings$temporalPlpData,
    removeLengthOnePatterns = frequentPatternMiningSettings$removeLengthOnePatterns,
    transactionsObject = frequentPatternMiningSettings$transactionsObject,
    savePatterns = frequentPatternMiningSettings$savePatterns,
    plpDataSettings = plpDataSettings,
    outputFolder = outputFolder, 
    fileName = fileName
  )
  
  # specify the function that will implement the sampling
  attr(featureEngineeringSettings, "fun") <- "extractFrequentPatterns"
  
  # make sure the object returned is of class "sampleSettings"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
  
}

#' @export
extractFrequentPatterns <- function(trainData, featureEngineeringSettings, covariateIdsInclude = NULL){
  # frequent pattern mining settings
  minimumSupport = featureEngineeringSettings$support
  patternLength = featureEngineeringSettings$maxlen
  itemSize = featureEngineeringSettings$maxsize
  outputFolder = featureEngineeringSettings$outputFolder
  fileName = featureEngineeringSettings$fileName
  removeLengthOnePatterns = featureEngineeringSettings$removeLengthOnePatterns
  temporalPlpData = featureEngineeringSettings$temporalPlpData
  transactionsObject = featureEngineeringSettings$transactionsObject
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
    
    covariateData <- Andromeda::copyAndromeda(temporalPlpData$covariateData)
    
    covariateData$covariates <- covariateData$covariates %>%
      filter(rowId %in% trainDataRowId)
    ParallelLogger::logTrace("\nPreparing data for Frequent Pattern mining...")
    start <- Sys.time()
    if (is.null(transactionsObject) == TRUE){
    inputData <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateData, 
                                                              fileToSave = file.path(dirLocation, paste0(fileName, ".txt")))
    
    transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, ".txt")), sep = ";", 
                                                  info = c("sequenceID","eventID","SIZE"))
    } else {
      transactions <- transactionsObject
    }
    
    delta <- Sys.time() - start
    ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
    s0 <- arulesSequences::cspade(data = transactions, 
                                  parameter = list(support = minimumSupport, 
                                                                        maxlen = patternLength, 
                                                                        maxsize = itemSize), 
                                  control = list(verbose = TRUE, tidLists = TRUE))
    if (savePatterns){
    saveRDS(s0, file.path(dirLocation, paste0(fileName, "_FPs_train.Rds")))
    }
    
    initialFPs <- as.numeric(dim(s0)[1])
    ParallelLogger::logInfo(paste("The set of FPs extracted were", initialFPs, "."))
    
    if (removeLengthOnePatterns == TRUE){
      s0 <- arulesSequences::subset(s0, size(x) > 1)
      remainingFPs <- as.numeric(dim(s0)[1])
      ParallelLogger::logInfo(paste("After removing length one FPs there were", remainingFPs, "remaining."))
    }
    
    if(dim(s0)[1]== 0){
      cov <- trainData
      ParallelLogger::logInfo("FP mining returned 0 FPs therefore returning trainData.")
    } else {
    transactionsRowId <- unique(transactionInfo(transactions)$sequenceID)
    
    cov <- AssociationRuleMining::addFrequentPatternsToAndromedaFromCSpade(plpDataObject = trainData, 
                                                                           fileWithFPs = s0,
                                                                           objectWithIds = inputData, 
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
    
    covariateDataTest <- Andromeda::copyAndromeda(temporalPlpData$covariateData)
    
    covariateDataTest$covariates <- covariateDataTest$covariates %>%
      filter(rowId %in% testDataRowId)
    
    inputDataTest <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateDataTest, fileToSave = file.path(dirLocation, paste0(fileName, "testSet.txt")))
    #browser()
    transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "testSet.txt")), sep = ";", info = c("sequenceID","eventID","SIZE"))
    
    transactionsRowId <- unique(transactionInfo(transactions)$sequenceID)
    # sTest <- arulesSequences::cspade(data = transactions, parameter = list(support = minimumSupport, maxlen = patternLength, maxsize = itemSize), control = list(verbose = TRUE, tidLists = TRUE))
    # Extracting matching transactions
    patternsTrain <- covariateIdsInclude$trainPatterns
    patternsTest <- arules::supportingTransactions(patternsTrain, transactions = transactions)
    
    if (savePatterns){
    saveRDS(patternsTest, file.path(dirLocation, paste0(fileName, "_FPs_test.Rds")))
    }
    
    trainCovariateRef <- covariateIdsInclude$trainCovariateRef
    
    transactionsRowId <- transactionInfo(transactions)$sequenceID
    
    if(dim(patternsTest)[1]== 0){
      cov <- trainData
      ParallelLogger::logInfo("FP mining on the test set returned 0 FPs, therefore returning testData.")
    } else {
    cov <- addTestSetPatternsToAndromedaFromCSpade(plpDataObject = trainData, 
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
    funct = 'extractFrequentPatterns',
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
