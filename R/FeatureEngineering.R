#' @export
createFrequentPatternMiningSettings <- function(minimumSupport, 
                                                maximumPatternLength,
                                                maximumItemSize, 
                                                removeLengthOnePatterns = FALSE, 
                                                temporalPlpData, 
                                                transactionsObject = NULL, 
                                                savePatterns = FALSE, 
                                                classification = FALSE){
  
  miningFPsSettings <- list(support = minimumSupport, 
                            maxlen = maximumPatternLength, 
                            maxsize = maximumItemSize, 
                            removeLengthOnePatterns = removeLengthOnePatterns,
                            temporalPlpData = temporalPlpData, 
                            transactionsObject = transactionsObject,
                            savePatterns = savePatterns, 
                            classification = classification
  )
  
  class(miningFPsSettings) <- "frequentPatternMiningSettings"
  return(miningFPsSettings) 
}

#' @export
extractFrequentPatternsSettings <- function(frequentPatternMiningSettings, outputFolder = getwd(), fileName){
  #add checks
  
  featureEngineeringSettings <- list(
    support = frequentPatternMiningSettings$support,
    maxlen =frequentPatternMiningSettings$maxlen,
    maxsize = frequentPatternMiningSettings$maxsize,
    temporalPlpData = frequentPatternMiningSettings$temporalPlpData,
    removeLengthOnePatterns = frequentPatternMiningSettings$removeLengthOnePatterns,
    transactionsObject = frequentPatternMiningSettings$transactionsObject,
    savePatterns = frequentPatternMiningSettings$savePatterns,
    classification = frequentPatternMiningSettings$classification,
    # plpDataSettings = plpDataSettings,
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
  classification = featureEngineeringSettings$classification
  
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
    # if (is.null(transactionsObject)){
    # if (file.exists(file.path(dirLocation, paste0(fileName, ".txt"))) == FALSE){
    if (classification == FALSE){
    inputData <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateData, 
                                                              fileToSave = file.path(dirLocation, paste0(fileName, ".txt")))
    
    transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, ".txt")), sep = ";", 
                                                  info = c("sequenceID","eventID","SIZE"))
    # } else {
    #   transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, ".txt")), sep = ";", 
    #                                                 info = c("sequenceID","eventID","SIZE"))
    # }
    
    delta <- Sys.time() - start
    ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
    s0 <- arulesSequences::cspade(data = transactions, 
                                  parameter = list(support = minimumSupport, 
                                                                        maxlen = patternLength, 
                                                                        maxsize = itemSize), 
                                  control = list(verbose = TRUE, tidLists = TRUE, numpart = 1))
    } else {
      trInputData <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateData, 
                                                                fileToSave = file.path(dirLocation, paste0(fileName, ".txt")))
      labels <- data.frame(rowId = trainData$labels$rowId, 
                           outcomeCount = trainData$labels$outcomeCount)
      inputData <- AssociationRuleMining::getInputFileForCSpadeWithClass(studyPopulation = labels, 
                                                                         transactions = trInputData, 
                                                                         outputFolder = file.path(dirLocation, paste0(fileName, "class.txt")))
      
      transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "class.txt")), sep = ";", 
                                                    info = c("sequenceID","eventID","SIZE", "classID"))
    
      delta <- Sys.time() - start
      ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
      s0 <- arulesSequences::cspade(data = transactions, 
                                    parameter = list(support = minimumSupport, 
                                                     maxlen = patternLength, 
                                                     maxsize = itemSize), 
                                    control = list(verbose = TRUE, tidLists = TRUE, numpart = 1))
    }
    
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
    
    #if (nrow(s0) < 5000) {
    cov <- AssociationRuleMining::addFrequentPatternsToAndromedaFromCSpade(plpDataObject = trainData, 
                                                                           fileWithFPs = s0,
                                                                           objectWithIds = inputData, 
                                                                           transactionsRowId = transactionsRowId,
                                                                           fileToSave = file.path(dirLocation, fileName))
    # } else {
    #   batches <- createBatch(s0)
    #   andromedaList <- lapply(batches, function(x) AssociationRuleMining::addFrequentPatternsToAndromedaFromCSpade(plpDataObject = trainData, 
    #                                                                                                              fileWithFPs = x,
    #                                                                                                              objectWithIds = inputData, 
    #                                                                                                              transactionsRowId = transactionsRowId,
    #                                                                                                              fileToSave = file.path(dirLocation, fileName)))
    # covariateList <- lapply(andromedaList[-1], function(x) appendBatch(andromedaList[[1]]$covariateData$covariates, x$covariateData$covariates))
    # covariateRefList <- lapply(andromedaList[-1], function(x) appendBatch(andromedaList[[1]]$covariateData$covariateRef, x$covariateData$covariateRef))
    # cov <- andromedaList[[1]]
    # lapply(andromedaList[-1], function(x) Andromeda::close(x$covariateData))
    # }
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
    
    # sTest <- arulesSequences::cspade(data = transactions, parameter = list(support = minimumSupport, maxlen = patternLength, maxsize = itemSize), control = list(verbose = TRUE, tidLists = TRUE))
    # Extracting matching transactions
    patternsTrain <- covariateIdsInclude$trainPatterns
    patternsTest <- arules::supportingTransactions(patternsTrain, transactions = transactions)
    
    if (savePatterns){
    saveRDS(patternsTest, file.path(dirLocation, paste0(fileName, "_FPs_test.Rds")))
    }
    
    trainCovariateRef <- covariateIdsInclude$trainCovariateRef
    
    # transactionsRowId <- transactionInfo(transactions)$sequenceID
    transactionsRowId <- unique(transactionInfo(transactions)$sequenceID)
    
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

createBatch <- function(frequentPatterns, batchSize = 5000){
  x <- split(frequentPatterns, (seq(nrow(frequentPatterns))-1) %/% batchSize) 
  return(x)
}

appendBatch <- function(tbl, batch){
  Andromeda::appendToTable(tbl, batch)
}

