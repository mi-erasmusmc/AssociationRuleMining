#' @export
createEmergentPatternsMiningSettings <- function(minimumSupport, 
                                                 maximumPatternLength,
                                                 maximumItemSize, 
                                                 removeLengthOnePatterns = FALSE, 
                                                 temporalPlpData, 
                                                 transactionsObject = NULL,
                                                 absoluteDifference, 
                                                 savePatterns = FALSE){
  
  miningEPsSettings <- list(support = minimumSupport, 
                            maxlen = maximumPatternLength, 
                            maxsize = maximumItemSize, 
                            removeLengthOnePatterns = removeLengthOnePatterns,
                            temporalPlpData = temporalPlpData, 
                            transactionsObject = transactionsObject,
                            absoluteDifference = absoluteDifference,
                            savePatterns = savePatterns
  )
  
  class(miningEPsSettings) <- "emergentPatternMiningSettings"
  return(miningEPsSettings) 
}

#' @export
extractEmergentPatternsSettings <- function(emergentPatternMiningSettings, plpDataSettings, outputFolder = getwd(), fileName){
  #add checks
  
  featureEngineeringSettings <- list(
    support = emergentPatternMiningSettings$support,
    maxlen =emergentPatternMiningSettings$maxlen,
    maxsize = emergentPatternMiningSettings$maxsize,
    temporalPlpData = emergentPatternMiningSettings$temporalPlpData,
    removeLengthOnePatterns = emergentPatternMiningSettings$removeLengthOnePatterns,
    transactionsObject = emergentPatternMiningSettings$transactionsObject,
    absoluteDifference = emergentPatternMiningSettings$absoluteDifference,
    savePatterns = emergentPatternMiningSettings$savePatterns,
    plpDataSettings = plpDataSettings,
    outputFolder = outputFolder, 
    fileName = fileName
  )
  
  # specify the function that will implement the sampling
  attr(featureEngineeringSettings, "fun") <- "extractEmergentPatterns"
  
  # make sure the object returned is of class "sampleSettings"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
  
}

#' @export
extractEmergentPatterns <- function(trainData, featureEngineeringSettings, covariateIdsInclude = NULL){
  # frequent pattern mining settings
  minimumSupport = featureEngineeringSettings$support
  patternLength = featureEngineeringSettings$maxlen
  itemSize = featureEngineeringSettings$maxsize
  outputFolder = featureEngineeringSettings$outputFolder
  fileName = featureEngineeringSettings$fileName
  removeLengthOnePatterns = featureEngineeringSettings$removeLengthOnePatterns
  temporalPlpData = featureEngineeringSettings$temporalPlpData
  transactionsObject = featureEngineeringSettings$transactionsObject
  absoluteDifference = featureEngineeringSettings$absoluteDifference
  populationSettings = featureEngineeringSettings$populationSettings
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
    
    ParallelLogger::logInfo(paste("Preparing study population of train set..."))
    # trainStudyPopulation <- trainData$labels$outcomeCount
    
    ParallelLogger::logInfo(paste("Getting negative train set row ids..."))
    negativeRowId <- trainData$labels %>% 
      filter(outcomeCount == 0) %>%
      pull(rowId)
    ParallelLogger::logInfo(paste("Getting positive train set row ids..."))
    positiveRowId <- trainData$labels %>% 
      filter(outcomeCount == 1) %>%
      pull(rowId)
    
    covariateData <- temporalPlpData$covariateData
    
    ParallelLogger::logInfo(paste("Splitting covariateData..."))
    negativeCovariateData <- copyAndromeda(covariateData)
    negativeCovariateData$covariates <- negativeCovariateData$covariates %>%
      filter(rowId %in% negativeRowId)
    positiveCovariateData <- copyAndromeda(covariateData)
    positiveCovariateData$covariates <- positiveCovariateData$covariates %>%
      filter(rowId %in% positiveRowId)
    
    ParallelLogger::logTrace("\nPreparing data for Frequent Pattern mining...")
    
    start <- Sys.time()
    # if (file.exists(file.path(dirLocation, paste0(fileName, ".txt"))) == FALSE){
    inputDataNegative <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = negativeCovariateData, 
                                                                      fileToSave = file.path(dirLocation, paste0(fileName, "negative.txt")))
    
    inputDataPositive <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = positiveCovariateData, 
                                                                      fileToSave = file.path(dirLocation, paste0(fileName, "positive.txt")))
    
    transactionsNegative <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "negative.txt")), sep = ";", 
                                                          info = c("sequenceID","eventID","SIZE"))
    # transactionsNegative@itemsetInfo$sequenceID <- inputDataNegative$rowId
    transactionsPositive <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "positive.txt")), sep = ";", 
                                                          info = c("sequenceID","eventID","SIZE"))
    # transactionsPositive@itemsetInfo$sequenceID <- inputDataPositive$rowId
    # } else {
    #   transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, ".txt")), sep = ";", 
    #                                                 info = c("sequenceID","eventID","SIZE"))
    # }
    
    delta <- Sys.time() - start
    ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
    s0 <- arulesSequences::cspade(data = transactionsNegative, 
                                  parameter = list(support = minimumSupport, 
                                                   maxlen = patternLength, 
                                                   maxsize = itemSize), 
                                  control = list(verbose = TRUE, tidLists = TRUE, numpart = 1))
    
    s1 <- arulesSequences::cspade(data = transactionsPositive, 
                                  parameter = list(support = minimumSupport, 
                                                   maxlen = patternLength, 
                                                   maxsize = itemSize), 
                                  control = list(verbose = TRUE, tidLists = TRUE, numpart = 1))
    if (savePatterns){
      saveRDS(s0, file.path(dirLocation, paste0(fileName, "_negative_FPs_train.Rds")))
      saveRDS(s1, file.path(dirLocation, paste0(fileName, "_positive_FPs_train.Rds")))
      
    }
    
    initialFPsNegative <- as.numeric(dim(s0)[1])
    initialFPsPositive <- as.numeric(dim(s1)[1])
    
    ParallelLogger::logInfo(paste0("The set of FPs extracted for the negative outcome class were ", initialFPsNegative, ".", " The set of FPs extracted for the positive outcome class were ", initialFPsPositive, "."))
    
    if (removeLengthOnePatterns == TRUE){
      s0 <- arulesSequences::subset(s0, size(x) > 1)
      s1 <- arulesSequences::subset(s1, size(x) > 1)
      remainingFPsNegative <- as.numeric(dim(s0)[1])
      remainingFPsPositive <- as.numeric(dim(s1)[1])
      ParallelLogger::logInfo(paste("After removing length one FPs from the negative class, there were", remainingFPsNegative, "remaining.", "After removing length one FPs from the positive class, there were", remainingFPsPositive, "remaining."))
    }
    
    #if (nrow(s0) < 5000) {
    #Need new function to combine results
    commonPatterns <- AssociationRuleMining::filterCommonPatterns(s0, 
                                                                  s1, 
                                                                  transactionsNegative = transactionsNegative, 
                                                                  transactionsPositive = transactionsPositive,
                                                                  absoluteDifference = absoluteDifference)
    ParallelLogger::logInfo(paste("After removing redundant FPs from the negative class, there were", dim(commonPatterns[[1]])[1], "remaining.", 
                                  "After removing length one FPs from the positive class, there were", dim(commonPatterns[[2]])[1], "remaining."))
  
    if(dim(commonPatterns[[1]])[1]== 0){
     cov0 <- negativeCovariateData
     transactionsRowIdNegative <- NULL
      ParallelLogger::logInfo("FP mining returned 0 FPs for negative class therefore returning trainData.")
    } else {
      cov0 <- commonPatterns[[1]]
      transactionsRowIdNegative <- unique(transactionInfo(transactionsNegative)$sequenceID)
    }
    
    if (dim(commonPatterns[[2]])[1]== 0) {
      cov1 <- positiveCovariateData
      transactionsRowIdPositive <- NULL
      ParallelLogger::logInfo("FP mining returned 0 FPs for positive class therefore returning trainData.")
    } else {
      cov1 <- commonPatterns[[2]]
      transactionsRowIdPositive <- unique(transactionInfo(transactionsPositive)$sequenceID)
    }
    
    cov <- addEmergentPatternsToAndromeda(plpDataObject = trainData,
                                          fileWithFPsNegative = cov0, 
                                          fileWithFPsPositive = cov1,
                                          transactionsRowIdNegative = transactionsRowIdNegative, 
                                          transactionsRowIdPositive = transactionsRowIdPositive, 
                                          objectWithIdsNegative = inputDataNegative,
                                          objectWithIdsPositive = inputDataPositive, 
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
  # }
  #drop(covariateData)
  covariateIdsInclude <- list(trainPatterns = commonPatterns, 
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
  
  inputDataTest <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateDataTest, 
                                                                fileToSave = file.path(dirLocation, 
                                                                                       paste0(fileName, "testSet.txt")))
  #browser()
  transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, 
                                                                 paste0(fileName, "testSet.txt")), sep = ";", info = c("sequenceID","eventID","SIZE"))
  
  # sTest <- arulesSequences::cspade(data = transactions, parameter = list(support = minimumSupport, maxlen = patternLength, maxsize = itemSize), control = list(verbose = TRUE, tidLists = TRUE))
  # Extracting matching transactions
  patternsTrain <- c(patternsTrain[[1]], patternsTrain[[2]])
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
  funct = 'extractEmergentPatterns',
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
