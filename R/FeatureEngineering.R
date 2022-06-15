#' @export
createFrequentPatternMiningSettings <- function(minimumSupport, 
                                                maximumPatternLength,
                                                maximumItemSize, 
                                                featureExtractionSettings){
  
  miningFPsSettings <- list(support = minimumSupport, 
                            maxlen = maximumPatternLength, 
                            maxsize = maximumItemSize, 
                            featureExtractionSettings = featureExtractionSettings 
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
    temporalSequenceFeatureExtractionSettings = frequentPatternMiningSettings$featureExtractionSettings,
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
  
  # featureExtraction settings
  covariateSettingsSequence <- featureEngineeringSettings$temporalSequenceFeatureExtractionSettings
  databaseDetails <- featureEngineeringSettings$plpDataSettings$databaseDetails
  restrictPlpDataSettings <- featureEngineeringSettings$plpDataSettings$restrictPlpDataSettings
  dirLocation <- file.path(getwd(), outputFolder)
  
  if (!dir.exists(dirLocation)){
    dir.create(dirLocation)
  }
  
  if (is.null(covariateIdsInclude) == TRUE){
    
    trainDataRowId <- trainData$labels$rowId
    
    sequencePlpData <- PatientLevelPrediction::getPlpData(
      databaseDetails = databaseDetails,
      covariateSettings = covariateSettingsSequence,
      restrictPlpDataSettings = restrictPlpDataSettings
    )
    
    covariateData <- sequencePlpData$covariateData
    
    covariateData$covariates <- covariateData$covariates %>%
      filter(rowId %in% trainDataRowId)
    
    
    inputData <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateData, fileToSave = file.path(dirLocation, paste0(fileName, ".txt")))
    #browser()
    transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, ".txt")), sep = ";", info = c("sequenceID","eventID","SIZE"))
    
    s0 <- arulesSequences::cspade(data = transactions, parameter = list(support = minimumSupport, maxlen = patternLength, maxsize = itemSize), control = list(verbose = TRUE, tidLists = TRUE))
    
    cov <- AssociationRuleMining::addFrequentPatternsToAndromedaFromCSpade(plpDataObject = trainData, fileWithFPs = s0, objectWithIds = inputData, fileToSave = file.path(getwd(), outputFolder, fileName))
    #browser()
    
    covariateIdsInclude <- list(trainPatterns = s0, 
                                trainCovariateRef = cov$covariateData$covariateRef)
  } else {
    
    # Which of the FPs to extract
    # covariateDataTest <- data$Test$covariateData
    
    testDataRowId <- data$Test$labels$rowId
    sequencePlpData <- PatientLevelPrediction::getPlpData(
      databaseDetails = databaseDetails,
      covariateSettings = covariateSettingsSequence,
      restrictPlpDataSettings = restrictPlpDataSettings
    )
    
    covariateDataTest <- sequencePlpData$covariateData
    
    covariateDataTest$covariates <- covariateDataTest$covariates %>%
      filter(rowId %in% testDataRowId)
    
    inputDataTest <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateDataTest, fileToSave = file.path(dirLocation, paste0(fileName, "test.txt")))
    #browser()
    transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "test.txt")), sep = ";", info = c("sequenceID","eventID","SIZE"))
    
    # sTest <- arulesSequences::cspade(data = transactions, parameter = list(support = minimumSupport, maxlen = patternLength, maxsize = itemSize), control = list(verbose = TRUE, tidLists = TRUE))
    # Extracting matching transactions
    patternsTrain <- covariateIdsInclude$trainPatterns
    patternsTest <- arules::supportingTransactions(patternsTrain, transactions = transactions)
    trainCovariateRef <- covariateIdsInclude$trainCovariateRef
    
# Not sure abot the flag for the plpDataObject in the next line
    cov <- AssociationRuleMining::addTestSetPatternsToAndromedaFromCSpade(plpDataObject = trainData, fileWithFPs = patternsTest, objectWithIds = inputDataTest, plpDataTrain = trainCovariateRef, fileToSave = file.path(getwd(), outputFolder, fileName))
    
    covariateIdsInclude <- list(trainPatterns = patternsTrain, 
                                trainCovariateRef = trainCovariateRef, 
                                testPatterns = patternsTest, 
                                testCovariateRef = cov$covariateData$covariateRef)
    # # Find them in the test set
    # ## extract sequence data
    # sequenceTestPlpData <- PatientLevelPrediction::getPlpData(
    #   databaseDetails = databaseDetails,
    #   covariateSettings = covariateSettingsSequence,
    #   restrictPlpDataSettings = restrictPlpDataSettings
    # )
    # 
    # testDataSequences <- inner_join(sequenceTestPlpData$covariateData$covariates, sequenceTestPlpData$covariateData$covariateRef,by = "covariateId") %>% 
    #   arrange(rowId, desc(timeId)) %>%
    #   collect() %>%
    #   mutate(covariateLabel = stringr::str_replace(covariateName, ".*: ", ""), 
    #          covariateLabel = stringr::str_replace_all(covariateLabel, " ", "_")) %>%
    #   group_by(rowId) %>%
    #   summarize(sequenceCovariteIdString = paste0(covariateId, collapse = ","), 
    #             sequenceCovariateNameString = paste0(covariateLabel, collapse = ","), .groups = "drop") 
    # 
    # 
    # 
    # fps <- lapply(fpsToExtract$Sequence, function(x) grep(x, testDataSequences$sequenceCovariateNameString))
    # names(fps) <- fpsToextract$covariateName
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
