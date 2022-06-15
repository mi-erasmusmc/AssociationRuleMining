#' @export
testSetToCovariateDataCSpade <- function(inputFile, objectWithIds, plpDataTrain){
  
  exp <- as(inputFile, "list")
  FrameData <- lapply(exp, function(x) as.data.frame(x))
  
  covariateLong <- reshape2::melt(FrameData, value.name = "sequenceID") %>%
    select(L1, sequenceID) %>%
    rename(Sequences = L1) %>%
    mutate(covariateValue = 1)
  
  
  # Making rowId numeric
  covariateLong$sequenceID <- as.numeric(covariateLong$sequenceID)
  
  # Fix rowIds
  trueRowIds <- tibble(sequenceID = as.numeric(inputFile@transactionInfo$sequenceID),
                       rowId = as.numeric(unique(objectWithIds$rowId)))
  covariateLong <- covariateLong %>% dplyr::inner_join(trueRowIds, by="sequenceID") %>%
    select(c("Sequences", "covariateValue", "rowId")) 
  
  # Fixing names of sequences
  
  # Getting unique covariateIds same as Train
  uniqueSeqs <- unique(covariateLong$Sequences)
  uniqueCovariates <- data.frame(covariateName = uniqueSeqs) 
  trainCovariateIds <- plpDataTrain$covariateRef %>% filter(analysisId == 999) %>% collect()
  uniqueCovariateIds <- trainCovariateIds %>% inner_join(uniqueCovariates, by = "covariateName")

  
  # include unique ids in the data
  covariateDataFp <- dplyr::left_join(x = covariateLong, y = uniqueCovariateIds, by = c("Sequences" = "covariateName"))

  
  # Constructing covariateData's object $covariates
  covariates <- covariateDataFp %>%
    dplyr::select(rowId, covariateId, covariateValue) 
  
  # Constructing covariateData's object $covariateRef
  covariateRef <- uniqueCovariateIds %>%
    dplyr::mutate(analysisId = 999, 
                  conceptId = 0)
  
  # Constructing covariateData's object $analysisRef
  analysisRef <- data.frame(analysisId = 999, 
                            analysisName = "FrequentPatterns", 
                            domainId = "FP",
                            startDay = 0, 
                            endDay = 0,
                            isBinary = "Y",
                            missingMeansZero = "Y",
                            stringsAsFactors = TRUE)
  
  metadata <- list()
  metadata$populationSize <- length(unique(covariates$rowId))
  metadata$cohortId <- -1
  result <- Andromeda::andromeda(covariates = covariates, 
                                 covariateRef = covariateRef, 
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metadata
  class(result) <- "CovariateData"
  attr(class(result), "package") <- "FeatureExtraction"
  
  return(result)
  
}

#' @export
testSetToCovariateDataObjectCSpade <- function(fileWithFPs, objectWithIds, covariateDataObject, plpDataTrain){
  
  t1start <- Sys.time()
  
  message("Writing frequent patterns as covariates...")
  fpdata <- testSetToCovariateDataCSpade(inputFile = fileWithFPs, objectWithIds = objectWithIds, plpDataTrain = plpDataTrain)
  
  t1duration <- Sys.time() - t1start
  
  message(paste("Writing FPs as covariates took", round(t1duration, 2), paste0(attr(t1duration, which = "units"), "."), sep = " "))
  
  t2start <- Sys.time()
  
  message("Appending covariates to covariate data object...")
  covariateData <- appendCovariateData(tempCovariateData = fpdata, covariateData = covariateDataObject)
  
  t2duration <- Sys.time() - t2start
  
  message(paste("Appending covariates took", round(t2duration, 2), paste0(attr(t2duration, which = "units"), "."), sep = " "))
  
  return(covariateData)
}

#' @export
addTestSetPatternsToAndromedaFromCSpade <- function(plpDataObject, fileWithFPs, plpDataTrain, objectWithIds, fileToSave) {
  if (!class(plpDataObject) == "plpData") {
    stop("plpDataObject should be a plpData object!")
  }
  
  oldPlpDataObject = plpDataObject
  
  # Step one: Copy the already existing covariateData object
  covariateData <- Andromeda::copyAndromeda(oldPlpDataObject$covariateData)
  
  #Step 2: add in there the FPs as covariates
  covariateData <- toCovariateDataObjectCSpade(fileWithFPs = fileWithFPs, objectWithIds = objectWithIds, covariateDataObject = covariateData) 
  covariateData <- testSetToCovariateDataObjectCSpade (fileWithFPs = fileWithFPs, objectWithIds = objectWithIds, covariateDataObject = covariateData, plpDataTrain = plpDataTrain) 
  
  #Step3: copy the old plpData[covariateData] attribute called "metadata"
  metaData <- attr(oldPlpDataObject$covariateData, "metaData")
  
  #step 3.5: and addong the old metadata to the new object
  attr(covariateData, "metaData") <- metaData
  
  # step4 : Giving the class name required
  class(covariateData) <- "CovariateData"
  
  # step 4.5: giving also the attribute that is created from the FeatureExtraction package
  attr(class(covariateData), "package") <- "FeatureExtraction"
  
  # Step 5: copying the old plp data object
  newPlpDataObject <- plpDataObject
  
  # step 5.5: attaching the new covariateData object to the new plpData
  newPlpDataObject$covariateData <- covariateData
  
  savePlpData(newPlpDataObject, file = fileToSave)
  
  return(newPlpDataObject)
  
}