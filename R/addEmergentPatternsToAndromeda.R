#' @export
addEmergentPatternsToAndromeda <- function(plpDataObject, 
                                          fileWithFPsNegative,
                                          fileWithFPsPositive,
                                          transactionsRowIdNegative,
                                          transactionsRowIdPositive,
                                          objectWithIdsNegative,
                                          objectWithIdsPositive,
                                          fileToSave) {
  
  if (!class(plpDataObject) == "plpData") {
    stop("plpDataObject should be a plpData object!")
  }
  
  oldPlpDataObject = plpDataObject
  
  # Step one: Copy the already existing covariateData object
  covariateData <- Andromeda::copyAndromeda(oldPlpDataObject$covariateData)
  
  #Step 2: add in there the FPs from Negative as covariates
  if(!is.null(transactionsRowIdNegative) && is.null(transactionsRowIdPositive)){
  covariateData <- toCovariateDataObjectCSpade(fileWithFPs = fileWithFPsNegative, 
                                               objectWithIds = objectWithIdsNegative,
                                               transactionsRowId = transactionsRowIdNegative,
                                               covariateDataObject = covariateData) 
  }
  
  
  #Step 2.5: add in there the FPs from Positive as covariates
  if(!is.null(transactionsRowIdPositive) && is.null(transactionsRowIdNegative)){
  covariateData <- toCovariateDataObjectCSpade(fileWithFPs = fileWithFPsPositive, 
                                               objectWithIds = objectWithIdsPositive,
                                               transactionsRowId = transactionsRowIdPositive,
                                               covariateDataObject = covariateData) 
  }
  
  if(!is.null(transactionsRowIdPositive) && !is.null(transactionsRowIdNegative)){
    # fileWithFPs <- c(fileWithFPsNegative, fileWithFPsPositive)
    # tidListNegative <- as(fileWithFPsNegative@tidLists, "list")
    # tidListPositive <- as(fileWithFPsPositive@tidLists, "list")
    # keys <- unique(c(names(tidListNegative), names(tidListPositive)))
    # listWithFps <- setNames(mapply(c, tidListNegative[keys], tidListPositive[keys]), keys)
    # objectWithIds <- full_join(objectWithIdsNegative, objectWithIdsPositive)
    
    covariateData <- toCovariateDataObjectCSpadeEmergentPatterns(covariateDataObject = covariateData, 
                                                                 inputFileNegative = fileWithFPsNegative, 
                                                                 inputFilePositive = fileWithFPsPositive, 
                                                                 objectWithIdsNegative = objectWithIdsNegative,
                                                                 objectWithIdsPositive = objectWithIdsPositive, 
                                                                 transactionsRowIdNegative = transactionsRowIdNegative, 
                                                                 transactionsRowIdPositive = transactionsRowIdPositive)
    
  }
  
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

#' @export
toCovariateDataCSpadeEmergentPatterns <- function(inputFileNegative,
                                                  inputFilePositive, 
                                                  objectWithIdsNegative,
                                                  objectWithIdsPositive, 
                                                  transactionsRowIdNegative, 
                                                  transactionsRowIdPositive){
  
  exp0 <- as(inputFileNegative@tidLists, "list")
  FrameData0 <- lapply(exp0, function(x) as.data.frame(x))
  
  suppressMessages({
    covariateLong0 <- reshape2::melt(FrameData0, value.name = "sequenceID") %>%
      dplyr::select(L1, sequenceID) %>%
      dplyr::rename(Sequences = L1) %>%
      dplyr::mutate(covariateValue = 1)
  })
  
  # Making rowId numeric
  covariateLong0$sequenceID <- as.numeric(covariateLong0$sequenceID)
  
  
  # trueRowIds <- tibble(sequenceID = as.numeric(inputFile@tidLists@transactionInfo$sequenceID),
  #                      rowId = as.numeric(unique(objectWithIds$rowId)))
  
  trueRowIds0 <- tibble(sequenceID = as.numeric(transactionsRowIdNegative),
                       rowId = as.numeric(unique(objectWithIdsNegative$rowId)))
  covariateLong0 <- covariateLong0 %>% dplyr::inner_join(trueRowIds0, by="sequenceID") %>%
    select(c("Sequences", "covariateValue", "rowId"))
  
  exp1 <- as(inputFilePositive@tidLists, "list")
  FrameData1 <- lapply(exp1, function(x) as.data.frame(x))
  
  suppressMessages({
    covariateLong1 <- reshape2::melt(FrameData1, value.name = "sequenceID") %>%
      dplyr::select(L1, sequenceID) %>%
      dplyr::rename(Sequences = L1) %>%
      dplyr::mutate(covariateValue = 1)
  })
  
  # Making rowId numeric
  covariateLong1$sequenceID <- as.numeric(covariateLong1$sequenceID)
  
  
  # trueRowIds <- tibble(sequenceID = as.numeric(inputFile@tidLists@transactionInfo$sequenceID),
  #                      rowId = as.numeric(unique(objectWithIds$rowId)))
  
  trueRowIds1 <- tibble(sequenceID = as.numeric(transactionsRowIdPositive),
                        rowId = as.numeric(unique(objectWithIdsPositive$rowId)))
  covariateLong1 <- covariateLong1 %>% dplyr::inner_join(trueRowIds1, by="sequenceID") %>%
    select(c("Sequences", "covariateValue", "rowId"))
  
  covariateLong <- rbind(covariateLong0, covariateLong1)
  # Fixing names of sequences
  
  #### Need to change this for the arulesSequences results
  #covariateLong$Sequences <- gsub(x = covariateLong$Sequences, pattern = "-1", replacement = "=>")
  #covariateLong$Sequences <- gsub(x = covariateLong$Sequences,pattern = "=>$", replacement = "")
  
  # Constructing unique covariate Ids
  uniqueSeqs <- unique(covariateLong$Sequences)
  uniqueCovariates <- data.frame(covariateName = uniqueSeqs) 
  uniqueCovariateIds <- uniqueCovariates %>%
    dplyr::mutate(covariateId = as.numeric(getUniqueId(Names = covariateName, idstaken = NULL)))
  
  # Assign the true patient id to rowId
  
  #trueRowIds <- as.data.frame(unique(objectWithIds$rowId))
  #colnames(trueRowIds) <- "trueRowIds"
  
  #trueRowIds <- trueRowIds %>%
  #  dplyr::mutate(row_no = dplyr::row_number(), 
  #                java_row_id = row_no - 1)
  
  # trueRowIds <- objectWithIds %>%
  #   select("rowId", "cspadeRowId") 
  # 
  # trueIdDf <- data.frame("rowId" = unique(trueRowIds$rowId),
  #                        "cspadeRowId" = unique(trueRowIds$cspadeRowId))
  
  
  
  
  #outputRowIds <- as.data.frame(unique(covariateTidy$rowId))
  # colnames(outputRowIds) <- "outputRowIds"
  #idData <- dplyr::bind_cols(outputRowIds, trueRowIds) #%>%
  #dplyr::rename("outputRowIds" = ...1, 
  #              "trueRowIds" = ...2) 
  
  
  # include unique ids in the data
  covariateDataFp <- dplyr::left_join(x = covariateLong, y = uniqueCovariateIds, by = c("Sequences" = "covariateName"))
  
  # include true row ids
  # covariateDataFp <- dplyr::left_join(x = covariateDataFp, y= trueIdDf, by = c("rowId" = "cspadeRowId"))
  
  # adding 1 to match R's enum
  # covariateDataFp$rowId <- covariateDataFp$rowId + 1 
  
  # Constructing covariateData's object $covariates
  covariates <- covariateDataFp %>%
    dplyr::select(rowId, covariateId, covariateValue) #%>%
  # dplyr::rename("rowId" = "trueRowIds")
  
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
toCovariateDataObjectCSpadeEmergentPatterns <- function(covariateDataObject, 
                                                        inputFileNegative,
                                                        inputFilePositive, 
                                                        objectWithIdsNegative,
                                                        objectWithIdsPositive, 
                                                        transactionsRowIdNegative,
                                                        transactionsRowIdPositive){
  
  t1start <- Sys.time()
  
  message("Writing frequent patterns as covariates...")
  fpdata <- toCovariateDataCSpadeEmergentPatterns(inputFileNegative = inputFileNegative,
                                                  inputFilePositive = inputFilePositive,
                                                  objectWithIdsNegative = objectWithIdsNegative, 
                                                  objectWithIdsPositive = objectWithIdsPositive,
                                                  transactionsRowIdNegative = transactionsRowIdNegative, 
                                                  transactionsRowIdPositive = transactionsRowIdPositive)
  
  t1duration <- Sys.time() - t1start
  
  message(paste("Writing FPs as covariates took", round(t1duration, 2), paste0(attr(t1duration, which = "units"), "."), sep = " "))
  
  t2start <- Sys.time()
  
  message("Appending covariates to covariate data object...")
  covariateData <- appendCovariateData(tempCovariateData = fpdata, covariateData = covariateDataObject)
  
  t2duration <- Sys.time() - t2start
  
  message(paste("Appending covariates took", round(t2duration, 2), paste0(attr(t2duration, which = "units"), "."), sep = " "))
  
  return(covariateData)
}

