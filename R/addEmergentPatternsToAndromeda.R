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
  covariateData <- toCovariateDataObjectCSpade(fileWithFPs = fileWithFPsNegative, 
                                               objectWithIds = objectWithIdsNegative,
                                               transactionsRowId = transactionsRowIdNegative,
                                               covariateDataObject = covariateData) 
  
  #Step 2.5: add in there the FPs from Positive as covariates
  covariateData <- toCovariateDataObjectCSpade(fileWithFPs = fileWithFPsPositive, 
                                               objectWithIds = objectWithIdsPositive,
                                               transactionsRowId = transactionsRowIdPositive,
                                               covariateDataObject = covariateData) 
  
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
