toCovariateDataObject <- function(fileWithFPs, objectWithIds, covariateDataObject){
  
  fpdata <- toCovariateData(fileWithFPs, objectWithIds)
  covariateData <- appendCovariateData(tempCovariateData = fpdata, covariateData = covariateDataObject)
  
  return(covariateData)
}