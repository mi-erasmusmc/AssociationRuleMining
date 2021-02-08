toCovariateDataObject <- function(fileWithFPs, objectWithIds, covariateDataObject){
  
  message("Writing frequent patterns as covariates...")
  fpdata <- toCovariateData(fileWithFPs, objectWithIds)
  
  message("Appending covariates to covariate data object...")
  covariateData <- appendCovariateData(tempCovariateData = fpdata, covariateData = covariateDataObject)
  
  return(covariateData)
}