toCovariateDataObject <- function(fileWithFPs, objectWithIds, covariateDataObject){
  
  t1start <- Sys.time()
  
  message("Writing frequent patterns as covariates...")
  fpdata <- toCovariateData(fileWithFPs, objectWithIds)
  
  t1duration <- Sys.time() - t1start
  
  message(paste0("Writing FPs as covariates took", round(t1duration, 2), "minutes."))
  
  t2start <- Sys.time()
  
  message("Appending covariates to covariate data object...")
  covariateData <- appendCovariateData(tempCovariateData = fpdata, covariateData = covariateDataObject)
  
  t2duration <- Sys.time() - t2start
  
  message(paste0("Appending covariates took", round(t2duration, 2), "minutes."))
  
  return(covariateData)
}