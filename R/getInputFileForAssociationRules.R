#' @export
getInputFileForAssociationRules <- function(covariateDataObject, fileToSave){
  
  if(fileToSave == ""){
    stop("Must declare a filename")
  } 
  
  if(grepl("\\.txt$", fileToSave)== FALSE){
    stop("Filename should be a .txt file")
  }
  
  #if(file.exists(fileToSave)){
  #  warning("File already exists!")
  #  overwrite <- menu(c("Yes", "No"), title="Should it be overwritten?")  
  #  if (overwrite==2){
  #    stop("Operation interrupted by user. Declare a different file name or location.") 
  #  }
  #}
  
  data <- covariateDataObject$covariates %>% dplyr::collect()
  
  message("Getting covariate names from covariate data object and writing text file...")
  NamesData <- getNamesFromCovariateId(data, covariateDataObject = covariateDataObject, fileToSave = fileToSave)
  
  message("Preparing input data for Association Rules...")
  getInputDataForAssociationRules(NamesData, filename = fileToSave)
  
  return(NamesData)
}