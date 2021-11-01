getInputFileForFrequentPatterns <- function(covariateDataObject, fileToSave) {
  if(fileToSave == ""){
    stop("Must declare a filename")
  } 
  
  if(grepl("\\.txt$", fileToSave)== FALSE){
    stop("Filename should be a .txt file")
  }
  
  # if(!dir.exists(fileToSave)){
  #   dir.create(fileToSave)
  # }
  # 
  #if(file.exists(fileToSave)){
  #  warning("File already exists!")
  #  overwrite <- menu(c("Yes", "No"), title="Should it be overwritten?")  
  #  if (overwrite==2){
  #    stop("Operation interrupted by user. Declare a different file name or location.") 
  #  }
  #}
  
  data <- dplyr::as_tibble(covariateDataObject$covariates)
  
  message("Extracting temporal data...")
  temporalData <- getTemporalInputFromFeatExtract(data)
  
  message("Extracting covariate names...")
  NamesData <- getNamesFromCovariateId(data = temporalData, covariateDataObject = covariateDataObject, fileToSave = fileToSave)
  
  message("Generating input file for frequent pattern mining...")
  getInputDataForFrequentPatterns(data = NamesData, filename = fileToSave)
  
  return(NamesData)
}
