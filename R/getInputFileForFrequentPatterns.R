#' Prepare input data for JAVA algorithms
#'
#'\code{getInputFileForFrequentPatterns} returns a tibble with input data in long format and a .txt file at the directory specified.
#'
#' The function takes a covariateData object as returned by the `FeatureExtraction` package and a location or name of file (should be .txt file) to save the input data.
#' 
#' @param covariateDataObject A covariateData object with temporal information as returned by `FeatureExtraction`. 
#' @param fileToSave Location and name of file to save the input data. Should be a .txt file.
#' 
#' @return A tibble and a .txt file at the location specified.
#' 
#' @export
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

#' @export
getInputFileForCSpade <- function(covariateDataObject, fileToSave) {
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
  
  data <- as.data.frame(covariateDataObject$covariates) %>%
    getTemporalInputFromFeatExtract(data = .) 
  
  tidyData <- data %>%
    getNamesFromCovariateId(., covariateDataObject = covariateDataObject, fileToSave = fileToSave) %>%
    arrange(rowId, eventId) %>%
    #select(c(rowId, eventId, SIZE, covariateLabel)) %>% 
    group_by(rowId, eventId, SIZE, SPMFrowId) %>%
    summarise(covariateLabel2 = paste(covariateLabel, collapse = ";")) %>% 
    ungroup() %>%
    #group_by(rowId) %>%
    mutate(cspadeRowId = SPMFrowId + 1) %>%
    distinct()
    
  
  trans <- as(tidyData[,"covariateLabel2", drop = FALSE], "transactions")
  transactionInfo(trans)$sequenceID <- tidyData$cspadeRowId
  transactionInfo(trans)$eventID <- tidyData$eventId
  

  #trans <- trans[order(transactionInfo(trans)$sequenceID), ]
  
  
  return(trans)
}
