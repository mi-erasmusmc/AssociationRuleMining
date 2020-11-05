getInputDataForAssociationRules <- function(data, filename){
  if(filename == ""){
    stop("Must declare a filename")
  } 
  
  if(grepl("\\.txt$", filename)== FALSE){
    stop("Filename should be a .txt file")
  }
  
  x <- data %>%
    dplyr::arrange(rowId, SPMFinputId) %>%
    dplyr::group_by(rowId) %>%
    dplyr::summarise(conditionbySPMFinputId = paste0(SPMFinputId, collapse = " ")) %>% 
    dplyr::select(conditionbySPMFinputId) %>%
    write.table(., file = paste(filename), col.names = FALSE, quote = FALSE, row.names = FALSE, append = TRUE)
  
  message(paste("Input data has been created succesfully and saved in", filename))
}