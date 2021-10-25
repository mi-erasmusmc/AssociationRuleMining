getInputDataForFrequentPatterns <- function(data, filename){
  if(filename == ""){
    stop("Must declare a filename")
  } 
  
  if(grepl("\\.txt$", filename)== FALSE){
    stop("Filename should be a .txt file")
  }
  
  x <- data %>%
    dplyr::arrange(rowId) %>%
    dplyr::group_by(rowId, eventId) %>%
    dplyr::summarise(sequencebySPMFinputId = paste0(SPMFinputId, collapse = " ")) %>% 
    dplyr::ungroup() %>%
    dplyr::group_by(rowId) %>%
    dplyr::summarise(sequencebySPMFinputId = paste0(sequencebySPMFinputId, collapse = " -1 ")) %>%
    dplyr::mutate(sequenceToSPMF = paste(sequencebySPMFinputId, "-2")) %>%
    dplyr::select(sequenceToSPMF)  %>%
    write.table(., file = paste(filename), col.names = FALSE, quote = FALSE, row.names = FALSE, append = TRUE)
  
  message(paste("Input data has been created succesfully and saved in", filename))
}
