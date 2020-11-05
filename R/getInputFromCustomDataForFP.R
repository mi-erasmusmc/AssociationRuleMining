getInputFromCustomDataForFP <- function(data = data){
  dataframe = data
  x <- dataframe %>%
    dplyr::group_by(ID, CONDITION_START_DATE) %>%
    dplyr::summarize(SIZE = dplyr::n(),
                     CONCEPT = paste(as.character(CONCEPT_NAME), collapse = ';')) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(eventId = sequence(rle(as.character(ID))$lengths)) %>%
    dplyr::select(ID, eventId, SIZE, CONCEPT) %>%
    dplyr::rename(sequenceID = ID, 
                  items = CONCEPT) %>%
    dplyr::arrange(sequenceID, eventId)
  
  return(x)
  
}