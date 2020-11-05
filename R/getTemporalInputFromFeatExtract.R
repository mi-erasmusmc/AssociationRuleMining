getInputFromFeatExtract <- function(data){
  dataframe = data
  x <- dataframe %>%
    dplyr::group_by(rowId, timeId) %>%
    dplyr::arrange(rowId, timeId) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(eventId = sequence(rle(as.character(rowId))$lengths)) %>% 
    dplyr::group_by(rowId, timeId, covariateId, covariateValue, eventId) %>%
    dplyr::summarize(SIZE = n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(rowId, eventId, SIZE, covariateId)
  return(x)
}
