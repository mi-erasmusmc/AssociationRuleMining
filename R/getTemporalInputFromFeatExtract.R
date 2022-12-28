getTemporalInputFromFeatExtract <- function(data){
  
  x <- data %>%
    dplyr::group_by(rowId, timeId) %>%
    dplyr::arrange(rowId, desc(timeId)) %>%
    dplyr::mutate(covariateId = paste0(covariateId, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(rowId, timeId, covariateId) %>%
    dplyr::summarize(SIZE = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(rowId) %>%
    dplyr::arrange(desc(timeId)) %>%
    dplyr::mutate(eventId = dplyr::row_number())%>%
    dplyr::arrange(rowId)%>%
    dplyr::select(rowId, eventId, timeId, SIZE, covariateId)
  
  return(x)
}
