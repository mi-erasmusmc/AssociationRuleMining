getTemporalInputFromFeatExtract <- function(data){
  x <- data %>%
    dplyr::group_by(rowId, timeId) %>%
    dplyr::arrange(rowId, timeId) %>%
    dplyr::mutate(covariateId = paste0(covariateId, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(rowId, timeId, covariateId) %>%
    dplyr::summarize(SIZE = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(eventId = sequence(rle(as.character(rowId))$lengths)) %>%
    dplyr::select(rowId, eventId, SIZE, covariateId)
  return(x)
}
