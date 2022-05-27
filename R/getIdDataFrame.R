#' Get a dataframe indicating which patterns are observed in each patient
#' 
#' \code{getIdDataFrame} returns a data frame with patients as rows and frequent patterns as columns.
#' 
#' @param inputFile The .txt file returned from `runFrequentPatterns()`.
#' @param objectWithIds The tibble returned from `getInputFileFromFrequentPatterns()`.
#' 
#' @export
getIdDataFrame <- function(inputFile, objectWithIds){
  
  #inputs to this function are: 1) inputfile: the results of the FP mining algorithms
  #                             2) an object with the true Ids. This is created when running getInputFileForFrequentPatterns()
  
  if (is.unsorted(objectWithIds$rowId) == TRUE || is.unsorted(objectWithIds$SPMFrowId) == TRUE){
    message("One of the rowId or SPMFrowId is not sorted. True row ids may differ. Please contact the package owner.")
  }
  
  inputfile = read.delim(inputFile, header = FALSE)
  
  if (any(stringi::stri_detect_fixed(inputfile$V1, "#SID", max_count = 1)) == FALSE) {
    stop("The input file provided does not contain sequence IDs")
  }
  
  y <- inputfile %>%
    dplyr::mutate(id = stringr::str_replace_all(V1, ".*SID: ", ""), 
                  Seqs = stringr::str_replace_all(V1, ".#.*", "")) %>%
    dplyr::select(Seqs, id) %>%
    tidyr::separate_rows(id, sep = " ") %>%
    dplyr::mutate(val=1) %>% 
    tidyr::pivot_wider(names_from = Seqs, values_from = val) %>% 
    dplyr::mutate(dplyr::across(-id, ~tidyr::replace_na(.x, 0))) %>%
    dplyr::arrange(as.numeric(id)) #%>%
    #dplyr::mutate(across(-id, ~ifelse(.x==1, TRUE,FALSE)))
  
  y$id <- as.numeric(y$id)
  
  # Getting the true IDs in a dtaframe object
  trueId <- objectWithIds %>%
    select("rowId", "SPMFrowId") 
  
  trueIdDf <- data.frame("rowId" = unique(trueId$rowId),
                         "SPMFrowId" = unique(trueId$SPMFrowId))
  
  # Merging the true IDs df with the patterns a covariates
  output <- full_join(trueIdDf, y,by = c("SPMFrowId" = "id")) %>%
    select(-"SPMFrowId")
  
  names(output) <- stringr::str_replace_all(names(output), "-1", "=>")
  names(output) <- stringr::str_replace_all(names(output), "=>$", "")

  return(output)
}
