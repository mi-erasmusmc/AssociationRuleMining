getOutputFromFrequentPatterns <- function(inputFile, numberOfSequenceIDs, showID) {
  inputfile = read.delim(inputFile, header = FALSE)
  
  # Condition when agorithm extracted Rules
  if (any(stringi::stri_detect_fixed(inputfile$V1, "#CONF", max_count = 1) == TRUE) == TRUE) {
    confidenceIncluded <- TRUE
  } else {
    confidenceIncluded <- FALSE
  } 
  
  if (showID == FALSE && confidenceIncluded == FALSE){
    x <- inputfile %>%
      dplyr::mutate(Count = stringr::str_replace_all(V1, ".*: ", ""),
                    Sequence = stringr::str_replace_all(V1, ".#.*", ""), 
                    Support = as.numeric(Count)/numberOfSequenceIDs) %>%
      dplyr::select(Sequence, Count, Support)
    
    x$Sequence <- stringr::str_replace_all(x$Sequence, " -1", " =>") 
    x$Sequence <- stringr::str_replace_all(x$Sequence, "_", " ") 
    x$Sequence <- stringr::str_replace_all(x$Sequence, "=>$", "")
    
  } else if (showID == TRUE && confidenceIncluded == FALSE) {
    x <- inputfile %>%
      dplyr::mutate(Count = stringr::str_match(inputfile$V1, "#SUP: \\s*(.*?)\\s*#SID:")[,2],
                    Sequence = stringr::str_replace_all(V1, ".#.*", ""), 
                    Support = as.numeric(Count)/numberOfSequenceIDs) %>%
      dplyr::select(Sequence, Count, Support)
    
    x$Sequence <- stringr::str_replace_all(x$Sequence, " -1", " =>") 
    x$Sequence <- stringr::str_replace_all(x$Sequence, "_", " ") 
    x$Sequence <- stringr::str_replace_all(x$Sequence, "=>$", "")
    
  } else if (confidenceIncluded == TRUE) {
    x <- inputfile %>%
      dplyr::mutate(Count = stringr::str_match(V1, "#SUP: \\s*(.*?)\\s*#CONF:")[,2],
                    Sequence = stringr::str_replace_all(V1, ".#.*", ""), 
                    Support = as.numeric(Count)/numberOfSequenceIDs,
                    Confidence = round(as.numeric(stringr::str_replace_all(V1, ".*CONF: ", "")), digits = 7)) %>%
      dplyr::select(Sequence, Count, Support, Confidence)
  }
  
  #x <- dplyr::select(x, c(Sequence, Count, everything()))
  return(x)
}
