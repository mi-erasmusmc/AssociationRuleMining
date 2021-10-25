getOutputFromFrequentPatterns <- function(inputFile, numberOfSequenceIDs, showID) {
  inputfile = vroom::vroom(file = inputFile, col_names = FALSE, col_types = "c", trim_ws = TRUE, progress = TRUE, delim = "///")
  
  # Condition when agorithm extracted Rules
  if (any(stringi::stri_detect_fixed(inputfile$X1, "#CONF", max_count = 1) == TRUE) == TRUE) {
    confidenceIncluded <- TRUE
  } else {
    confidenceIncluded <- FALSE
  } 
  
  if (showID == FALSE && confidenceIncluded == FALSE){
    x <- inputfile %>%
      dplyr::mutate(Count = gsub(x = X1, pattern = ".*: ", replacement = ""),
                    Sequence = gsub(x = X1, pattern = ".#.*", replacement = ""), 
                    Support = as.numeric(Count)/numberOfSequenceIDs) %>%
      dplyr::select(Sequence, Count, Support)
    
    x$Sequence <- gsub(x = x$Sequence, pattern = " -1", replacement = " =>") 
    x$Sequence <- gsub(x = x$Sequence, pattern = "_", replacement = " ") 
    x$Sequence <- gsub(x = x$Sequence, pattern = "=>$", replacement = "")
    
  } else if (showID == TRUE && confidenceIncluded == FALSE) {
    x <- inputfile %>%
      dplyr::mutate(Count = stringr::str_match(inputfile$X1, "#SUP: \\s*(.*?)\\s*#SID:")[,2],
                    Sequence = gsub(x = X1, pattern = ".#.*", replacement = ""), 
                    Support = as.numeric(Count)/numberOfSequenceIDs) %>%
      dplyr::select(Sequence, Count, Support)
    
    x$Sequence <- gsub(x = x$Sequence, pattern = " -1", replacement = " =>") 
    x$Sequence <- gsub(x = x$Sequence, pattern = "_", replacement = " ") 
    x$Sequence <- gsub(x = x$Sequence, pattern = "=>$", replacement = "")
    
  } else if (confidenceIncluded == TRUE) {
    x <- inputfile %>%
      dplyr::mutate(Count = stringr::str_match(X1, "#SUP: \\s*(.*?)\\s*#CONF:")[,2],
                    Sequence = gsub(x = X1, pattern = ".#.*", replacement = ""), 
                    Support = as.numeric(Count)/numberOfSequenceIDs,
                    Confidence = round(as.numeric(gsub(x = X1, pattern = ".*CONF: ", replacement = "")), digits = 7)) %>%
      dplyr::select(Sequence, Count, Support, Confidence)
  }
  
  #x <- dplyr::select(x, c(Sequence, Count, everything()))
  return(x)
}
