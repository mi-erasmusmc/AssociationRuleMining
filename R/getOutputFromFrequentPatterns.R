getOutputFromFrequentPatterns <- function(inputFile, numberOfSequenceIDs, showID) {
  inputfile = read.delim(inputFile, header = FALSE)
  
  if (showID == FALSE){
    x <- inputfile %>%
      dplyr::mutate(Count = stringr::str_replace_all(V1, ".*: ", ""),
                    Sequence = stringr::str_replace_all(V1, ".#.*", ""), 
                    Support = as.numeric(Count)/numberOfSequenceIDs)
    
    x$Sequence <- stringr::str_replace_all(x$Sequence, " -1", " =>") 
    x$Sequence <- stringr::str_replace_all(x$Sequence, "_", " ") 
    x$Sequence <- stringr::str_replace_all(x$Sequence, "=>$", "")
  } else {
    x <- inputfile %>%
      dplyr::mutate(Count = stringr::str_match(inputfile$V1, "#SUP: \\s*(.*?)\\s*#SID:")[,2],
                    Sequence = stringr::str_replace_all(V1, ".#.*", ""), 
                    Support = as.numeric(Count)/numberOfSequenceIDs)
    
    x$Sequence <- stringr::str_replace_all(x$Sequence, " -1", " =>") 
    x$Sequence <- stringr::str_replace_all(x$Sequence, "_", " ") 
    x$Sequence <- stringr::str_replace_all(x$Sequence, "=>$", "")
  }
  
  x <- dplyr::select(x, c(Sequence, Count, Support))
  return(x)
}
