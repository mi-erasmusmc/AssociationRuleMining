getOutputFromFrequentPatterns <- function(inputFile, numberOfSequenceIDs) {
  inputfile = read.delim(inputFile, header = FALSE)
  
  #From the code below 67 should change. in its place, number of sequence ids should be enetered
  x <- inputfile %>%
    mutate(Count = stringr::str_replace_all(V1, ".*: ", ""),
           Sequence = stringr::str_replace_all(V1, ".#.*", ""), 
           Support = as.numeric(Count)/numberOfSequenceIDs)
  
  x$Sequence <- stringr::str_replace_all(x$Sequence, " -1", " =>") 
  x$Sequence <- stringr::str_replace_all(x$Sequence, "_", " ") 
  x$Sequence <- stringr::str_replace_all(x$Sequence, "=>$", "")
  x <- dplyr::select(x, c(Sequence, Count, Support))
  return(x)
}