getOutputFromAssociationRules <- function(inputFile, numberOfTransactionIDs) {
  inputfile = read.delim(inputFile, header = FALSE, blank.lines.skip = TRUE)
  x <- inputfile %>%
    dplyr::mutate(Count = stringr::str_replace_all(V1, ".*: ", ""),
                  Set = stringr::str_replace_all(V1, ".#.*", ""), 
                  Support = as.numeric(Count)/numberOfTransactionIDs)
  
  x$Set <- stringr::str_replace_all(x$Set, " ", ", ") 
  x$Set <- stringr::str_replace_all(x$Set, "_", " ") 
  x$Set <- stringr::str_replace_all(x$Set, ", $", "")
  x <- dplyr::select(x, c(Set, Count, Support))
  return(x)
}