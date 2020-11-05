getOutputFromAssociationRules <- function(inputFile) {
  inputfile = read.delim(inputFile, header = FALSE)
  x <- inputfile %>%
    mutate(Support = stringr::str_replace_all(V1, ".*: ", ""),
           Set = stringr::str_replace_all(V1, ".#.*", ""))
  
  x$Set <- stringr::str_replace_all(x$Set, " ", ", ") 
  x$Set <- stringr::str_replace_all(x$Set, "_", " ") 
  x$Set <- stringr::str_replace_all(x$Set, ", $", "")
  x <- dplyr::select(x, c(Set, Support))
  return(x)
}