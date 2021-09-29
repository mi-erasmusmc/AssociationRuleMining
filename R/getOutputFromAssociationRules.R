getOutputFromAssociationRules <- function(inputFile, numberOfTransactionIDs) {
  inputfile = read.delim(inputFile, header = FALSE, blank.lines.skip = TRUE)
  
# condition when algorithm extracted rules  
  if (any(stringi::stri_detect_fixed(inputfile$V1, "#CONF", max_count = 1) == TRUE) == TRUE) {
    confidenceIncluded <- TRUE
  } else {
    confidenceIncluded <- FALSE
  } 

  if (confidenceIncluded == FALSE) {  
  x <- inputfile %>%
    dplyr::mutate(Count = stringr::str_replace_all(V1, ".*: ", ""),
                  Set = stringr::str_replace_all(V1, ".#.*", ""), 
                  Support = as.numeric(Count)/numberOfTransactionIDs)
  
  x$Set <- stringr::str_replace_all(x$Set, " ", ", ") 
  x$Set <- stringr::str_replace_all(x$Set, "_", " ") 
  x$Set <- stringr::str_replace_all(x$Set, ", $", "")
  x <- dplyr::select(x, c(Set, Count, Support))
  return(x)
  } else if (confidenceIncluded == TRUE){
    x <- inputfile %>%
      dplyr::mutate(Count = stringr::str_match(V1, "#SUP: \\s*(.*?)\\s*#CONF:")[,2],
                    Rule = gsub(x = V1, pattern = ".#.*", replacement = ""), 
                    Support = as.numeric(Count)/numberOfTransactionIDs,
                    Confidence = round(as.numeric(gsub(x = V1, pattern = ".*CONF: ", replacement = "")), digits = 7)) %>%
      dplyr::select(Rule, Count, Support, Confidence)
    
    x$Rule <- stringr::str_replace_all(x$Rule, " ", ", ") 
    x$Rule <- stringr::str_replace_all(x$Rule, "_", " ") 
    x$Rule <- stringr::str_replace_all(x$Rule, ", $", "")
    x <- dplyr::select(x, c(Rule, Count, Support, Confidence))
    return(x)
  }
}