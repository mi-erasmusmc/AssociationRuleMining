#' Run Frequent Patterns algorithms
#'
#' \code{runFrequentPatterns} returns the extracted frequent patterns.
#'
#' The function takes a .txt file as input and returns the results in a .txt file as well as a tibble. 
#' 
#' @param algorithm One between "SPAM", "SPADE", "prefixSpan", "CM-SPADE", "CM-SPAM", "Clasp", "CM-Clasp", "BIDE+", "VMSP", "VGEN", "RuleGrowth", "ERMiner", "TNS", "TopSeqRules". 
#' @param inputFile Name of the .txt file to be processed.
#' @param outputFile Name of .txt file to extract results.
#' @param minsup Value of minimum support between 0-1.
#' @param minconf Value of minimum confidence. Only some algorithms are making use of this (TO be specified)
#' @export

runFrequentPatterns <- function(algorithm, inputFile, outputFile, minsup, minconf, minLength = 1 , maxLength = Inf , maxGap = Inf, maxAntecedentLength = Inf, maxConsequentLength = 1, showID = FALSE) {
  datafile <- read.delim(paste(inputFile), header = FALSE, blank.lines.skip = TRUE, comment.char = "@")
  noTIDs <- nrow(datafile)
  
  cat(paste("Analysing", noTIDs, "sequence IDs...", sep = " "))
  
  cat("Running frequent pattern algorithm...")
  getFrequentPatterns(algorithm, inputFile = inputFile, outputFile = outputFile, minsup = minsup, minconf = minconf, minLength, maxLength, maxGap, maxAntecedentLength, maxConsequentLength, showID = showID)
  
  cat("Preparing output...")
  getOutputFromFrequentPatterns(inputFile = outputFile, numberOfSequenceIDs = noTIDs, showID)
}
