runFrequentPatterns <- function(algorithm, inputFile, outputFile, minsup, minconf, minLength = 1 , maxLength = Inf , maxGap = Inf, maxAntecedentLength = Inf, maxConsequentLength = 1, showID = FALSE) {
  datafile <- read.delim(paste(inputFile), header = FALSE, blank.lines.skip = TRUE, comment.char = "@")
  noTIDs <- nrow(datafile)
  
  cat(paste("Analysing", noTIDs, "sequence IDs...", sep = " "))
  
  cat("Running frequent pattern algorithm...")
  getFrequentPatterns(algorithm, inputFile = inputFile, outputFile = outputFile, minsup = minsup, minconf = minconf, minLength, maxLength, maxGap, maxAntecedentLength, maxConsequentLength, showID = showID)
  
  cat("Preparing output...")
  getOutputFromFrequentPatterns(inputFile = outputFile, numberOfSequenceIDs = noTIDs, showID)
}
