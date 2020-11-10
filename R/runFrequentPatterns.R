runFrequentPatterns <- function(algorithm, inputFile, outputFile, minsup, minLength = 1 , maxLength = Inf , maxGap = Inf, showID = FALSE) {
  datafile <- read.delim(paste(inputFile), header = FALSE, blank.lines.skip = TRUE, comment.char = "@")
  noTIDs <- nrow(datafile)
  
  cat(paste("Analysing", noTIDs, "sequence IDs...", sep = " "))
  
  cat("Running frequent pattern algorithm...")
  getFrequentPatterns(algorithm, inputFile = inputFile, outputFile = outputFile, minsup = minsup, showID = showID)
  
  cat("Preparing output...")
  getOutputFromFrequentPatterns(inputFile = outputFile, numberOfSequenceIDs = noTIDs, showID)
}
