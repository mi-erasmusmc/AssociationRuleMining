runFrequentPatterns <- function(algorithm, inputFile, outputFile, minsup, minLength = 1 , maxLength = Inf , maxGap = Inf, showID = FALSE) {
  datafile <- read.delim(paste(inputFile), header = FALSE, blank.lines.skip = TRUE, comment.char = "@")
  noTIDs <- nrow(datafile)
  
  message(paste("Analysing", noTIDs, "sequence IDs...", sep = " "))
  
  message("Running frequent pattern algorithm...")
  getFrequentPatterns(algorithm, inputFile = inputFile, outputFile = outputFile, minsup = minsup, showID = showID)
  
  getOutputFromFrequentPatterns(inputFile = outputFile, numberOfSequenceIDs = noTIDs)
}