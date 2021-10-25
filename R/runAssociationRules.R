runAssociationRules <- function(algorithm, inputFile, outputFile, minsup, minconf, showID = FALSE) {
  
  datafile <- read.delim(paste(inputFile), header = FALSE, blank.lines.skip = TRUE, comment.char = "@")
  noTIDs <- nrow(datafile)
  
  cat(paste("Analysing", noTIDs, "transaction IDs...", sep = " "))
  
  cat("Running Association Rules Algorithm...")
  getAssociationSets(algorithm, inputFile, outputFile, minsup, minconf, showID = FALSE)
  
  cat("Preparing output...")
  getOutputFromAssociationRules(outputFile, noTIDs)
}