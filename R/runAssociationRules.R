runAssociationRules <- function(algorithm, inputFile, outputFile, minsup, showID = FALSE) {
  
  datafile <- read.delim(paste(inputFile), header = FALSE, blank.lines.skip = TRUE, comment.char = "@")
  noTIDs <- nrow(datafile)
  
  message(paste("Analysing", noTIDs, "transaction IDs...", sep = " "))
  
  getAssociationSets(algorithm, inputFile, outputFile, minsup, showID = FALSE)
  
  getOutputFromAssociationRules(outputFile, noTIDs)
}