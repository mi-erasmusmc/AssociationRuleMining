getFrequentPatterns <- function(algorithm, inputFile, outputFile, minsup, minconf, minLength = 1, maxLength = Inf, maxGap = Inf, maxAntecedentLength = Inf, maxConsequentLength = 1, top_k = 1, delta = 0, showID = FALSE) {
  
  spmf.dir <- rJava::.jclassPath()[stringr::str_ends(rJava::.jclassPath(), "spmf.jar")]
  frequentSequencesAlgorithms <- c("SPAM", "SPADE", "prefixSpan", "Clasp", "CM-Clasp", "VMSP", "VGEN", "RuleGrowth", "ERMiner")
  #`%notin%` <- Negate(`%in%`)
  outputID = paste(tolower(showID))
  #maxLengthvalue = jdx::convertToJava(maxLength, scalars.as.objects = TRUE)
  #maxGapvalue = jdx::convertToJava(maxGap, scalars.as.objects = TRUE)
  if (maxLength == Inf) {
  maxLengthValue = as.integer(1000000000)
  } else {
    maxLengthValue = maxLength
  }
  if (maxGap == Inf){ 
    maxGapValue = as.integer(1000000000)
  } else{
    maxGapValue = maxGap
  }
  if (maxAntecedentLength == Inf){
  maxAntecedentLength = as.integer(1000000000)
  } else {
    maxAntecedentLength
  }
  
  maxConsequentLength = maxConsequentLength
  
  if(!all(algorithm %in% frequentSequencesAlgorithms)){
    stop("Algorithm is not supported at the moment!")
  }
  
  #Below I replaced the Inf values for maxLength and maxGap with 1000 since Inf is not a Java object for SPAM and prefixSpan
    # Frequent Pattern Mining
  if (algorithm == "SPAM" ) {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, minLength, maxLengthValue, maxGapValue, outputID, sep = " ")
  } else if (algorithm == "SPADE") {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, outputID, sep = " ")
  } else if (algorithm == "prefixSpan") {
    executable <- paste("java -jar", spmf.dir, "run", "PrefixSpan", inputFile, outputFile, minsup, maxLengthValue, outputID, sep = " ")
  } else if (algorithm == "CM-SPADE") {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, outputID, sep = " ")
  } else if (algorithm == "CM-SPAM") {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, minLength, maxLengthValue, maxGapValue, outputID, sep = " ")
  } else if (algorithm == "Clasp") {
    # Closed patterns
     executable <- paste("java -jar", spmf.dir, "run", "ClaSP", inputFile, outputFile, minsup, outputID, sep = " ")
  } else if (algorithm == "CM-Clasp"){
     executable <- paste("java -jar", spmf.dir, "run", "CM-ClaSP", inputFile, outputFile, minsup, outputID, sep = " ")
  } else if (algorithm == "Bide+") {
    executable <- paste("java -jar", spmf.dir, "run", "BIDE+", inputFile, outputFile, minsup, outputID, sep = " ")
  } else if (algorithm == "VMSP") {
    # Maximal patters
    executable <- paste("java -jar", spmf.dir, "run", "VMSP", inputFile, outputFile, minsup, maxLengthValue, maxGapValue, outputID, sep = " ")
  } else if (algorithm == "VGEN") {
    # Sequential generators
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, maxLengthValue, maxGapValue, outputID, sep = " ")
  } else if (algorithm == "RuleGrowth") {
    # Sequential Rules
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, minconf, maxAntecedentLength, maxConsequentLength, outputID, sep = " ")
  } else if (algorithm == "ERMiner") {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, minconf, maxAntecedentLength, maxConsequentLength, outputID, sep = " ")
  } else if (algorithm == "TNS") {
    # Top K sequential rules
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, top_k, minconf, delta, sep = " ")
  } else if (algorithm == "TopSeqRules") {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, top_k, minconf, sep = " ") 
  }
    

  cat(paste("The command line that has been running is:", print(executable)))
  system(executable)
}
