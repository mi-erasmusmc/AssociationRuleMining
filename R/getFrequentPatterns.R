getFrequentPatterns <- function(algorithm, inputFile, outputFile, minsup, minconf, minLength = 1, maxLength = Inf, maxGap = Inf, maxAntecedentLength = Inf, maxConsequentLength = 1, showID = FALSE) {
  
  spmf.dir <- rJava::.jclassPath()[stringr::str_ends(rJava::.jclassPath(), "spmf.jar")]
  frequentSequencesAlgorithms <- c("SPAM", "SPADE", "prefixSpan", "Clasp", "CM-Clasp", "VMSP", "VGEN", "RuleGrowth", "ERMiner")
  #`%notin%` <- Negate(`%in%`)
  outputID = paste(tolower(showID))
  #maxLengthvalue = jdx::convertToJava(maxLength, scalars.as.objects = TRUE)
  #maxGapvalue = jdx::convertToJava(maxGap, scalars.as.objects = TRUE)
  if (maxLength == Inf) {
  maxLengthvalue = as.integer(1000000000)
  }
  if (maxGap == Inf){ 
    maxGapvalue = as.integer(1000000000)
  }
  if (maxAntecedentLength == Inf){
  maxAntecedentLength = as.integer(1000000000)
  }
  
  maxConsequentLength = maxConsequentLength
  
  if(!all(algorithm %in% frequentSequencesAlgorithms)){
    stop("Algorithm is not supported at the moment!")
  }
  
  #Below I replaced the Inf values for maxLength and maxGap with 1000 since Inf is not a Java object for SPAM and prefixSpan
  if (algorithm == "SPAM" ) {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, minLength, maxLength, maxGap, outputID, sep = " ")
  } else if (algorithm == "SPADE") {
      executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, outputID, sep = " ")
  } else if (algorithm == "prefixSpan"){
      executable <- paste("java -jar", spmf.dir, "run", "PrefixSpan", inputFile, outputFile, minsup, maxLength, outputID, sep = " ")
  } else if (algorithm == "Clasp") {
      executable <- paste("java -jar", spmf.dir, "run", "ClaSP", inputFile, outputFile, minsup, outputID, sep = " ")
  } else if (algorithm == "CM-Clasp"){
     executable <- paste("java -jar", spmf.dir, "run", "CM-ClaSP", inputFile, outputFile, minsup, outputID, sep = " ")
  } else if (algorithm == "VMSP") {
    executable <- paste("java -jar", spmf.dir, "run", "VMSP", inputFile, outputFile, minsup, maxLengthvalue, maxGapvalue, outputID, sep = " ")
  } else if (algorithm == "VGEN") {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, maxLengthvalue, maxGapvalue, outputID, sep = " ")
  } else if (algorithm == "RuleGrowth") {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, minconf, maxAntecedentLength, maxConsequentLength, outputID, sep = " ")
  } else if (algorithm == "ERMiner") {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, minconf, maxAntecedentLength, maxConsequentLength, outputID, sep = " ")
  }
    
  

  cat(paste("The command line that has been running is:", print(executable)))
  system(executable)
}
