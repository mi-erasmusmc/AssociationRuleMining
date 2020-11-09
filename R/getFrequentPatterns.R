getFrequentPatterns <- function(algorithm, inputFile, outputFile, minsup, minLength = 1 , maxLength = Inf , maxGap = Inf, showID = FALSE) {
  
  spmf.dir <- .jclassPath()[str_ends(.jclassPath(), "spmf.jar")]
  frequentsequencesAlgorithms <- c("SPAM", "SPADE", "prefixSpan")
  `%notin%` <- Negate(`%in%`)
  outputID = paste(tolower(showID))
  #maxLengthvalue = jdx::convertToJava(maxLength, scalars.as.objects = TRUE)
  #maxGapvalue = jdx::convertToJava(maxGap, scalars.as.objects = TRUE)
  maxLengthvalue = 1000
  maxGapvalue = 1000
  
  if(algorithm %notin% frequentsequencesAlgorithms){
    stop("Algorithm is not supported at the moment!")
  }
  
  #Below I replaced the Inf values for maxLength and maxGap with 1000 since Inf is not a Java object for SPAM and prefixSpan
  if (algorithm == "SPAM" ) {
    executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, minLength, 1000, 1000, outputID, sep = " ")
  } else {
    if (algorithm == "SPADE") {
      executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, outputID, sep = " ")
    } else {
      if (algorithm == "prefixSpan"){
        executable <- paste("java -jar", spmf.dir, "run", algorithm, inputFile, outputFile, minsup, 1000, outputID, sep = " ")
      }  
    }
  }
  cat(paste("The command line that has been running is:", print(executable)))
  system(executable)
}
