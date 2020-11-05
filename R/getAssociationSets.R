getAssociationSets <- function(algorithm, inputFile, outputFile, minsup, showID = FALSE) {
  
  associationAlgorithms <- c("Apriori", "Eclat", "FP-Growth", "Relim")
  `%notin%` <- Negate(`%in%`)
  outputID = paste(tolower(showID))
  #maxLengthvalue = jdx::convertToJava(maxLength, scalars.as.objects = TRUE)
  #maxGapvalue = jdx::convertToJava(maxGap, scalars.as.objects = TRUE)
  maxLengthvalue = 1000
  maxGapvalue = 1000
  
  if(algorithm %notin% associationAlgorithms){
    stop("Algorithm is not supported at the moment!")
  }
  message("Running algorithm")
  
  #Below I replaced the Inf values for maxLength and maxGap with 1000 since Inf is not a Java object for SPAM and prefixSpan
  if (algorithm == "Apriori" ) {
    executable <- paste("java -jar ./inst/java/spmf.jar run", algorithm, inputFile, outputFile, minsup) # Removed outputID but should look up for other parameters since some are applicable
  } else {
    if (algorithm == "Eclat") {
      executable <- paste("java -jar ./inst/java/spmf.jar run", algorithm, inputFile, outputFile, minsup)
    } else {
      if (algorithm == "FP-Growth"){
        executable <- paste("java -jar ./inst/java/spmf.jar run", "FPGrowth", inputFile, outputFile, minsup)
      } else {
       if (algorithm == "Relim"){
         executable <- paste("java -jar ./inst/java/spmf.jar run", "Relim", inputFile, outputFile, misup)
       } 
      } 
    }
  }
  message(paste("The command line that has been running is:", print(executable)))
  system(executable)
}
