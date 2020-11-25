createFrequentPatternsCovariateSettings <- function(useFrequentPatterns = TRUE, 
                                                    algorithm,
                                                    inputFile, 
                                                    outputFile, 
                                                    minsup, 
                                                    minLength = 1,
                                                    maxLength = Inf, 
                                                    maxGap = Inf, 
                                                    showID = TRUE, 
                                                    temporalCovariateSettings) {
  covariateSettings <- list(useFrequentPatterns = useFrequentPatterns, 
                            temporalCovariateSettings = temporalCovariateSettings, 
                            algorithm = algorithm, 
                            inputFile = inputFile, 
                            outputFile = outputFile, 
                            minsup = minsup,
                            minLength = minLength, 
                            maxLength = maxLength, 
                            maxGap = maxGap, 
                            showID = showID)
  attr(covariateSettings, "fun") <- "getFrequentPatternsCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}