createFrequentPatternsCovariateSettings <- function(useFrequentPatterns = TRUE, 
                                                    algorithm,
                                                    inputFile, 
                                                    outputFile, 
                                                    minsup, 
                                                    minLength = 1,
                                                    maxLength = Inf, 
                                                    maxGap = Inf, 
                                                    showID = TRUE, 
                                                    temporalCovariateSettings, 
                                                    cohortDatabaseSchema, 
                                                    #cohortTable, 
                                                    #cohortId,
                                                    #rowIdField, 
                                                    isCohortTableTemp = FALSE) {
  covariateSettings <- list(useFrequentPatterns = useFrequentPatterns, 
                            temporalCovariateSettings = temporalCovariateSettings, 
                            algorithm = algorithm, 
                            inputFile = inputFile, 
                            outputFile = outputFile, 
                            minsup = minsup,
                            minLength = minLength, 
                            maxLength = maxLength, 
                            maxGap = maxGap, 
                            showID = showID, 
                            cohortDatabaseSchema = cohortDatabaseSchema, 
                            #cohortTable = cohortTable,
                            #cohortId = cohortId,
                            #rowIdField = rowIdField,
                            isCohortTableTemp = isCohortTableTemp)
  attr(covariateSettings, "fun") <- "getFrequentPatternsCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
