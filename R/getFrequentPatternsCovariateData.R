getFrequentPatternsCovariateData <- function(connection, 
                                             oracleTempSchema = NULL,
                                             cdmDatabaseSchema,
                                             cohortTable,
                                             cohortId,
                                             cdmVersion = "5",
                                             rowIdField = "subject_id",
                                             covariateSettings,
                                             temporalCovariateSettings,
                                             cohortTableIsTemp = FALSE,
                                             aggregated = FALSE) {
  writeLines("Constructing frequent patterns covariates")
  if (covariateSettings$useFrequentPatterns == FALSE) {
    return(NULL)
  }
  if (aggregated)
    stop("Aggregation not supported")
  
  if (covariateSettings$showID == FALSE) 
    stop("Argument 'ShowId' is FALSE. No reason to run this analysis.")
  
  # Extracting covariates 
  
  temporalData <- FeatureExtraction::getDbCovariateData(connection = connection, 
                                                        cdmDatabaseSchema = cdmdatabaseschema, 
                                                        cohortDatabaseSchema = resultsdatabaseschema, 
                                                        cohortTable = cohortTable, 
                                                        rowIdField = rowIdField, 
                                                        covariateSettings = temporalCovariateSettings, 
                                                        cohortTableIsTemp = cohortTableIsTemp)
  
  # Preparing dataset
  
  input <- getInputFileForFrequentPatterns(covariateDataObject = temporalData, fileToSave = covariateSettings$inputFile)
  
  # Getting frequent patterns
  
  frequentPatterns <- runFrequentPatterns(algorithm = covariateSettings$algorithm, 
                                          inputFile = covariateSettings$inputFile, 
                                          outputFile = covariateSettings$outputFile, 
                                          minsup = covariateSettings$minsup, 
                                          showID = covariateSettings$showID)
  
  # Converting to FeatureExtraction object 
  fpData <- toCovariateData(inputFile = covariateSettings$outputFile, objectWithIds = input)
  
  assign("frequentPatternsData", frequentPatterns, envir = .GlobalEnv)
  return(fpData)
}

