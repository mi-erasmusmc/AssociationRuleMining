getFrequentPatternsCovariateData <- function(connection, 
                                             oracleTempSchema = NULL,
                                             cdmDatabaseSchema,
                                             cohortTable,
                                             cohortId,
                                             cdmVersion = "5",
                                             rowIdField,
                                             covariateSettings,
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
                                                        cdmDatabaseSchema = cdmDatabaseSchema, 
                                                        cohortDatabaseSchema = covariateSettings$cohortDatabaseSchema, 
                                                        cohortTable = cohortTable, 
                                                        cohortId = cohortId, 
                                                        rowIdField = rowIdField, 
                                                        covariateSettings = covariateSettings$temporalCovariateSettings, 
                                                        cohortTableIsTemp = covariateSettings$isCohortTableTemp) 
  
  # Preparing dataset
  writeLines("Preparing input for running frequent pattern analysis...")
  input <- getInputFileForFrequentPatterns(covariateDataObject = temporalData, fileToSave = covariateSettings$inputFile)
  
  # Getting frequent patterns
  writeLines("Running frequent pattern analysis...")
  frequentPatterns <- runFrequentPatterns(algorithm = covariateSettings$algorithm, 
                                          inputFile = covariateSettings$inputFile, 
                                          outputFile = covariateSettings$outputFile, 
                                          minsup = covariateSettings$minsup, 
                                          showID = covariateSettings$showID)
  
  # Converting to FeatureExtraction object
  writeLines("Creating covariates out the extracted patterns...")
  fpData <- toCovariateData(inputFile = covariateSettings$outputFile, objectWithIds = input)
  
  assign("frequentPatternsData", frequentPatterns, envir = .GlobalEnv)
  return(fpData)
}

