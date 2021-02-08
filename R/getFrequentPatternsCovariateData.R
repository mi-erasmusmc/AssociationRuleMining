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
  t1start <- Sys.time()
  temporalData <- FeatureExtraction::getDbCovariateData(connection = connection, 
                                                        cdmDatabaseSchema = cdmDatabaseSchema, 
                                                        cohortDatabaseSchema = covariateSettings$cohortDatabaseSchema, 
                                                        cohortTable = cohortTable, 
                                                        cohortId = cohortId, 
                                                        rowIdField = rowIdField, 
                                                        covariateSettings = covariateSettings$temporalCovariateSettings, 
                                                        cohortTableIsTemp = covariateSettings$isCohortTableTemp) 
  
  t1duration <- Sys.time() - t1start
  
  message(paste0("Extracting covariates took", round(t1duration, 2), "minutes."))
  
  # Saving covariate data object 
  ## this should be saveed in a formal location folder ie. "analysis folder" as it is a costly operation
  #FeatureExtraction::saveCovariateData(temporalData, file = "temporalCovariateData")
  
  # Preparing dataset
  writeLines("Preparing input for running frequent pattern analysis...")
  
  t2start <- Sys.time()
  
  input <- getInputFileForFrequentPatterns(covariateDataObject = temporalData, fileToSave = covariateSettings$inputFile)
  
  t2duration <- Sys.time() - t2start
  
  message(paste0("Preparing input dataset took", round(t2duration, 2), "minutes."))
  
  # Getting frequent patterns
  writeLines("Running frequent pattern analysis...")
  
  t3start <- Sys.time()
  
  frequentPatterns <- runFrequentPatterns(algorithm = covariateSettings$algorithm, 
                                          inputFile = covariateSettings$inputFile, 
                                          outputFile = covariateSettings$outputFile, 
                                          minsup = covariateSettings$minsup, 
                                          showID = covariateSettings$showID)
  
  t3duration <- Sys.time() - t3start
  
  message(paste0("Extracting frequent patterns took", round(t3duration, 2), "minutes."))
  
  # Converting to FeatureExtraction object
  writeLines("Creating covariates out the extracted patterns...")
  
  t4start <- Sys.time()
  
  fpData <- toCovariateData(inputFile = covariateSettings$outputFile, objectWithIds = input)
  
  t4duration <- Sys.time() - t4start 
  
  message(paste0("Creating covariate data object took", round(t4duration, 2), "minutes."))
  
  assign("frequentPatternsData", frequentPatterns, envir = .GlobalEnv)
  return(fpData)
}

