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
  
  writeLines(paste("Extracting covariates took", round(t1duration, 2), paste0(attr(t1duration, which = "units"), "."), sep = " "))
  
  # Saving covariate data object 
  ## this should be saveed in a formal location folder ie. "analysis folder" as it is a costly operation
  #FeatureExtraction::saveCovariateData(temporalData, file = "temporalCovariateData")
  
  # Preparing dataset
  writeLines("Preparing input for running frequent pattern analysis...")
  
  t2start <- Sys.time()
  
  input <- getInputFileForFrequentPatterns(covariateDataObject = temporalData, fileToSave = covariateSettings$inputFile)
  
  t2duration <- Sys.time() - t2start
  
  writeLines(paste("Preparing input dataset took", round(t2duration, 2), paste0(attr(t2duration, which = "units"), "."), sep = " "))
  
  # Getting frequent patterns
  writeLines("Running frequent pattern analysis...")
  
  t3start <- Sys.time()
  
  frequentPatterns <- runFrequentPatterns(algorithm = covariateSettings$algorithm, 
                                          inputFile = covariateSettings$inputFile, 
                                          outputFile = covariateSettings$outputFile, 
                                          minsup = covariateSettings$minsup, 
                                          showID = covariateSettings$showID)
  
  t3duration <- Sys.time() - t3start
  
  writeLines(paste("\nExtracting frequent patterns took", round(t3duration, 2), paste0(attr(t3duration, which = "units"), "."), sep = " "))
  
  # Converting to FeatureExtraction object
  writeLines("Creating covariates out of the extracted patterns...")
  
  t4start <- Sys.time()
  
  fpData <- toCovariateData(inputFile = covariateSettings$outputFile, objectWithIds = input)
  
  t4duration <- Sys.time() - t4start 
  
  writeLines(paste("Creating covariate data object took", round(t4duration, 2), paste0(attr(t4duration, which = "units"), "."), sep = " "))
  
  assign("frequentPatternsData", frequentPatterns, envir = .GlobalEnv)
  return(fpData)
}

