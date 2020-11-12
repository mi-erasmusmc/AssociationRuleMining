## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(AssociationRuleMining)

## ---- eval=FALSE--------------------------------------------------------------
#  #### Feature Extraction ####
#  covariateSettings <- createCovariateSettings(useConditionOccurrenceAnyTimePrior = TRUE)
#  
#  covariateData <- getDbCovariateData(connection = connection,
#                                      cdmDatabaseSchema = cdmdatabaseschema, #The database schema where the cdm lives
#                                      cohortDatabaseSchema = resultsDatabaseSchema, #The database schema where the cohort table lives
#                                      cohortTable = "diagnoses", #Name of the cohort table
#                                      rowIdField = "subject_id",
#                                      covariateSettings = covariateSettings,
#                                      cohortTableIsTemp = TRUE) #If the cohort table is temporary or not
#  

## ---- eval = FALSE------------------------------------------------------------
#  TemporalcovariateSettings <- createTemporalCovariateSettings(useConditionOccurrence = TRUE,
#                                                        temporalStartDays = seq(-(60*365), -1, by = 1) ,
#                                                        temporalEndDays = seq(-(60*365)+1, 0, by = 1))
#  
#  # Extract covariates
#  TemporalcovariateData<- getDbCovariateData(connection = connection,
#                                             cdmDatabaseSchema = cdmdatabaseschema, #The database schema where the cdm lives
#                                             cohortDatabaseSchema = resultsdatabaseschema, #The database schema where the cohort table lives
#                                             cohortTable = cohorttable, #Name of the cohort table
#                                             rowIdField = "subject_id",
#                                             covariateSettings = TemporalcovariateSettings,
#                                             cohortTableIsTemp = TRUE) #If the cohort table is temporary or not

## ---- eval=FALSE--------------------------------------------------------------
#  getInputFileForAssociationRules(covariateDataObject = covariateData, fileToSave = "AssociationRulesExample.txt")

## ---- eval=FALSE--------------------------------------------------------------
#  getInputFileForFrequentPatterns(covariateDataObject = TemporalcovariateData, fileToSave = "FrequentPatternsExample.txt")

## ---- eval=FALSE--------------------------------------------------------------
#  associationSets <- runAssociationRules(algorithm = "Apriori",
#                                         inputFile = "AssociationRulesExample.txt",
#                                         outputFile = "AssociationRulesExample_Results.txt",
#                                         minsup = 0.5 )

## ---- eval=FALSE--------------------------------------------------------------
#  frequentPatterns <- runFrequentPatterns(algorithm = "SPADE",
#                                          inputFile = "FrequentPatternsExample.txt",
#                                          outputFile = "FrequentPatternsExample_Results.txt",
#                                          minsup = 0.5,
#                                          showID = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  getIdDataFrame(inputFile = "FrequentPatternsExample_Results.txt")

