library(DatabaseConnector)
library(SqlRender)
library(Eunomia)
library(FeatureExtraction)
library(AssociationRuleMining)
devtools::load_all()

#### Connect to the database #### 

connectionDetails <- createConnectionDetails(
  dbms = "",
  server = "",
  user ="",
  password = "",
  port = 0)

#### Define database parameters ####

cdmDatabaseSchema = ""
resultsDatabaseSchema = ""
cohortTable <- ""
#cohortId <- 1
rowIdField <- "subject_id"

#### Define location and names of input/output files ####

arm_inputFile <- ".txt" #This is the input file containing structured data for the algorithms. Should be a .txt file.
arm_outputFile <- ".txt" # This is where the results output will be saved. Should be a .txt file

fpm_inputFile <- ".txt" #This is the input file containing structured data for the algorithms. Should be a .txt file.
fpm_outputFile <- ".txt" # This is where the results output will be saved. Should be a .txt file

####  Define covariate settings and Construct covariate data ####

## For Assoiation rules 

covariateSettings <- FeatureExtraction::createCovariateSettings(useConditionOccurrenceAnyTimePrior = TRUE, 
                                                                  useDrugExposureAnyTimePrior = TRUE)

covariateData <- FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
                                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                                       cohortDatabaseSchema = cohortDatabaseSchema,
                                                       cohortTable = cohortTable,
                                                       cohortId = cohortId,
                                                       covariateSettings = covariateSettings, 
                                                       ...)

## For Frequent pattern mining covariates need to have a timeId

### NOTE: For the following covariate settings as they are defined below, the construction time might be quite long.
temporalCovariateSettings <- FeatureExtraction::createTemporalCovariateSettings(useConditionOccurrence = TRUE,
                                                                                useDrugExposure = TRUE, 
                                                                                temporalStartDays = seq(-(99*365), -1, by = 1) ,
                                                                                temporalEndDays = seq(-(99*365)+1, 0, by = 1))

temporalCovariateData <- FeatureExtraction::getDbCovariateData(connection = connection, 
                                                    cdmDatabaseSchema = cdmdatabaseschema, 
                                                    cohortDatabaseSchema = resultsdatabaseschema, 
                                                    cohortTable = cohortTable, 
                                                    rowIdField = "subject_id", 
                                                    covariateSettings = temporalCovariateSettings, 
                                                    ...)

#### Association rule mining ####

## Prepare the data
getInputFileForAssociationRules(covariateDataObject = covariateData, fileToSave = arm_inputFile)

## Run Apriori

apriori_associationSets <- runAssociationRules(algorithm = "Apriori", 
                                       inputFile = arm_inputFile, 
                                       outputFile = arm_outputFile,
                                       minsup = 0.5 )

## Run Eclat

eclat_associationSets <- runAssociationRules(algorithm = "Eclat", 
                                       inputFile = arm_inputFile, 
                                       outputFile = arm_outputFile,
                                       minsup = 0.5 )

## Run FP-Growth 

fpgrowth_associationSets <- runAssociationRules(algorithm = "FP-Growth", 
                                       inputFile = arm_inputFile, 
                                       outputFile = arm_outputFile,
                                       minsup = 0.5 )


## Run Relim

relim_associationSets <- runAssociationRules(algorithm = "Relim", 
                                       inputFile = arm_inputFile, 
                                       outputFile = arm_outputFile,
                                       minsup = 0.5 )

#### Frequent pattern mining ####

## Prepare the data
getInputFileForFrequentPatterns(covariateDataObject = TemporalcovariateData, fileToSave = fpm_inputFile)

## Run SPAM

spam_frequentPatterns <- runFrequentPatterns(algorithm = "SPAM", 
                                        inputFile = fpm_inputFile, 
                                        outputFile = fpm_outputFile, 
                                        minsup = 0.5, 
                                        showID = TRUE)

## Run SPADE

spade_frequentPatterns <- runFrequentPatterns(algorithm = "SPADE", 
                                        inputFile = fpm_inputFile, 
                                        outputFile = fpm_outputFile, 
                                        minsup = 0.5, 
                                        showID = TRUE)

## Run prefixSpan
pS_frequentPatterns <- runFrequentPatterns(algorithm = "prefixSpan", 
                                        inputFile = fpm_inputFile, 
                                        outputFile = fpm_outputFile, 
                                        minsup = 0.5, 
                                        showID = TRUE)

