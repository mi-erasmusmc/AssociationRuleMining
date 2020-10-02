library(DatabaseConnector)
library(SqlRender)
library(FeatureExtraction)
library(data.table)

source("connections/connectionDetails_hds.R")
connection <- DatabaseConnector::connect(connectionDetails)


#Need to define the following parameters
cdmDatabaseSchema = "hds1.synpuf"
resultsDatabaseSchema = "hds1.sioannou"
vocabularyDatabaseSchema = cdmDatabaseSchema
targetCohortTable = "cohort"  # Or Just define it within the SqlRender::render()
targetCohortID = 1  # Or Just define it within the SqlRender::render()

  
#Executing the cohort generation script
sql <- readSql("data/cohorts/MI_2.sql")
sql <- render(sql, 
              cdm_database_schema = cdmDatabaseSchema, 
              results_database_schema = resultsDatabaseSchema, 
              vocabulary_database_schema = cdmDatabaseSchema, 
              target_database_schema = resultsDatabaseSchema, 
              target_cohort_table = targetCohortTable, 
              target_cohort_id = 1)
sql <- translate(sql, targetDialect = connection@dbms)
executeSql(connection, sql)

# Just to obtain the number of patients in the cohort table
querySql(connection, "SELECT count(*) FROM hds1.sioannou.cohort;")



# Define Covariate settings
covariateSettings1 <- createTemporalCovariateSettings(useConditionOccurrence = TRUE, 
                                                      temporalStartDays = seq(-(5*365), -1, by = 1) ,
                                                      temporalEndDays = seq(-(5*365)+1, 0, by = 1))

# Get coavriates
covariateData <- getDbCovariateData(connection = connection, 
                                    cdmDatabaseSchema = cdmDatabaseSchema, 
                                    cohortDatabaseSchema = resultsDatabaseSchema, 
                                    cohortTable = targetCohortTable, 
                                    rowIdField = "subject_id", 
                                    covariateSettings = covariateSettings1, 
                                    cohortId = 1,
                                    cohortTableIsTemp = F)

#summary(covariateData)
#covariateData$covariates
#class(covariateData$covariates)
summary(covariateData$covariates)


df <- as.data.frame(covariateData$covariates)
df.dt <- as.data.table(covariateData$covariates)
unique(df$rowId)


