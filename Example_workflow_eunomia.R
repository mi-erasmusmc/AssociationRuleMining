library(DatabaseConnector)
library(SqlRender)
library(FeatureExtraction)
library(data.table)
library(stringr)

#### Connect to the database ####

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
connection <- connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(connection)) #Close db connection on error or exit

#Define database parameters
cdmdatabaseschema = "main"
resultsdatabaseschema = "main"

#### Define cohort ####

# There are two options for defining a cohort:
# 1) Define the cohort in ATLAS and export the SQL file
# 2) define it locally within R

## For this database we define the cohiort manually in the following script

sql <- "SELECT person_id AS subject_id,
  condition_start_date AS cohort_start_date
INTO #diagnoses
FROM @cdm.condition_occurrence
WHERE condition_concept_id IN (
    SELECT descendant_concept_id
    FROM @cdm.concept_ancestor
    WHERE ancestor_concept_id = 4329847 -- Myocardial infarction
)
  AND condition_concept_id NOT IN (
    SELECT descendant_concept_id
    FROM @cdm.concept_ancestor
    WHERE ancestor_concept_id = 314666 -- Old myocardial infarction
);
INSERT INTO @cdm.cohort (
  subject_id, 
  cohort_start_date, 
  cohort_definition_id
  )
SELECT subject_id,
  cohort_start_date,
  CAST (1 AS INT) AS cohort_definition_id
FROM #diagnoses
INNER JOIN @cdm.visit_occurrence
  ON subject_id = person_id
    AND cohort_start_date >= visit_start_date
    AND cohort_start_date <= visit_end_date
WHERE visit_concept_id IN (9201, 9203, 262); -- Inpatient or ER;"

# Execute the script to receive the data
renderTranslateExecuteSql(connection, sql, cdm = "main")

#### Feature Extraction ####

# Define covariate settings
covariateSettings_eunomia <- createTemporalCovariateSettings(useConditionOccurrence = TRUE, 
                                                      temporalStartDays = seq(-(5*365), -1, by = 1) ,
                                                      temporalEndDays = seq(-(5*365)+1, 0, by = 1))

# Extract covariates
covariateData_eunomia <- getDbCovariateData(connection = connection, 
                         cdmDatabaseSchema = cdmdatabaseschema, 
                         cohortDatabaseSchema = resultsdatabaseschema, 
                         cohortTable = "diagnoses", 
                         rowIdField = "subject_id", 
                         covariateSettings = covariateSettings_eunomia, 
                         cohortTableIsTemp = TRUE)


#summary(covariateData)
#class(covariateData$covariates)
#summary(covariateData$covariates)

#Converting to a dataframe
df_eunomia <- as.data.frame(covariateData_eunomia$covariates)
dt_eunomia <- as.data.table(df_eunomia)

#### Data preparation ####

# Preparing dataset with temporal information

trans_sequence <- df_eunomia
## Making sure everything is in the right order
#trans_sequence <- trans_sequence[order(trans_sequence$rowId, trans_sequence$timeId)]
trans_sequence <- trans_sequence %>% 
  group_by(rowId, timeId) %>% 
  arrange(rowId, timeId) %>% 
  ungroup

## Creating the eventID variable- although there exists a timeId variable given by the getTemporalCovariates function ,
## I am creating a new one as I do not know the effect of the numbering of this specific variable yet
trans_sequence$eventID <-  sequence(rle(as.character(trans_sequence$rowId))$lengths)

names(trans_sequence)

## Creating the Size variable
trans_sequence <- trans_sequence %>%
  group_by(rowId, timeId, covariateId, covariateValue, eventID) %>%
  summarize(SIZE = n()) %>% ungroup()

## extracting covariate names 
names.df <- as.data.frame(covariateData_eunomia$covariateRef)
str(names.df)
names.df$covariateId <- as.character(names.df$covariateId)
names.df$covariateLabel <-  str_replace(names.df$covariateName, ".*: ", "")
trans_sequence$covariateId <- as.character(trans_sequence$covariateId)
df_input <- inner_join(trans_sequence, names.df, by = "covariateId")
str(df_input)
# Filtering useful variables
df_input2 <- select(df_input, c(rowId, covariateLabel, eventID, SIZE))

#names(trans_sequence) <- c("sequenceID", "eventID", "SIZE", "items")
df_input2 <- data.frame(lapply(df_input2, as.factor))

#### Data characterization ####

# At this step the analyst would probably like to have a summary/characterization of the dataset.
# it would be nice to consider to provide a the functionality of observing a numerical or a visual summary
# of the contents of the dataset. 
# In addition, a verification of the extracted size of the dataset would be nice. 
# Below just adding few details of things that may be useful. 

# How many patients in the extracted cohort
length(unique(df_eunomia$rowId))

df <- df %>% group_by(rowId) %>% arrange(rowId, timeId)
#df_input <- select(df, c(ID, CONCEPT_NAME))

#How many conditions per person in the dataset?
df %>% 
  group_by(rowId) %>%
  summarise(no_rows = length(rowId))

#Average conditions per person in the dataset?
a <- df %>% 
  group_by(rowId) %>%
  summarise(no_rows = length(rowId))


# Confirming distinct values in the ID variable
length(unique(df$rowId))

df %>% 
  group_by(rowId) %>%
  summarise(no_rows = length(rowId)) %>% 
  ggplot(aes(no_rows)) + geom_density()

df %>% 
  group_by(rowId) %>%
  summarise(no_rows = length(rowId)) %>% 
  ggplot(aes(no_rows)) + geom_histogram()




#elapsed_months <- function(end_date, start_date) {
#  ed <- as.POSIXlt(end_date)
#  sd <- as.POSIXlt(start_date)
#  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
#}

