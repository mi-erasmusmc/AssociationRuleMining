/***********************************
File cohortsOfInterest.sql
***********************************/
IF OBJECT_ID('@resultsDatabaseSchema.cohorts_of_interest', 'U') IS NOT NULL
DROP TABLE @resultsDatabaseSchema.cohorts_of_interest;
SELECT *
INTO @resultsDatabaseSchema.cohorts_of_interest
FROM (
SELECT condition_concept_id AS cohort_definition_id,
MIN(condition_start_date) AS cohort_start_date,
MIN(condition_end_date) AS cohort_end_date,
person_id
FROM @cdmDatabaseSchema.condition_occurrence 
WHERE condition_concept_id = 4329847-- Myocardial infarction
); /*use
INNER JOIN @cdmDatabaseSchema.observation_period
ON use.person_id = observation_period.person_id
AND cohort_start_date >= observation_period_start_date
AND cohort_end_date <= observation_period_end_date
WHERE DATEDIFF(DAY, observation_period_start_date, cohort_start_date) >= 365;*/