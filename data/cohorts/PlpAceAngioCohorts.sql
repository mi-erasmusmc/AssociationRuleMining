/***********************************
File AceAngioCohorts.sql
***********************************/
/*
Create a table to store the persons in the T and C cohort
*/
IF OBJECT_ID('@resultsDatabaseSchema.PLPAceAngioCohort', 'U') IS NOT NULL
DROP TABLE @resultsDatabaseSchema.PLPAceAngioCohort;
CREATE TABLE @resultsDatabaseSchema.PLPAceAngioCohort
(
cohort_definition_id INT,
subject_id BIGINT,
cohort_start_date DATE,
cohort_end_date DATE
);
/*
T cohort: [PatientLevelPrediction vignette]: T : patients who are newly
dispensed an ACE inhibitor
- persons with a drug exposure record of any 'ACE inhibitor' or
any descendants, indexed at the first diagnosis
- who have >364 days of prior observation before their first dispensing
*/
INSERT INTO @resultsDatabaseSchema.AceAngioCohort (cohort_definition_id,
subject_id,
cohort_start_date,
cohort_end_date)
SELECT 1 AS cohort_definition_id,
Ace.person_id AS subject_id,
Ace.drug_start_date AS cohort_start_date,
observation_period.observation_period_end_date AS cohort_end_date
FROM
(
SELECT person_id, min(drug_exposure_date) as drug_start_date
FROM @cdmDatabaseSchema.drug_exposure
WHERE drug_concept_id IN (SELECT descendant_concept_id FROM
@cdmDatabaseSchema.concept_ancestor WHERE ancestor_concept_id IN
(1342439,1334456, 1331235, 1373225, 1310756, 1308216, 1363749, 1341927, 1340128, 1335471 /*ace inhibitors*/ ))
GROUP BY person_id
) Ace
INNER JOIN @cdmDatabaseSchema.observation_period
ON Ace.person_id = observation_period.person_id
AND Ace.drug_start_date >= dateadd(dd,364,
observation_period.observation_period_start_date)
AND Ace.drug_start_date <= observation_period.observation_period_end_date
;
/*
C cohort: [PatientLevelPrediction vignette]: O: Angioedema
*/
INSERT INTO @resultsDatabaseSchema.AceAngioCohort (cohort_definition_id,
subject_id,
cohort_start_date,
cohort_end_date)
SELECT 2 AS cohort_definition_id,
angioedema.person_id AS subject_id,
angioedema.condition_start_date AS cohort_start_date,
angioedema.condition_start_date AS cohort_end_date
FROM
(
SELECT person_id, condition_start_date
FROM @cdmDatabaseSchema.condition_occurrence
WHERE condition_concept_id IN (SELECT DISTINCT descendant_concept_id FROM
@cdmDatabaseSchema.concept_ancestor WHERE ancestor_concept_id IN
(432791 /*angioedema*/ ) OR descendant_concept_id IN
(432791 /*angioedema*/ )
) angioedema
;
