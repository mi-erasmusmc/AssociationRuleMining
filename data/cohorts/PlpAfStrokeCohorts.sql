/***********************************
File AfStrokeCohorts.sql
***********************************/
/*
Create a table to store the persons in the T and C cohort
*/
IF OBJECT_ID('@resultsDatabaseSchema.PLPAFibStrokeCohort', 'U') IS NOT NULL
DROP TABLE @resultsDatabaseSchema.PLPAFibStrokeCohort;
CREATE TABLE @resultsDatabaseSchema.PLPAFibStrokeCohort
(
cohort_definition_id INT,
subject_id BIGINT,
cohort_start_date DATE,
cohort_end_date DATE
);

/*
T cohort: [PatientLevelPrediction vignette]: T : patients who are newly
diagnosed with Atrial fibrillation
- persons with a condition occurrence record of 'Atrial fibrillation' or
any descendants, indexed at the first diagnosis
- who have >1095 days of prior observation before their first diagnosis
- and have no warfarin exposure any time prior to first AFib diagnosis
*/
INSERT INTO @resultsDatabaseSchema.AFibStrokeCohort (cohort_definition_id,
subject_id,
cohort_start_date,
cohort_end_date)
SELECT 1 AS cohort_definition_id,
AFib.person_id AS subject_id,
AFib.condition_start_date AS cohort_start_date,
observation_period.observation_period_end_date AS cohort_end_date
FROM
(
SELECT person_id, min(condition_start_date) as condition_start_date
FROM @cdmDatabaseSchema.condition_occurrence
WHERE condition_concept_id IN (SELECT descendant_concept_id FROM
@cdmDatabaseSchema.concept_ancestor WHERE ancestor_concept_id IN
(313217 /*atrial fibrillation*/ ))
GROUP BY person_id
) AFib
INNER JOIN @cdmDatabaseSchema.observation_period
ON AFib.person_id = observation_period.person_id
AND AFib.condition_start_date >= dateadd(dd,1095,
observation_period.observation_period_start_date)
AND AFib.condition_start_date <= observation_period.observation_period_end_date
LEFT JOIN
(
SELECT person_id, min(drug_exposure_start_date) as drug_exposure_start_date
FROM @cdmDatabaseSchema.drug_exposure
WHERE drug_concept_id IN (SELECT descendant_concept_id FROM
@cdmDatabaseSchema.concept_ancestor WHERE ancestor_concept_id IN
(1310149 /*warfarin*/ ))
GROUP BY person_id
) warfarin
ON Afib.person_id = warfarin.person_id
AND Afib.condition_start_date > warfarin.drug_exposure_start_date
WHERE warfarin.person_id IS NULL
;
/*
C cohort: [PatientLevelPrediction vignette]: O: Ischemic stroke events
- inpatient visits that include a condition occurrence record for
'cerebral infarction' and descendants, 'cerebral thrombosis',
'cerebral embolism', 'cerebral artery occlusion'
*/
INSERT INTO @resultsDatabaseSchema.AFibStrokeCohort (cohort_definition_id,
subject_id,
cohort_start_date,
cohort_end_date)
SELECT 2 AS cohort_definition_id,
visit_occurrence.person_id AS subject_id,
visit_occurrence.visit_start_date AS cohort_start_date,
visit_occurrence.visit_end_date AS cohort_end_date
FROM
(
SELECT person_id, condition_start_date
FROM @cdmDatabaseSchema.condition_occurrence
WHERE condition_concept_id IN (SELECT DISTINCT descendant_concept_id FROM
@cdmDatabaseSchema.concept_ancestor WHERE ancestor_concept_id IN
(443454 /*cerebral infarction*/ ) OR descendant_concept_id IN
(441874 /*cerebral thrombosis*/ , 375557 /*cerebral embolism*/ ,
372924 /*cerebral artery occlusion*/ ))
) stroke
INNER JOIN @cdmDatabaseSchema.visit_occurrence
ON stroke.person_id = visit_occurrence.person_id
AND stroke.condition_start_date >= visit_occurrence.visit_start_date
AND stroke.condition_start_date <= visit_occurrence.visit_end_date
AND visit_occurrence.visit_concept_id IN (9201, 262 /*'Inpatient Visit' or
'Emergency Room and Inpatient Visit'*/ )
GROUP BY visit_occurrence.person_id, visit_occurrence.visit_start_date,
visit_occurrence.visit_end_date
;
