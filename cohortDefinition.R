# # Last updated: 09-09-2021
# Author: Cong Liu
# checked version: No

rm(list=ls())
source("./utils.R")
library(data.table)
library(dplyr)


server = 'elilex.dbmi.columbia.edu' # please input your server here
database = 'ohdsi_cumc_2021q2' # please input ohdsi database here
uid = 'cl3720_local' # please input the user name for ohdsi db here.
con = ohdsiConnection(server = server,database = database, uid = uid)
testTable = dbGetQuery(con, 
    "SELECT TOP 100 * FROM [dbo].[measurement]"
)
testTable = data.table(testTable)
testTable

# Define vaccinated cohort 
# (there is no jassen vaccine)
sql = "
select d.person_id
	, 'moderna' as vaccine_brand
	, max(d.drug_exposure_start_date) as latest_dose_date
	, min(d.drug_exposure_start_date) as earliest_dose_date
	from [dbo].drug_exposure d 
	where d.drug_concept_id in (36371349,37003432,37003435,37003517,37003518)
	group by d.person_id
	union all 
	select d.person_id
	, 'pfizer' as vaccine_brand
	, max(d.drug_exposure_start_date) as latest_dose_date
	, min(d.drug_exposure_start_date) as earliest_dose_date
	from [dbo].drug_exposure d 
	where d.drug_concept_id in (37003432,37003433,37003435,37003436,42794278)
	group by d.person_id
	union all
	select d.person_id
	, 'jassen' as vaccine_brand
	, max(d.drug_exposure_start_date) as latest_dose_date
	, min(d.drug_exposure_start_date) as earliest_dose_date
	from [dbo].drug_exposure d 
	where d.drug_concept_id in (739902,739903,739905,739906,1202358)
	group by d.person_id
"
vaccinatedCohort = dbGetQuery(con,sql)
dim(vaccinatedCohort) # 262205

# Define cleaned vaccinated cohort cohort cleaning 
vaccinatedCohort %>% 
    mutate(dose_interval = as.integer(
        difftime(latest_dose_date, earliest_dose_date, units = "days"))) %>% 
    group_by(vaccine_brand) %>%
    summarise(quantile(dose_interval,probs = seq(0,1,0.1))) %>% print(n=30)
# moderna:  (27,31)
# pfizer: (20,23)
cleanedVaccinatedCohort = vaccinatedCohort %>% 
    mutate(dose_interval = as.integer(
        difftime(latest_dose_date, earliest_dose_date, units = "days"))) %>% 
    filter((vaccine_brand == 'moderna' & dose_interval > 27 & dose_interval < 31) 
           |(vaccine_brand == 'pfizer' & dose_interval > 20 & dose_interval < 23)) %>%
  filter(latest_dose_date > as.Date("2020-12-11"))
dim(cleanedVaccinatedCohort) # 182561
cleanedVaccinatedCohort %>% arrange(latest_dose_date) %>% head(1) # 2021-01-04
cleanedVaccinatedCohort %>% arrange(earliest_dose_date) %>% head(1) # 2020-12-09

# cleanedVaccinatedCohort %>% arrange(latest_dose_date)

# Define covid positive (based on prc) cohort
sql = "
select m.person_id
,m.measurement_date as evidence_date
,m.measurement_concept_id as evidence_concept_id
from [dbo].measurement m
where m.measurement_concept_id in 
(586307,586308,586310,586517,586518,586519,586520,586523,586524,586525,586526,586528,586529,700360,704975,704976,704991,704992,704993,705000,705001,705106,705107,706154,706155,706156,706157,706158,706159,706160,706161,706163,706165,706166,706167,706168,706169,706170,706171,706172,706173,706174,706175,715260,715261,715262,715272,723463,723464,723465,723466,723467,723468,723469,723470,723471,723472,723476,723478,742218,742219,742220,756029,756055,756065,756084,756085,757677,757678,3667067,3667069,36661375,36661376,36661377,36661378,36661384,40218804,40218805)
and value_as_concept_id in (9191,4127785,3661907,4126681,4127786,9192,4123508,4125547,
4126673,4126674,4181412,40479562,40479567,40479985,45877985,45879438,45884084)
"
covidPositivePcrCohort = dbGetQuery(con,sql)
dim(covidPositivePcrCohort) # 20905 (37529 if ab test positive included)

# Define covid negative (based on prc) cohort
sql = "
select m.person_id
,m.measurement_date as evidence_date
,m.measurement_concept_id as evidence_concept_id
from [dbo].measurement m
where m.measurement_concept_id in 
(586307,586308,586310,586517,586518,586519,586520,586523,586524,586525,586526,586528,586529,700360,704975,704976,704991,704992,704993,705000,705001,705106,705107,706154,706155,706156,706157,706158,706159,706160,706161,706163,706165,706166,706167,706168,706169,706170,706171,706172,706173,706174,706175,715260,715261,715262,715272,723463,723464,723465,723466,723467,723468,723469,723470,723471,723472,723476,723478,742218,742219,742220,756029,756055,756065,756084,756085,757677,757678,3667067,3667069,36661375,36661376,36661377,36661378,36661384,40218804,40218805)
and value_as_concept_id not in (9191,4127785,3661907,4126681,4127786,9192,4123508,4125547,
4126673,4126674,4181412,40479562,40479567,40479985,45877985,45879438,45884084)
"
covidNegativePcrCohort = dbGetQuery(con,sql)
dim(covidNegativePcrCohort) # 370157 (37529 if ab test positive included)

# Define covid positive (based on all meas and cond) cohort
sql = "
select m.person_id
,m.measurement_date as evidence_date
,m.measurement_concept_id as evidence_concept_id
from [dbo].measurement m
where m.measurement_concept_id in 
(586307,586308,586309,586310,586515,586516,586517,586518,586519,586520,586521,586522,586523,586524,586525,586526,586527,586528,586529,700360,702834,704975,704976,704991,704992,704993,705000,705001,705106,705107,706154,706155,706156,706157,706158,706159,706160,706161,706163,706165,706166,706167,706168,706169,706170,706171,706172,706173,706174,706175,706176,706177,706178,706179,706180,706181,715260,715261,715262,715272,723459,723463,723464,723465,723466,723467,723468,723469,723470,723471,723472,723473,723474,723475,723476,723477,723478,723479,723480,742218,742219,742220,742223,756029,756055,756065,756084,756085,757677,757678,757679,757680,757686,3667067,3667069,36659631,36661375,36661376,36661377,36661378,36661384,37310257,37310258,40218804,40218805)
and value_as_concept_id in (9191,4127785,3661907,4126681,4127786,9192,4123508,4125547,
4126673,4126674,4181412,40479562,40479567,40479985,45877985,45879438,45884084)
union all
select c.person_id
,c.condition_start_date as evidence_date
,c.condition_concept_id as evidence_concept_id
from [dbo].condition_occurrence c
where c.condition_concept_id in (756031,756039,3655975,3655976,3655977,3656667,3656668,3656669,3661405,3661406,3661408,3661631,3661632,3661748,3661885,3662381,3663281,37310254,37310283,37310284,37310286,37310287,37311061)
"
covidPositiveGeneralCohort = dbGetQuery(con,sql)
dim(covidPositiveGeneralCohort) # 223093


# define covid-vaccine breakthrough cohort

breakthroughCovid = cleanedVaccinatedCohort %>% 
    left_join(covidPositivePcrCohort,by = "person_id") %>%
    mutate(days_to_last_dose = as.integer(
        difftime(evidence_date, latest_dose_date, units = "days"))) %>%
    filter(days_to_last_dose > 14) %>% 
    group_by(person_id) %>%
    arrange(days_to_last_dose) %>%
    slice(1) %>%
    ungroup %>%
    mutate(index_date = evidence_date)
  
dim(breakthroughCovid) # 129 (924 if ab test included, 2203 if all covid used)

# define covid-vaccine non breakthrough cohort
suspBreakthroughCohort = cleanedVaccinatedCohort %>% 
    left_join(covidPositiveGeneralCohort,by = "person_id") %>%
    mutate(days_to_first_dose = as.integer(
        difftime(evidence_date, earliest_dose_date, units = "days"))) %>%
    filter(days_to_first_dose > 0) %>%  
    group_by(person_id) %>%
    arrange(days_to_first_dose) %>%
    slice(1) %>%
    ungroup %>% 
    dplyr::select(person_id,evidence_date)
dim(suspBreakthroughCohort) # 3312

nonBreakthroughCovid = cleanedVaccinatedCohort %>% 
    left_join(suspBreakthroughCohort,by = "person_id") %>%
    filter(is.na(evidence_date)) %>% 
    mutate(index_date = latest_dose_date)
dim(nonBreakthroughCovid) # 179275

# define covid-vaccine non breakthrough cohort with more confidences.
nonBreakthroughPcrCovid = cleanedVaccinatedCohort %>% 
    left_join(covidNegativePcrCohort,by = "person_id") %>%
    mutate(days_to_last_dose = as.integer(
        difftime(evidence_date, latest_dose_date, units = "days"))) %>%
    filter(days_to_last_dose > 14) %>% 
    group_by(person_id) %>%
    arrange(-days_to_last_dose) %>%
    slice(1) %>%
    ungroup %>% 
    left_join(suspBreakthroughCohort,by = "person_id") %>%
    filter(is.na(evidence_date.y)) %>% 
    mutate(index_date = evidence_date.x)
dim(nonBreakthroughPcrCovid) # 12503

# define pre-vaccine PCR negative cohort
preVaccinePcrNegativeCovid = covidNegativePcrCohort %>% 
    dplyr::select(person_id,evidence_date) %>%
    filter(evidence_date < "2020-12-11") %>%
    mutate(days_to_first_eua = as.integer(
    difftime("2020-12-11",evidence_date, units = "days"))) %>%
    group_by(person_id) %>%
    arrange(days_to_first_eua) %>%
    slice(1) %>%
    ungroup %>% 
    left_join(covidPositiveGeneralCohort,by = "person_id") %>%
    filter(is.na(evidence_date.y)) %>%
    mutate(index_date = evidence_date.x)

dim(preVaccinePcrNegativeCovid) # 89725

# define pre-vaccine PCR positive cohort
preVaccinePcrPositiveCovid = covidPositivePcrCohort %>% 
  dplyr::select(person_id,evidence_date) %>%
  filter(evidence_date < "2020-12-11") %>% 
  mutate(days_to_first_eua = as.integer(
    difftime("2020-12-11",evidence_date, units = "days"))) %>%
  group_by(person_id) %>%
  arrange(days_to_first_eua) %>%
  slice(1) %>%
  ungroup %>% 
  mutate(index_date = evidence_date)
dim(preVaccinePcrPositiveCovid) # 8356

# first administrating date.
cleanedVaccinatedCohort %>% dplyr::pull(latest_dose_date) %>% unique() %>% min()

# define post-vaccinated cohort
postVaccinePcrNegativeCovid = covidNegativePcrCohort %>% 
  dplyr::select(person_id,evidence_date) %>%
  mutate(entry_date = as.Date("2021-01-04") + 14) %>%
  left_join(covidPositiveGeneralCohort,by = "person_id") %>%
  filter(is.na(evidence_date.y)) %>%
  mutate(index_date = evidence_date.x) %>% 
  dplyr::select(person_id,index_date,entry_date) %>%
  left_join(cleanedVaccinatedCohort) %>%
  mutate(censor_date = earliest_dose_date) %>%
  dplyr::select(person_id,index_date,entry_date,censor_date) %>%
  filter((!is.na(censor_date) & (index_date < censor_date) | is.na(censor_date) ) & (index_date > entry_date) ) %>%
  group_by(person_id) %>%
  arrange(desc(index_date)) %>%
  slice(1) %>%
  ungroup

dim(postVaccinePcrNegativeCovid) # 51,005

postVaccinePcrPositiveCovid = covidPositivePcrCohort %>% 
  dplyr::select(person_id,evidence_date) %>%
  mutate(entry_date = as.Date("2021-01-04") + 14) %>%
  mutate(index_date = evidence_date) %>% 
  dplyr::select(person_id,index_date,entry_date) %>%
  left_join(cleanedVaccinatedCohort) %>%
  mutate(censor_date = earliest_dose_date) %>%
  dplyr::select(person_id,index_date,entry_date,censor_date) %>%
  filter((!is.na(censor_date) & (index_date < censor_date) | is.na(censor_date) ) & (index_date > entry_date) ) %>%
  group_by(person_id) %>%
  arrange(index_date) %>%
  slice(1) %>%
  ungroup
dim(postVaccinePcrPositiveCovid) # 5078

# define pre-vaccine PCR positive cohort


    






