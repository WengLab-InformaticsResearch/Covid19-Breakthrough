# # Last updated: 09-09-2021
# Author: Cong Liu
# checked version: No

rm(list=ls())
source("./utils.R")
library(data.table)
library(dplyr)


server = 'xxx.xxx.xxx.xxx' # please input your server here
database = 'ohdsi_cumc_2021q1r3' # please input ohdsi database here
uid = 'xxx' # please input the user name for ohdsi db here.
con = ohdsiConnection(server = server,database = database, uid = uid)
testTable = dbGetQuery(con, 
    "SELECT TOP 100 * FROM [dbo].[measurement]"
)
testTable = data.table(testTable)
# testTable

# Define vaccinated cohort 
# (there is no jassen vaccine)
sql = "
select d.person_id
	, 'moderna' as vaccine_brand
	, max(d.drug_exposure_start_date) as latest_dose_date
	, min(d.drug_exposure_start_date) as earliest_dose_date
	from [dbo].drug_exposure d 
	where d.drug_concept_id in (37003518)
	group by d.person_id
	union all 
	select d.person_id
	, 'pfizer' as vaccine_brand
	, max(d.drug_exposure_start_date) as latest_dose_date
	, min(d.drug_exposure_start_date) as earliest_dose_date
	from [dbo].drug_exposure d 
	where d.drug_concept_id in (37003436)
	group by d.person_id
	union all
	select d.person_id
	, 'jassen' as vaccine_brand
	, max(d.drug_exposure_start_date) as latest_dose_date
	, min(d.drug_exposure_start_date) as earliest_dose_date
	from [dbo].drug_exposure d 
	where d.drug_concept_id in (702866)
	group by d.person_id
"
vaccinatedCohort = dbGetQuery(con,sql)
vaccinatedCohort %>% pull(person_id) %>% unique() %>% length() # 336236

# dim(vaccinatedCohort) # 262205
# vaccinatedCohort %>% group_by(vaccine_brand) %>% summarise(N=length(unique(person_id)))

# Define cleaned vaccinated cohort cohort cleaning 
# only focus on mRNA vaccines - remove 8012 Jassen ones.
vaccinatedCohort %>% 
    filter(vaccine_brand %in% c("pfizer","moderna")) %>%
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
  filter(earliest_dose_date >= as.Date("2020-12-11"))

duplicatesDosePersonID = cleanedVaccinatedCohort %>% filter(person_id %>% duplicated()) %>% pull(person_id) # 57

cleanedVaccinatedCohort = cleanedVaccinatedCohort %>% filter(!person_id %in% duplicatesDosePersonID)
# dim(cleanedVaccinatedCohort) # 182560
# cleanedVaccinatedCohort %>% arrange(latest_dose_date) %>% head(1) # 2021-01-04
# cleanedVaccinatedCohort %>% arrange(earliest_dose_date) %>% head(1) # 2020-12-14
cleanedVaccinatedCohort %>% pull(person_id) %>% unique() %>% length() # 227617
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
# dim(covidPositivePcrCohort) # 20905 (37529 if ab test positive included)
covidPositivePcrCohort %>% pull(person_id) %>% unique() %>% length() # 17923


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
# dim(covidNegativePcrCohort)[1] # 370157 (37529 if ab test positive included)
covidNegativePcrCohort %>% pull(person_id) %>% unique() %>% length() # 211246
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
covidPositiveGeneralCohort %>% pull(person_id) %>% unique() %>% length() # 61694


# define covid-vaccine breakthrough cohort

breakthroughCovid = cleanedVaccinatedCohort %>% 
    left_join(covidPositivePcrCohort,by = "person_id") %>%
    mutate(days_to_last_dose = as.integer(
        difftime(evidence_date, latest_dose_date, units = "days"))) %>%
    filter(days_to_last_dose >= 14) %>% 
    group_by(person_id) %>%
    arrange(days_to_last_dose) %>% # ealiert prc.
    slice(1) %>%
    ungroup %>%
    mutate(index_date = evidence_date)
breakthroughCovid %>% pull(person_id) %>% unique() %>% length() # 357

# Remove before vax evidences
beforeVaxEvidence = cleanedVaccinatedCohort %>% 
  left_join(covidPositiveGeneralCohort,by = "person_id") %>%
  mutate(days_to_last_dose = as.integer(
    difftime(evidence_date, latest_dose_date, units = "days"))) %>%
  filter(days_to_last_dose < 14) %>% 
  group_by(person_id) %>%
  arrange(days_to_last_dose) %>% # ealiert prc.
  slice(1) %>%
  ungroup %>%
  mutate(index_date = evidence_date) %>% dplyr::select(person_id)

breakthroughCovid = breakthroughCovid %>% filter(!person_id %in% beforeVaxEvidence$person_id)
breakthroughCovid %>% pull(person_id) %>% unique() %>% length() # 263.


# define covid-vaccine non breakthrough cohort with more confidences.
nonBreakthroughPcrCovid = cleanedVaccinatedCohort %>% 
    left_join(covidNegativePcrCohort,by = "person_id") %>%
    mutate(days_to_last_dose = as.integer(
        difftime(evidence_date, latest_dose_date, units = "days"))) %>%
    filter(days_to_last_dose >= 14) %>% 
    group_by(person_id) %>%
    arrange(-days_to_last_dose) %>% # latest pcr date.
    slice(1) %>%
    ungroup %>% mutate(index_date = evidence_date) %>% dplyr::select(-evidence_concept_id,-evidence_date) 

nonBreakthroughPcrCovid %>% pull(person_id) %>% unique() %>% length() # 24760

# dim(nonBreakthroughPcrCovid) # 10201
beforeExitEvidence = nonBreakthroughPcrCovid %>%
  left_join(covidPositiveGeneralCohort,by = "person_id") %>%
  filter(!is.na(evidence_date))
# before left evidence.
nonBreakthroughPcrCovid = nonBreakthroughPcrCovid %>% filter(!person_id %in% beforeExitEvidence$person_id)
nonBreakthroughPcrCovid %>% pull(person_id) %>% unique() %>% length() # 18683

# define pre-vaccine PCR negative cohort
preVaccinePcrNegativeCovid = covidNegativePcrCohort %>% 
    dplyr::select(person_id,evidence_date) %>%
    filter(evidence_date < "2020-12-11") %>%
    mutate(days_to_first_eua = as.integer(
    difftime("2020-12-11",evidence_date, units = "days"))) %>%
    group_by(person_id) %>%
    arrange(days_to_first_eua) %>% # pcr date close to EUA date
    slice(1) %>%
    ungroup %>% mutate(index_date = evidence_date) %>% dplyr::select(-evidence_date)
preVaccinePcrNegativeCovid %>% pull(person_id) %>% unique() %>% length() # 117848

# before eua evidence
beforeEuaEvidence = preVaccinePcrNegativeCovid %>%
    left_join(covidPositiveGeneralCohort,by = "person_id") %>%
    filter(evidence_date < "2020-12-11" & !is.na(evidence_date))

preVaccinePcrNegativeCovid = preVaccinePcrNegativeCovid %>% filter(!person_id %in% beforeEuaEvidence$person_id)
preVaccinePcrNegativeCovid %>% pull(person_id) %>% unique() %>% length() # 95793

# dim(preVaccinePcrNegativeCovid) # 89725

# define pre-vaccine PCR positive cohort
preVaccinePcrPositiveCovid = covidPositivePcrCohort %>% 
  dplyr::select(person_id,evidence_date) %>%
  filter(evidence_date < "2020-12-11") %>% 
  mutate(days_to_first_eua = as.integer(
    difftime("2020-12-11",evidence_date, units = "days"))) %>%
  group_by(person_id) %>%
  arrange(days_to_first_eua) %>% # pcr date close to EUA date
  slice(1) %>%
  ungroup %>% 
  mutate(index_date = evidence_date)
preVaccinePcrPositiveCovid %>% pull(person_id) %>% unique() %>% length() # 8770

# first administrating date.
cleanedVaccinatedCohort %>% dplyr::pull(latest_dose_date) %>% unique() %>% min() # 2021-01-04

# define post-vaccinated pcr negative cohort
postVaccinePcrNegativeCovid = covidNegativePcrCohort %>% 
  dplyr::select(person_id,evidence_date) %>%
  mutate(entry_date = as.Date("2021-01-04") + 14) %>%
  mutate(index_date = evidence_date) %>% 
  dplyr::select(person_id,index_date,entry_date) %>%
  left_join(vaccinatedCohort) %>%
  mutate(censor_date = earliest_dose_date) %>%
  dplyr::select(person_id,index_date,entry_date,censor_date) %>%
  filter(((!is.na(censor_date) & (index_date < censor_date)) | is.na(censor_date) ) & (index_date > entry_date) ) %>%
  group_by(person_id) %>%
  arrange(desc(index_date)) %>% # latest pcr
  slice(1) %>%
  ungroup
postVaccinePcrNegativeCovid %>% pull(person_id) %>% unique() %>% length() # 74726

# before vax evidence.
beforeVaxEvidence = postVaccinePcrNegativeCovid %>% 
  left_join(covidPositiveGeneralCohort,by = "person_id") %>%
  filter(!is.na(evidence_date) & (is.na(censor_date) | (!is.na(censor_date) & (evidence_date < censor_date))))

postVaccinePcrNegativeCovid = postVaccinePcrNegativeCovid %>% filter(!person_id %in% beforeVaxEvidence$person_id)
postVaccinePcrNegativeCovid %>% pull(person_id) %>% unique() %>% length() # 63208

# dim(postVaccinePcrNegativeCovid) # 51,005

# define post-vaccinated pcr positive cohort
postVaccinePcrPositiveCovid = covidPositivePcrCohort %>% 
  dplyr::select(person_id,evidence_date) %>%
  mutate(entry_date = as.Date("2021-01-04") + 14) %>%
  mutate(index_date = evidence_date) %>% 
  dplyr::select(person_id,index_date,entry_date) %>%
  left_join(vaccinatedCohort) %>%
  mutate(censor_date = earliest_dose_date) %>%
  dplyr::select(person_id,index_date,entry_date,censor_date) %>%
  filter((!is.na(censor_date) & (index_date < censor_date) | is.na(censor_date) ) & (index_date > entry_date) ) %>%
  group_by(person_id) %>%
  arrange(index_date) %>% # ealiest pcr
  slice(1) %>%
  ungroup 
postVaccinePcrPositiveCovid %>% pull(person_id) %>% unique() %>% length() # 6035

# before eua evidence.
beforeEuaEvidence = postVaccinePcrPositiveCovid %>% 
  left_join(covidPositiveGeneralCohort,by = "person_id") %>%
  filter(!is.na(evidence_date) & ((evidence_date < entry_date)))

postVaccinePcrPositiveCovid = postVaccinePcrPositiveCovid %>% filter(!person_id %in% beforeEuaEvidence$person_id)
postVaccinePcrPositiveCovid %>% pull(person_id) %>% unique() %>% length() # 5457



    






