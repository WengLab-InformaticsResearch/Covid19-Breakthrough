# Last updated: 09-08-2021
# Author: Cong Liu
# checked version: Yes

source("./featureExtraction.R")
library(MatchIt)
library(epitools)
library(chron)
library(survival)
library(ggplot2)
library(ggfortify)



# get matched samples based on nearest neighbors.
breakthroughCovidCov = breakthroughCovid %>% 
  left_join(breakthroughCovidFeatures$visit,by = "person_id",copy = TRUE) %>%
  left_join(breakthroughCovidFeatures$obDays,by = "person_id",copy = TRUE) %>%
  left_join(breakthroughCovidFeatures$demo,by = "person_id",copy = TRUE) %>%
  mutate(age_at_index = as.integer(difftime(units = "days",index_date,DOB)/365.24)) %>%
  mutate(race_category = case_when((race == "White") ~ "White",
                                   (race == "Black or African American") ~ "Black",
                                   (race == "Asian") ~ "Asian",
                                   TRUE ~ "Other Race or Unknown")) %>%
  left_join(breakthroughCovidFeatures$rollingAvg,by = c("person_id","index_date"),copy = TRUE) %>%
  left_join(breakthroughCovidFeatures$immuno,by = c("person_id"),copy = TRUE) %>%
  left_join(breakthroughCovidFeatures$last %>% dplyr::select(person_id,censored_date),by = c("person_id"),copy = TRUE) %>%
  mutate(isImmuo = case_when(is.na(category)~'Not Immuno Comprised',
                              TRUE~'Immuno Comprised')) %>%
  dplyr::select(person_id,index_date, censored_date, count_of_visits,count_of_visits,
                observation_days,gender,age_at_index,race_category,ethnicity,
                cases_avg,isImmuo) %>%
  mutate(is_vaccinated = T) %>% 
  distinct_all()

preVaccinePcrPositiveCovidCov = preVaccinePcrPositiveCovid %>% 
  left_join(preVaccinePcrPositiveCovidFeatures$visit,by = "person_id",copy = TRUE) %>%
  left_join(preVaccinePcrPositiveCovidFeatures$obDays,by = "person_id",copy = TRUE) %>%
  left_join(preVaccinePcrPositiveCovidFeatures$demo,by = "person_id",copy = TRUE) %>%
  mutate(age_at_index = as.integer(difftime(units = "days",index_date,DOB)/365.24)) %>%
  mutate(race_category = case_when((race == "White") ~ "White",
                                   (race == "Black or African American") ~ "Black",
                                   (race == "Asian") ~ "Asian",
                                   TRUE ~ "Other Race or Unknown")) %>%
  left_join(preVaccinePcrPositiveCovidFeatures$rollingAvg,by = c("person_id","index_date"),copy = TRUE) %>%
  left_join(preVaccinePcrPositiveCovidFeatures$immuno,by = c("person_id"),copy = TRUE) %>%
  left_join(preVaccinePcrPositiveCovidFeatures$last %>% dplyr::select(person_id,censored_date),by = c("person_id"),copy = TRUE) %>%
  mutate(isImmuo = case_when(is.na(category)~'Not Immuno Comprised',
                             TRUE~'Immuno Comprised')) %>%
  dplyr::select(person_id,index_date,censored_date,count_of_visits,
                observation_days,gender,age_at_index,race_category,ethnicity,
                cases_avg,isImmuo) %>% 
  mutate(is_vaccinated = F) %>% 
  distinct_all()

unvaccinatedPositiveCovidCov = postVaccinePcrPositiveCovid %>% 
  left_join(postVaccinePcrPositiveCovidFeatures$visit,by = "person_id",copy = TRUE) %>%
  left_join(postVaccinePcrPositiveCovidFeatures$obDays,by = "person_id",copy = TRUE) %>%
  left_join(postVaccinePcrPositiveCovidFeatures$demo,by = "person_id",copy = TRUE) %>%
  mutate(age_at_index = as.integer(difftime(units = "days",index_date,DOB)/365.24)) %>%
  mutate(race_category = case_when((race == "White") ~ "White",
                                   (race == "Black or African American") ~ "Black",
                                   (race == "Asian") ~ "Asian",
                                   TRUE ~ "Other Race or Unknown")) %>%
  left_join(postVaccinePcrPositiveCovidFeatures$rollingAvg,by = c("person_id","index_date"),copy = TRUE) %>%
  left_join(postVaccinePcrPositiveCovidFeatures$immuno,by = c("person_id"),copy = TRUE) %>%
  left_join(postVaccinePcrPositiveCovidFeatures$last %>% dplyr::select(person_id,censored_date),by = c("person_id"),copy = TRUE) %>%
  mutate(isImmuo = case_when(is.na(category)~'Not Immuno Comprised',
                             TRUE~'Immuno Comprised')) %>%
  dplyr::select(person_id,index_date,censored_date,count_of_visits,
                observation_days,gender,age_at_index,race_category,ethnicity,
                cases_avg,isImmuo) %>% 
  mutate(is_vaccinated = F) %>% 
  distinct_all()

forMatchData = rbind(breakthroughCovidCov,preVaccinePcrPositiveCovidCov)
# fill missing value.
forMatchData = forMatchData %>% 
  replace_na(list(count_of_visits = 0, observation_days = 0,cases_avg=0))
set.seed(5)
# take a minute
matchIt = matchit(is_vaccinated ~ gender + age_at_index + race_category + 
                    ethnicity + isImmuo + count_of_visits + observation_days, data = forMatchData, method="nearest", ratio=10)
plot(summary(matchIt))
matchItData = match.data(matchIt)[1:ncol(forMatchData)] 
# generate outcome
outcome = rbind(breakthroughCovidFeatures$outcome,preVaccinePcrPositiveCovidFeatures$outcome)
allSevereOutcomes = outcome %>% 
  filter(category %in% c("ventilation","tracheostomy","ICU","Death","Inpatient")) %>%
  group_by(person_id) %>% summarise(event_date = min(event_date)) %>%
  distinct_all()
severOutcomeWith90days = matchItData %>% left_join(allSevereOutcomes,by = "person_id") %>%
  mutate(status = case_when(is.na(event_date)~0,TRUE~1)) %>% 
  mutate(time = case_when(is.na(event_date)~as.integer(difftime(units = "days",censored_date,index_date)),
                          TRUE~as.integer(difftime(units = "days",event_date,index_date)))) %>% 
  mutate(status = case_when((time <= 28 & status == 1L)~1L,TRUE~0L)) %>%
  mutate(time = if_else(time<28,time,28L)) %>%
  mutate(time = case_when(time <=0L~0L,TRUE~time))
severOutcomeWith90days %>% dplyr::select(person_id,is_vaccinated,index_date,censored_date,event_date,time,status)
univarTest(forTest = severOutcomeWith90days,var = "is_vaccinated",adj = NULL,lr = F,cox=T,poisson = F)
univarTest(forTest = severOutcomeWith90days,var = "is_vaccinated",adj = c("age_at_index","count_of_visits","observation_days","isImmuo"),lr = F,cox=T,poisson = F)
table(severOutcomeWith90days$status,severOutcomeWith90days$is_vaccinated)
# for each one.
for(i in c("ventilation","tracheostomy","ICU","Death","Inpatient")){
  print(i)
  allSevereOutcomes = outcome %>% 
    filter(category %in% i) %>%
    group_by(person_id) %>% summarise(event_date = min(event_date)) %>%
    distinct_all()
  severOutcomeWith90days = matchItData %>% left_join(allSevereOutcomes,by = "person_id") %>%
    mutate(status = case_when(is.na(event_date)~0,TRUE~1)) %>% 
    mutate(time = case_when(is.na(event_date)~as.integer(difftime(units = "days",censored_date,index_date)),
                            TRUE~as.integer(difftime(units = "days",event_date,index_date)))) %>% 
    mutate(status = case_when((time <= 28 & status == 1L)~1L,TRUE~0L)) %>%
    mutate(time = case_when(time <=0L~0L,TRUE~time))
  kmFit <- survfit(Surv(time, status) ~ is_vaccinated, data=severOutcomeWith90days)
  autoplot(kmFit)
  print(univarTest(forTest = severOutcomeWith90days,var = "is_vaccinated",adj = NULL,lr = F,cox=T,poisson = F))
  print(
    univarTest(forTest = severOutcomeWith90days,var = "is_vaccinated",adj = c("age_at_index","count_of_visits","observation_days","isImmuo"),lr = F,cox=T,poisson = F)
  )
  print(table(severOutcomeWith90days$status,severOutcomeWith90days$is_vaccinated)
)
  
}


coxFit = coxph(Surv(time, status) ~ is_vaccinated,
             data = severOutcomeWith90days)
summary(coxFit)
kmFit <- survfit(Surv(time, status) ~ is_vaccinated, data=severOutcomeWith90days)
autoplot(kmFit)
# oddsRatioTest(table(severOutcomeWith90days$status,severOutcomeWith90days$postVaccinated))
# adjusted by covariates.


##### vax vs. unvax
forMatchData = rbind(breakthroughCovidCov,unvaccinatedPositiveCovidCov)
# fill missing value.
forMatchData = forMatchData %>% 
  replace_na(list(count_of_visits = 0, observation_days = 0,cases_avg=0))
set.seed(5)
# take a minute
matchIt = matchit(is_vaccinated ~ count_of_visits+
                    observation_days+gender+age_at_index+race_category+ethnicity+
                    isImmuo, data = forMatchData, method="nearest", ratio=10)
plot(summary(matchIt))
matchItData = match.data(matchIt)[1:ncol(forMatchData)] 
# generate outcome
outcome = rbind(breakthroughCovidFeatures$outcome,postVaccinePcrPositiveCovidFeatures$outcome)
allSevereOutcomes = outcome %>% 
  filter(category %in% c("ventilation","tracheostomy","ICU","Death","Inpatient")) %>%
  group_by(person_id) %>% summarise(event_date = min(event_date)) %>%
  distinct_all()
severOutcomeWith90days = matchItData %>% left_join(allSevereOutcomes,by = "person_id") %>%
  mutate(status = case_when(is.na(event_date)~0,TRUE~1)) %>% 
  mutate(time = case_when(is.na(event_date)~as.integer(difftime(units = "days",censored_date,index_date)),
                          TRUE~as.integer(difftime(units = "days",event_date,index_date)))) %>% 
  mutate(status = case_when((time <= 28 & status == 1L)~1L,TRUE~0L)) %>%
  mutate(time = if_else(time<28,time,28L)) %>%
  mutate(time = case_when(time <=0L~0L,TRUE~time))
severOutcomeWith90days %>% dplyr::select(person_id,is_vaccinated,index_date,censored_date,event_date,time,status)
univarTest(forTest = severOutcomeWith90days,var = "is_vaccinated",adj = NULL,lr = F,cox=T,poisson = F)
univarTest(forTest = severOutcomeWith90days,var = "is_vaccinated",adj = c("age_at_index","count_of_visits","observation_days","isImmuo"),lr = F,cox=T,poisson = F)
table(severOutcomeWith90days$status,severOutcomeWith90days$is_vaccinated)

# for each one.
for(i in c("ventilation","tracheostomy","ICU","Death","Inpatient")){
  print(i)
  allSevereOutcomes = outcome %>% 
    filter(category %in% i) %>%
    group_by(person_id) %>% summarise(event_date = min(event_date)) %>%
    distinct_all()
  severOutcomeWith90days = matchItData %>% left_join(allSevereOutcomes,by = "person_id") %>%
    mutate(status = case_when(is.na(event_date)~0,TRUE~1)) %>% 
    mutate(time = case_when(is.na(event_date)~as.integer(difftime(units = "days",censored_date,index_date)),
                            TRUE~as.integer(difftime(units = "days",event_date,index_date)))) %>% 
    mutate(status = case_when((time <= 28 & status == 1L)~1L,TRUE~0L)) %>%
    mutate(time = case_when(time <=0L~0L,TRUE~time))
  kmFit <- survfit(Surv(time, status) ~ is_vaccinated, data=severOutcomeWith90days)
  autoplot(kmFit)
  print(univarTest(forTest = severOutcomeWith90days,var = "is_vaccinated",adj = NULL,lr = F,cox=T,poisson = F))
  print(
    univarTest(forTest = severOutcomeWith90days,var = "is_vaccinated",adj = c("count_of_visits",
                                                                                "observation_days","age_at_index",
                                                                                "isImmuo"),lr = F,cox=T,poisson = F)
  )
  print(oddsratio(table(severOutcomeWith90days$status,severOutcomeWith90days$is_vaccinated)
)
)
  
}



