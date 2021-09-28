# Last updated: 09-20-2021
# Author: Cong Liu
# checked version: Yes

# source("./cohortCharacterizationAndRefine.R")



breakthroughCovidPerson = breakthroughCovidRefined %>% 
  mutate(is_vaccinated = T) %>%
  mutate(time = as.integer(
    difftime(index_date, latest_dose_date, units = "days")) - 14 + 1) %>% 
  mutate(status = 1) %>%
  dplyr::select(person_id,latest_dose_date,index_date,is_vaccinated,time,status)

nonBreakthroughPcrCovidPerson = nonBreakthroughPcrCovidRefined %>% 
  mutate(is_vaccinated = T) %>%
  mutate(time = as.integer(
    difftime(index_date, latest_dose_date,units = "days")) -14 + 1) %>% 
  mutate(status = 0) %>%
  dplyr::select(person_id,latest_dose_date,index_date,is_vaccinated,time,status)

preVaccinePcrPositiveCovidPerson = preVaccinePcrPositiveCovidRefined %>%
  mutate(is_vaccinated = F) %>%
  mutate(status = 1) %>%
  dplyr::select(person_id,index_date,is_vaccinated,status)

preVaccinePcrNegativeCovidPerson = preVaccinePcrNegativeCovidRefined %>%
  mutate(is_vaccinated = F) %>%
  mutate(status = 0) %>%
  dplyr::select(person_id,index_date,is_vaccinated,status)

UnVaccinePcrPositiveCovidPerson = postVaccinePcrPositiveCovidRefined %>%
  mutate(is_vaccinated = F) %>%
  mutate(status = 1) %>%
  mutate(time = as.integer(
    difftime(index_date, entry_date,units = "days")) + 1)

UnVaccinePcrNegativeCovidPerson = postVaccinePcrNegativeCovidRefined %>%
  mutate(is_vaccinated = F) %>%
  mutate(status = 0) %>%
  mutate(end_date = index_date) %>% 
  # mutate(end_date = case_when(is.na(censor_date)~index_date,TRUE~censor_date)) %>%
  mutate(time = as.integer(
    difftime(end_date, entry_date,units = "days")) + 1 ) %>% 
  dplyr::select(-end_date)

vaccinatedCohort = rbind(breakthroughCovidPerson,nonBreakthroughPcrCovidPerson)
vaccinatedCohortCov = vaccinatedCohort %>% left_join(
  rbind(breakthroughCovidFeatures$demo,nonBreakthroughPcrCovidFeatures$demo)
) %>% left_join(
  rbind(breakthroughCovidFeatures$obDays,nonBreakthroughPcrCovidFeatures$obDays)
) %>% left_join(
  rbind(breakthroughCovidFeatures$visit,nonBreakthroughPcrCovidFeatures$visit)
) %>% left_join(
  rbind(breakthroughCovidFeatures$immuno,nonBreakthroughPcrCovidFeatures$immuno) %>%
    mutate(is_immunoD = T) %>%
    dplyr::select(person_id,is_immunoD) %>% distinct_all()
) %>% left_join(
  rbind(breakthroughCovidFeatures$rollingAvg, nonBreakthroughPcrCovidFeatures$rollingAvg)
) %>% distinct_all() %>% 
  mutate(age_at_index = as.integer(difftime(units = "days",index_date,DOB)/365.24)) %>%
  mutate(age_category_at_index = cut_number(x = age_at_index, n = 4)) %>%
  mutate(race_category = case_when((race == "White") ~ "White",
                                   (race == "Black or African American") ~ "Black",
                                   (race == "Asian") ~ "Asian",
                                   TRUE ~ "Other Race or Unknown")) %>%
  replace_na(list(count_of_visits = 0, is_immunoD = F, observation_days = 0,cases_avg=0, deaths_avg=0))


prevaccinatedCohort = rbind(preVaccinePcrNegativeCovidPerson,preVaccinePcrPositiveCovidPerson)
prevaccinatedCohortCov = prevaccinatedCohort %>% left_join(
  rbind(preVaccinePcrPositiveCovidFeatures$demo,preVaccinePcrNegativeCovidFeatures$demo)
) %>% left_join(
  rbind(preVaccinePcrPositiveCovidFeatures$obDays,preVaccinePcrNegativeCovidFeatures$obDays)
) %>% left_join(
  rbind(preVaccinePcrPositiveCovidFeatures$visit,preVaccinePcrNegativeCovidFeatures$visit)
) %>% left_join(
  rbind(preVaccinePcrPositiveCovidFeatures$immuno,preVaccinePcrNegativeCovidFeatures$immuno) %>%
    mutate(is_immunoD = T) %>%
    dplyr::select(person_id,is_immunoD) %>% distinct_all()
) %>% left_join(
  rbind(preVaccinePcrPositiveCovidFeatures$rollingAvg, preVaccinePcrPositiveCovidFeatures$rollingAvg)
) %>% distinct_all() %>% 
  mutate(age_at_index = as.integer(difftime(units = "days",index_date,DOB)/365.24)) %>%
  mutate(age_category_at_index = cut_number(x = age_at_index, n = 4)) %>%
  mutate(race_category = case_when((race == "White") ~ "White",
                                   (race == "Black or African American") ~ "Black",
                                   (race == "Asian") ~ "Asian",
                                   TRUE ~ "Other Race or Unknown")) %>%
  replace_na(list(count_of_visits = 0, is_immunoD = F, observation_days = 0,cases_avg=0, deaths_avg=0))


#### vax vs pre-vax #### 
# match
forMatchData = rbind(vaccinatedCohortCov %>% dplyr::select(-latest_dose_date,-time),prevaccinatedCohortCov)
set.seed(5)
# take a minute
matchIt = matchit(is_vaccinated ~ count_of_visits+
                    observation_days+gender+age_at_index+race_category+ethnicity+
                    is_immunoD + cases_avg, data = forMatchData, method="nearest", ratio=1)
plot(summary(matchIt))
matchItData = match.data(matchIt)[1:ncol(forMatchData)] 

# oddsRatioTest(table(matchItData$status, matchItData$is_vaccinated))
# oddsratio(table(matchItData$status, matchItData$is_vaccinated))
# oddsratio(table(matchItData %>% filter(age_at_index <= 65) %>% pull(status), matchItData %>% filter(age_at_index <= 65) %>% pull(is_vaccinated)))
# oddsratio(table(matchItData %>% filter(age_at_index > 65) %>% pull(status), matchItData %>% filter(age_at_index > 65) %>% pull(is_vaccinated)))

# prevalence in vax/pre-vax
table5col1 = rbind(
  t(as.matrix(matchItData %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(age_at_index <= 65) %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(age_at_index > 65) %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(gender == "MALE") %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(gender == "FEMALE") %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(is_immunoD == T) %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(is_immunoD == F) %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',]
)

table5col2 = rbind(
  t(as.matrix(matchItData %>% group_by(is_vaccinated) %>% summarise(N = sum(status))))['N',],
  t(as.matrix(matchItData %>% filter(age_at_index <= 65) %>% group_by(is_vaccinated) %>% summarise(N = sum(status))))['N',],
  t(as.matrix(matchItData %>% filter(age_at_index > 65) %>% group_by(is_vaccinated) %>% summarise(N = sum(status))))['N',],
  t(as.matrix(matchItData %>% filter(gender == "MALE") %>% group_by(is_vaccinated) %>% summarise(N = sum(status))))['N',],
  t(as.matrix(matchItData %>% filter(gender == "FEMALE") %>% group_by(is_vaccinated) %>% summarise(N = sum(status))))['N',],
  t(as.matrix(matchItData %>% filter(is_immunoD == T) %>% group_by(is_vaccinated) %>% summarise(N = sum(status))))['N',],
  t(as.matrix(matchItData %>% filter(is_immunoD == F) %>% group_by(is_vaccinated) %>% summarise(N = sum(status))))['N',]
  
)
# test
# adj for covariates
table5col3 = rbind(
  univarTest(forTest = matchItData,var = "is_vaccinated",adj = NULL,lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(age_at_index <= 65),var = "is_vaccinated",adj = NULL,lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(age_at_index > 65),var = "is_vaccinated",adj = NULL,lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(gender == "MALE"),var = "is_vaccinated",adj = NULL,lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(gender == "FEMALE"),var = "is_vaccinated",adj = NULL,lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(is_immunoD == T),var = "is_vaccinated",adj = NULL,lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(is_immunoD == F),var = "is_vaccinated",adj = NULL,lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',]
)
# further adj.
table5col4 = rbind(
  univarTest(forTest = matchItData,var = "is_vaccinated",adj = c("count_of_visits","observation_days"),lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(age_at_index <= 65),var = "is_vaccinated",adj = c("count_of_visits","observation_days"),lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(age_at_index > 65),var = "is_vaccinated",adj = c("count_of_visits","observation_days"),lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(gender == "MALE"),var = "is_vaccinated",adj = c("count_of_visits","observation_days"),lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(gender == "FEMALE"),var = "is_vaccinated",adj = c("count_of_visits","observation_days"),lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(is_immunoD == T),var = "is_vaccinated",adj = c("count_of_visits","observation_days","age_at_index"),lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',],
  univarTest(forTest = matchItData %>% filter(is_immunoD == F),var = "is_vaccinated",adj = c("count_of_visits","observation_days","age_at_index"),lr = T,cox=F,poisson = F)['is_vaccinatedTRUE',]
)

# vax vs. unvax.

unVaccinatedCohort = rbind(UnVaccinePcrNegativeCovidPerson,UnVaccinePcrPositiveCovidPerson)
unVaccinatedCohortCov = unVaccinatedCohort %>% left_join(
  rbind(postVaccinePcrPositiveCovidFeatures$demo,postVaccinePcrNegativeCovidFeatures$demo)
) %>% left_join(
  rbind(postVaccinePcrPositiveCovidFeatures$obDays,postVaccinePcrNegativeCovidFeatures$obDays)
) %>% left_join(
  rbind(postVaccinePcrPositiveCovidFeatures$visit,postVaccinePcrNegativeCovidFeatures$visit)
) %>% left_join(
  rbind(postVaccinePcrPositiveCovidFeatures$immuno,postVaccinePcrNegativeCovidFeatures$immuno) %>%
    mutate(is_immunoD = T) %>%
    dplyr::select(person_id,is_immunoD) %>% distinct_all()
) %>% left_join(
  rbind(postVaccinePcrPositiveCovidFeatures$rollingAvg, postVaccinePcrPositiveCovidFeatures$rollingAvg)
) %>% distinct_all() %>% 
  mutate(age_at_index = as.integer(difftime(units = "days",index_date,DOB)/365.24)) %>%
  mutate(age_category_at_index = cut_number(x = age_at_index, n = 4)) %>%
  mutate(race_category = case_when((race == "White") ~ "White",
                                   (race == "Black or African American") ~ "Black",
                                   (race == "Asian") ~ "Asian",
                                   TRUE ~ "Other Race or Unknown")) %>%
  replace_na(list(count_of_visits = 0, is_immunoD = F, observation_days = 0,cases_avg=0, deaths_avg=0))

# match
forMatchData = bind_rows(vaccinatedCohortCov ,unVaccinatedCohortCov %>% dplyr::select(-entry_date))
forMatchData = forMatchData %>% mutate(ldd_category= cut(x = index_date, "months"))

set.seed(5)
# take a minute
matchIt = matchit(is_vaccinated ~ count_of_visits+
                    observation_days+gender+age_at_index+race_category+ethnicity+
                    is_immunoD + ldd_category, data = forMatchData, method="nearest", ratio=1)
plot(summary(matchIt))
matchItData = match.data(matchIt)[1:ncol(forMatchData)] 

# test
# incident rate in vax/unvax
table6col1 = rbind(
  t(as.matrix(matchItData %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(age_at_index <= 65) %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(age_at_index > 65) %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(gender == "MALE") %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(gender == "FEMALE") %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(is_immunoD == T) %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',],
  t(as.matrix(matchItData %>% filter(is_immunoD == F) %>% group_by(is_vaccinated) %>% summarise(N = length(status))))['N',]
)
table6col2 = rbind(
  t(as.matrix(matchItData %>% group_by(is_vaccinated) %>% summarise(N = 1000*sum(status)/sum(time))))['N',],
t(as.matrix(matchItData %>% filter(age_at_index <= 65) %>% group_by(is_vaccinated) %>% summarise(N = 1000*sum(status)/sum(time))))['N',],

t(as.matrix(matchItData %>% filter(age_at_index > 65) %>% group_by(is_vaccinated) %>% summarise(N = 1000*sum(status)/sum(time))))['N',],
t(as.matrix(matchItData %>% filter(gender == "MALE") %>% group_by(is_vaccinated) %>% summarise(N = 1000*sum(status)/sum(time))))['N',],
t(as.matrix(matchItData %>% filter(gender == "FEMALE") %>% group_by(is_vaccinated) %>% summarise(N = 1000*sum(status)/sum(time))))['N',],
t(as.matrix(matchItData %>% filter(is_immunoD == T) %>% group_by(is_vaccinated) %>% summarise(N = 1000*sum(status)/sum(time))))['N',],
t(as.matrix(matchItData %>% filter(is_immunoD == F) %>% group_by(is_vaccinated) %>% summarise(N = 1000*sum(status)/sum(time))))['N',]
  
)


# raw IRR
table6col3 = rbind(
univarTest(forTest = matchItData,var = "is_vaccinated",adj = NULL,lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(age_at_index <= 65),var = "is_vaccinated",adj = NULL,lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(age_at_index > 65),var = "is_vaccinated",adj = NULL,lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(gender == "MALE"),var = "is_vaccinated",adj = NULL,lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(gender == "FEMALE"),var = "is_vaccinated",adj = NULL,lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(is_immunoD == T),var = "is_vaccinated",adj = NULL,lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(is_immunoD == F),var = "is_vaccinated",adj = NULL,lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',]
)
# further adj.
table6col4 = rbind(
univarTest(forTest = matchItData,var = "is_vaccinated",adj = c("count_of_visits","observation_days"),lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(age_at_index <= 65),var = "is_vaccinated",adj = c("count_of_visits","observation_days"),lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(age_at_index > 65),var = "is_vaccinated",adj = c("count_of_visits","observation_days"),lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(gender == "MALE"),var = "is_vaccinated",adj = c("count_of_visits","observation_days"),lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(gender == "FEMALE"),var = "is_vaccinated",adj = c("count_of_visits","observation_days"),lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(is_immunoD == T),var = "is_vaccinated",adj = c("count_of_visits","observation_days","age_at_index"),lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',],
univarTest(forTest = matchItData %>% filter(is_immunoD == F),var = "is_vaccinated",adj = c("count_of_visits","observation_days","age_at_index"),lr = F,cox=F,poisson = T)['is_vaccinatedTRUE',]
)
# by ldd
# res = NULL
# for(i in matchItData$ldd_category %>% unique()){
#   r = univarTest(forTest = matchItData %>% filter(ldd_category == i ),var = "is_vaccinated",adj = NULL,lr = F,cox=F,poisson = T)
#   res = rbind(res,cbind(i,r[2,1]))
# }
# res
# # further adj.
# res = NULL
# for(i in matchItData$ldd_category %>% unique()){
#   r = univarTest(forTest = matchItData %>% filter(ldd_category == i ),var = "is_vaccinated",adj = c("count_of_visits","observation_days","age_at_index"),lr = F,cox=F,poisson = T)
#   res = rbind(res,cbind(i,r[2,1]))
# }
# res

# # calculate raw IR.
# icn = (vaccinatedCohort %>% filter(is_vaccinated & status) %>% dim())[1]
# obsTimePerson = (matchItData %>% filter(is_vaccinated) %>% pull(time) %>% sum()/1000)
# icn/obsTimePerson

# IR   
# cleanedNoVaccinatedCohortPerson = cleanedVaccinatedCohort %>% left_join(breakthroughCovidPerson) %>%
#   filter(is.na(time)) %>%
#   mutate(is_vaccinated = T) %>%
#   mutate(status = 0)%>% 
#   mutate(time = as.integer(
#     difftime("2021-06-30", latest_dose_date, units = "days")) - 14) %>%
#   dplyr::select(person_id,latest_dose_date,index_date,is_vaccinated,time,status) # 182432.
# 
# vaccinatedRawCohort = rbind(breakthroughCovidPerson,cleanedNoVaccinatedCohortPerson)
# 
# icn = (vaccinatedRawCohort %>% filter(status == 1) %>% dim())[1]
# obsTimePerson = (vaccinatedRawCohort %>% pull(time) %>% sum()/1000)
# icn/obsTimePerson # incident rate by comparing positive vaccinated vs. all vaccinated

