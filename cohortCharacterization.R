# Last updated: 09-08-2021
# Author: Cong Liu
# checked version: Yes

source("./featureExtraction.R")

library(epiR)
library(tibble)


summarizeFeatures = function(featuresList=breakthroughCovidFeatures,value=breakthroughCovid){
  demo = featuresList$demo
  gender = demo %>% pull(gender) %>% table() %>% as_tibble()
  race = demo %>% mutate(race_category = case_when((race == "White") ~ "White",
                                          (race == "Black or African American") ~ "Black",
                                          (race == "Asian") ~ "Asian",
                                          TRUE ~ "Other Race or Unknown")) %>% 
    pull(race_category) %>% table() %>% as_tibble()
  ethnicity = demo %>% pull(ethnicity) %>% table() %>% as_tibble()
  indexedAge = demo %>% left_join(value) %>%
    mutate(indexed_age = as.integer(
      difftime(index_date, DOB, units = "days"))/365.24) %>% 
    pull(indexed_age) %>% 
    getMeanSd(name = "indexed_age") %>% gather()
  visit = featuresList$visit %>% pull(count_of_visits) %>% getMeanSd(name = "visits_count_before_index") %>% gather()
  obDays = featuresList$obDays %>% pull(observation_days) %>% getMeanSd(name="obsdays_count_before_index") %>% gather()
  
  df1 = rbind(gender,race,ethnicity)
  colnames(df1) = c("key","value")
  df1 = rbind(df1,indexedAge,visit,obDays) 
  if(!is.null(featuresList$brand)){
    df2 = featuresList$brand %>% pull(vaccine_brand) %>% table() %>% as_tibble()
    colnames(df2) = c("key","value")
    df1 = rbind(df1,df2)
  }
  if(!is.null(featuresList$immuno)){
    df3 = featuresList$immuno %>% dplyr::select(person_id,category) %>% distinct_all() %>% pull(category) %>% table() %>% as_tibble()
    total = featuresList$demo %>% pull(person_id) %>% unique() %>% length() 
    isImmuno = featuresList$immuno %>% pull(person_id) %>% unique() %>% length() 
    colnames(df3) = c("key","value")
    df1 = rbind(df1,df3)
    df1 = df1 %>% add_row(key = "no", value = total - isImmuno)
  }
  if(!is.null(featuresList$outcome)){
    df4 = featuresList$outcome %>% dplyr::select(person_id,category) %>% distinct_all() %>% pull(category) %>% table() %>% as_tibble()
    total = featuresList$demo %>% pull(person_id) %>% unique() %>% length() 
    isSevere = featuresList$outcome %>% pull(person_id) %>% unique() %>% length() 
    colnames(df4) = c("key","value")
    df1 = rbind(df1,df4)
    df1 = df1 %>% add_row(key = "no", value = total - isSevere)
  }
  return(df1)
}

# Table 1
view1 = summarizeFeatures(breakthroughCovidFeatures)
view2 = summarizeFeatures(nonBreakthroughPcrCovidFeatures,value = nonBreakthroughPcrCovid)
view3 = summarizeFeatures(postVaccinePcrPositiveCovidFeatures, value = postVaccinePcrPositiveCovid)
view4 = summarizeFeatures(postVaccinePcrNegativeCovidFeatures, value = postVaccinePcrNegativeCovid)
view5 = summarizeFeatures(preVaccinePcrPositiveCovidFeatures, value = preVaccinePcrPositiveCovid)
view6 = summarizeFeatures(preVaccinePcrNegativeCovidFeatures, value = preVaccinePcrNegativeCovid)
