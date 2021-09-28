# Last updated: 09-08-2021
# Author: Cong Liu
# checked version: Yes

source("./featureExtraction.R")


nyczip = read.csv("./nyc-zip-codes.csv")
summarizeFeatures = function(featuresList=breakthroughCovidFeatures,value=breakthroughCovid){
  value = value %>% 
    left_join(featuresList$demo %>% dplyr::select(DOB,person_id,zip) %>% distinct(),by = "person_id",copy = TRUE) %>%
    mutate(age_at_index = as.integer(difftime(units = "days",index_date,DOB)/365.24)) %>%
    filter(age_at_index >= 18) %>% 
    filter(zip %in% c(nyczip$ZipCode)) %>% 
    dplyr::select(-DOB,-age_at_index,-zip) %>% distinct()
  
  N = value %>% pull(person_id) %>% unique() %>% length()
  demo = value %>% left_join(featuresList$demo) %>% distinct()
  gender = demo %>% mutate(gender = case_when((gender=="No matching concept") ~ "Other gender or Unknown", 
                           TRUE~gender)) %>% distinct() %>% pull(gender) %>% table() %>% as_tibble()
  race = demo %>% mutate(race_category = case_when((race == "White") ~ "White",
                                          (race == "Black or African American") ~ "Black",
                                          (race == "Asian") ~ "Asian",
                                          TRUE ~ "Other Race or Unknown")) %>% distinct() %>% 
    pull(race_category) %>% table() %>% as_tibble()
  ethnicity = demo %>% mutate(ethnicity = case_when((ethnicity=="No matching concept") ~ "Other ethnicity or Unknown", 
                              TRUE~ethnicity)) %>% distinct() %>% pull(ethnicity) %>% table() %>% as_tibble()
  indexedAge = demo %>%
    mutate(indexed_age = as.integer(
      difftime(index_date, DOB, units = "days"))/365.24) %>% 
    pull(indexed_age) %>% 
    getMeanSd(name = "indexed_age") %>% gather()
  visit = value %>% left_join(featuresList$visit) %>% pull(count_of_visits) %>% getMeanSd(name = "visits_count_before_index") %>% gather()
  obDays = value %>% left_join(featuresList$obDays) %>% pull(observation_days) %>% getMeanSd(name="obsdays_count_before_index") %>% gather()
  
  df1 = rbind(gender,race,ethnicity)
  colnames(df1) = c("key","value")
  df1 = rbind(df1,indexedAge,visit,obDays)
  df1 = df1 %>% add_row(key = "N", value = as.character(N))

  if(!is.null(featuresList$brand)){
    df2 = value %>% left_join(featuresList$brand) %>% pull(vaccine_brand) %>% table() %>% as_tibble()
    colnames(df2) = c("key","value")
    df1 = rbind(df1,df2)
  }
  if(!is.null(featuresList$immuno)){
    df3 = value %>% left_join(featuresList$immuno) %>% dplyr::select(person_id,category) %>% distinct_all() %>% pull(category) %>% table() %>% as_tibble()
    total = value %>% pull(person_id) %>% unique() %>% length() 
    isImmuno = value %>% left_join(featuresList$immuno) %>% filter(!is.na(category)) %>% pull(person_id) %>% unique() %>% length() 
    colnames(df3) = c("key","value")
    df1 = rbindlist(list(df1,df3))
    df1 = df1 %>% add_row(key = "notImmuno", value = as.character(total - isImmuno))
  }
  if(!is.null(featuresList$outcome)){
    df4 = value %>% left_join(featuresList$outcome) %>% dplyr::select(person_id,category) %>% distinct_all() %>% pull(category) %>% table() %>% as_tibble()
    total = value %>% left_join(featuresList$demo) %>% pull(person_id) %>% unique() %>% length() 
    isSevere = value %>% left_join(featuresList$outcome) %>% filter(!is.na(category)) %>% pull(person_id) %>% unique() %>% length() 
    colnames(df4) = c("key","value")
    df1 = rbindlist(list(df1,df4))
    df1 = df1 %>% add_row(key = "notSevere", value = as.character(total - isSevere))
  }
  return(list(view=df1,value=value))
}

# refine and summary
refinedAndSummaryList = summarizeFeatures(breakthroughCovidFeatures,value = breakthroughCovid)
breakthroughCovidRefined = refinedAndSummaryList$value
breakthroughCovidView = refinedAndSummaryList$view
colnames(breakthroughCovidView)[2] = 'Vax_positive'
refinedAndSummaryList = summarizeFeatures(nonBreakthroughPcrCovidFeatures,value = nonBreakthroughPcrCovid)
nonBreakthroughPcrCovidRefined = refinedAndSummaryList$value
nonBreakthroughPcrCovidView = refinedAndSummaryList$view
colnames(nonBreakthroughPcrCovidView)[2] = 'Vax_negative'
refinedAndSummaryList = summarizeFeatures(postVaccinePcrPositiveCovidFeatures, value = postVaccinePcrPositiveCovid)
postVaccinePcrPositiveCovidRefined = refinedAndSummaryList$value
postVaccinePcrPositiveCovidView = refinedAndSummaryList$view
colnames(postVaccinePcrPositiveCovidView)[2] = 'Un-Vax_positive'
refinedAndSummaryList = summarizeFeatures(postVaccinePcrNegativeCovidFeatures, value = postVaccinePcrNegativeCovid)
postVaccinePcrNegativeCovidRefined = refinedAndSummaryList$value
postVaccinePcrNegativeCovidView = refinedAndSummaryList$view
colnames(postVaccinePcrNegativeCovidView)[2] = 'Un-Vax_negative'
refinedAndSummaryList = summarizeFeatures(preVaccinePcrPositiveCovidFeatures, value = preVaccinePcrPositiveCovid)
preVaccinePcrPositiveCovidRefined = refinedAndSummaryList$value
preVaccinePcrPositiveCovidView = refinedAndSummaryList$view
colnames(preVaccinePcrPositiveCovidView)[2] = 'Pre-Vax_positive'
refinedAndSummaryList = summarizeFeatures(preVaccinePcrNegativeCovidFeatures, value = preVaccinePcrNegativeCovid)
preVaccinePcrNegativeCovidRefined = refinedAndSummaryList$value
preVaccinePcrNegativeCovidView = refinedAndSummaryList$view
colnames(preVaccinePcrNegativeCovidView)[2] = 'Pre-Vax_negative'

# Table 1
table1 = breakthroughCovidView %>% full_join(nonBreakthroughPcrCovidView) %>%
  full_join(postVaccinePcrPositiveCovidView) %>%
  full_join(postVaccinePcrNegativeCovidView) %>%
  full_join(preVaccinePcrPositiveCovidView) %>%
  full_join(preVaccinePcrNegativeCovidView)