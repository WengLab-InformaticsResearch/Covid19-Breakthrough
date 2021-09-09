# Last updated: 09-08-2021
# Author: Cong Liu
# checked version: Yes

source("./featureExtraction.R")
library(MatchIt)
library(epitools)
library(chron)
library(biostat3)
library(tibble)
library(epiR)
library(parallel)


# get matched samples based on nearest neighbors.
breakthroughCovidCov = breakthroughCovid %>% 
  left_join(breakthroughCovidFeatures$visit,by = "person_id",copy = TRUE) %>%
  left_join(breakthroughCovidFeatures$obDays,by = "person_id",copy = TRUE) %>%
  dplyr::select(person_id,latest_dose_date,earliest_dose_date,observation_days,count_of_visits,index_date) %>%
  mutate(sample = "breakthroughCovid") %>%
  replace_na(list(observation_days = 0, count_of_visits = 0))


nonBreakthroughPcrCovidCov = nonBreakthroughPcrCovid %>% 
  left_join(nonBreakthroughPcrCovidFeatures$visit,by = "person_id",copy = TRUE) %>%
  left_join(nonBreakthroughPcrCovidFeatures$obDays,by = "person_id",copy = TRUE) %>%
  dplyr::select(person_id,latest_dose_date,earliest_dose_date,observation_days,count_of_visits,index_date) %>%
  replace_na(list(observation_days = 0, count_of_visits = 0)) %>%
  mutate(sample = "nonBreakthroughPcrCovid")

forMatchData = rbind(breakthroughCovidCov,nonBreakthroughPcrCovidCov) %>%
  mutate(group = as.logical(sample == 'breakthroughCovid')) %>%
  mutate(ldd_category= cut(x = latest_dose_date, "months")) %>%
  mutate(status = as.numeric(group)) %>%
  mutate(time = if_else(condition = (status == 1), 
                        true = (as.integer(difftime(units = "days",index_date,latest_dose_date) - 14)), 
                        false = (as.integer(difftime(units = "days","2021-06-30",latest_dose_date) - 14))
                        )
          )

# set.seed(5)
# # take a minute
# matchIt = matchit(group ~ ldd_category + observation_days + count_of_visits, data = forMatchData, method="nearest", ratio=10)
# plot(summary(matchIt))
# matchItData = match.data(matchIt)[1:ncol(forMatchData)] 
# totalBtCount = table(matchItData$group)
# matchItData = matchItData %>% 
#   mutate(status = as.numeric(group)) %>% 
#   mutate(time = as.integer(difftime(units = "days",index_date,latest_dose_date) - 14))
# 
# # prepare outcome
# # breakthroughCovid %>% mutate(time_to_event = difftime(units = "day",evidence_date,latest_dose_date)) %>% 
# #   dplyr::select(person_id,time_to_event)

# # add var of interest for cox regression analysis.
# # control -- check visit
# visitForChisqTest = matchItData %>%
#   mutate(visit_category= cut_number(x = count_of_visits, n = 5))
# oddsRatioTest(table(visitForChisqTest$group, visitForChisqTest$visit_category))
# 
# # control -- check observation_days
# obDaysForChisqTest = matchItData %>%
#   mutate(obDays_category= cut_number(x = observation_days, n = 5))
# oddsRatioTest(table(obDaysForChisqTest$group, obDaysForChisqTest$obDays_category))
# 
# # control -- check observation_days
# lddForChisqTest = matchItData %>%
#   mutate(ldd_category= cut(x = latest_dose_date, "months"))
# oddsRatioTest(table(lddForChisqTest$group, lddForChisqTest$ldd_category))


# brand
brand = rbind(breakthroughCovidFeatures$brand,nonBreakthroughPcrCovidFeatures$brand)
# brandForTest = matchItData %>% left_join(brand,by="person_id",copy = TRUE)
# oddsRatioTest(table(brandForChisqTest$group, brandForChisqTest$vaccine_brand))
# brandForPoissonTest = matchItData %>% left_join(brand,by="person_id",copy = TRUE)
# brandForCoxTest = matchItData %>%
#   left_join(brand,by="person_id",copy = TRUE)
brandForTest = forMatchData %>%
  left_join(brand,by="person_id",copy = TRUE)

brandResults = univarTest(brandForTest,"vaccine_brand",
                          adj=c("ldd_category","count_of_visits","observation_days"))

# demo
demo = rbind(breakthroughCovidFeatures$demo,nonBreakthroughPcrCovidFeatures$demo)
# matchItData vs. forMatchData
demoForTest = forMatchData %>% left_join(demo,by = "person_id") %>%
  mutate(age_at_event = as.integer(difftime(units = "days",latest_dose_date,DOB)/365.24)) %>%
  mutate(age_category_at_event = cut_number(x = age_at_event,n = 2)) %>%
  mutate(race_category = case_when((race == "White") ~ "White",
                          (race == "Black or African American") ~ "Black",
                          (race == "Asian") ~ "Asian",
                          TRUE ~ "Other/Unknown")) %>% 
  mutate(ethnicity_category = case_when((ethnicity=='Hispanic or Latino')~"Hispanic",
                                        ethnicity=='Not Hispanic or Latino'~"Not Hispanic",
                                        TRUE~"Other/Unknown"))

covariates = c("age_category_at_event", "race_category",  "ethnicity_category", "gender")
demoResults = do.call("rbind", sapply(covariates,
                                      function(x) univarTest(demoForTest,x,
                                                             adj=c("ldd_category","count_of_visits","observation_days"))))

# oddsRatioTest(table(demoForChisqTest$group, demoForChisqTest$gender))
# oddsRatioTest(table(demoForChisqTest$group, demoForChisqTest$race_category))
# oddsRatioTest(table(demoForChisqTest$group, demoForChisqTest$ethnicity_category))
# oddsRatioTest(table(demoForChisqTest$group, demoForChisqTest$age_category_at_event))

# immuno supressed test
immuno = rbind(breakthroughCovidFeatures$immuno,nonBreakthroughPcrCovidFeatures$immuno)
orderedLevelsCategory = c("no", "tumor", "ckd", "hiv",
                          "immuno_suppress_drug", "immuno_deficiency","transplant")
isImmunoLevelsCategory = c("Not Immuno Comprised", "Immuno Comprised")
immunoForTest = forMatchData %>% left_join(immuno,by = "person_id") %>% 
  dplyr::select(person_id,group,category,time,status,ldd_category,count_of_visits,observation_days) %>%
  replace(., is.na(.), "no") %>%
  mutate(isImmuo = case_when((category=='no')~'Not Immuno Comprised',TRUE~'Immuno Comprised')) %>%
  mutate(isImmuo = factor(isImmuo,levels = isImmunoLevelsCategory)) %>%
  mutate(category = factor(category,levels = orderedLevelsCategory)) %>%
  distinct_all() %>%
  dcast(person_id+group+time+status+ldd_category+count_of_visits+observation_days+isImmuo~category,fun = length)
covariates = c("isImmuo", "tumor","ckd","hiv","immuno_suppress_drug","immuno_deficiency","transplant")

immunoResults = lapply(covariates,
                       function(x) univarTest(immunoForTest,x,
                                              adj=c("ldd_category","count_of_visits","observation_days"))
)

# adj for age.
immunoResultsAdj = lapply(covariates,
                        function(x) univarTest(immunoForTest %>% left_join(demoForTest),x,
                                               adj=c("age_at_event","ldd_category","count_of_visits","observation_days")
                                               ))


brandResultsAdj = as.data.frame(univarTest(brandForTest %>% left_join(demoForTest),"vaccine_brand",
                          adj=c("age_at_event","ldd_category","count_of_visits","observation_days")))

# do.call("rbind", list(
# brandResults,
# brandResultsAdj,
# demoResults,
# immunoResults,
# immunoResultsAdj)) %>% write.csv("./breakthroughAnalysisRiskFactors.csv",sep = ",",row.names = T)

# forMultiTest = demoForTest %>% left_join(brandForCoxTest) %>% left_join(immunoForTest)
# multivarTest(forMultiTest,c("age_category_at_event", "race_category",  "ethnicity_category", "gender","vaccine_brand","isImmuo", "category"))

# oddsRatioTest(table(immunoForChisqTest$group, immunoForChisqTest$isImmuo))
# 
# for(i in immunoForChisqTest$category %>% unique()){
#   immunoForChisqTestSub = immunoForChisqTest %>% filter(category %in% c('no',i))
#   oddsRatioTest(table(immunoForChisqTestSub$group, immunoForChisqTestSub$category))
# }


# condition and drug
condition = rbind(breakthroughCovidFeatures$condition,nonBreakthroughPcrCovidFeatures$condition)
colnames(condition)[2] = "concept_id"
drug = rbind(breakthroughCovidFeatures$drug,nonBreakthroughPcrCovidFeatures$drug)
colnames(drug)[2] = "concept_id"
concept = rbind(condition,drug)
matchItDataConcept = forMatchData %>% left_join(concept,by = "person_id") %>% left_join(demoForTest) %>%
  dplyr::select(person_id,group,concept_id,time,status,count_of_visits,observation_days,ldd_category,age_at_event) %>%
  distinct_all() %>%
  as.data.table() 
matchItDataConceptSum = matchItDataConcept[,.(conceptNBtCount = .N - sum(group), conceptBtCount = sum(group)),by = concept_id]
matchItDataConceptFiltered = matchItDataConceptSum[(conceptNBtCount + conceptBtCount) > 10] %>% left_join(matchItDataConcept) %>%
  dplyr::select(person_id, time, status, count_of_visits,observation_days,ldd_category, age_at_event, concept_id)
matchItDataConceptWide = dcast(matchItDataConceptFiltered, person_id + time + status + count_of_visits + observation_days + ldd_category + age_at_event ~ concept_id,fun = length)
covariates = colnames(matchItDataConceptWide)[9:dim(matchItDataConceptWide)[2]]
length(covariates) # 5422
# test.
# conceptRes = t(sapply(covariates[1:10],function(x) univarTest(matchItDataConceptWide,x)))
conceptRes = t(sapply(covariates,function(x) univarConceptScreen(matchItDataConceptWide,x)))
colnames(conceptRes) = c("irrCI", "irrP")
conceptRes = conceptRes %>% as.data.frame() %>% rownames_to_column(var = "concept_id")
# matchItDataConcept = matchItDataConcept[,.(conceptNBtCount = .N - sum(group), conceptBtCount = sum(group)),by = concept_id]
# matchItDataConcept[,AllNBtCount := totalBtCount['FALSE']]
# matchItDataConcept[,AllBtCount := totalBtCount['TRUE']]
# # matchItDataCondition = matchItDataConcept[(conceptNBtCount + conceptBtCount) > 10]
# matchItDataOrResults = matchItDataConcept[,dataTableGroupOddsRatioTest(conceptNBtCount,conceptBtCount,AllNBtCount,AllBtCount),by = concept_id]
matchItDataOrTopResults = conceptRes %>% 
  mutate(irrFdr = p.adjust(as.numeric(irrP),method = "bonferroni")) %>% 
  filter(irrFdr < 0.05)
matchItDataOrTopResults = matchItDataOrTopResults %>% mutate(concept_id = as.integer(concept_id))
writeToSqlTempdb(con = con,"#matchItDataOrTopResults",matchItDataOrTopResults)
conceptName = extractConceptName(con,"#matchItDataOrTopResults")
matchItDataOrTopResultsName = matchItDataOrTopResults %>% left_join(conceptName) %>%
  arrange(irrFdr)
matchItDataOrTopResultsName %>% fwrite("./breakthroughAnalysisRiskFactorsAllConcepts90daysPrior.csv",sep = ",")
matchItDataOrTopResults = conceptRes %>% 
  mutate(irrFdr = p.adjust(as.numeric(irrP),method = "bonferroni")) %>% 
  filter(irrFdr <= 1)
matchItDataOrTopResults = matchItDataOrTopResults %>% mutate(concept_id = as.integer(concept_id))
writeToSqlTempdb(con = con,"#matchItDataOrTopResults",matchItDataOrTopResults)
conceptName = extractConceptName(con,"#matchItDataOrTopResults")
matchItDataOrTopResultsName = matchItDataOrTopResults %>% left_join(conceptName) %>%
  arrange(irrFdr)
matchItDataOrTopResultsName %>% fwrite("./breakthroughAnalysisRiskFactorsAllConcepts90daysPriorFullList.csv",sep = ",")
# all test stratified by age, gender and brand
# stratifyImmunoForChisqTest = immunoForChisqTest %>% left_join(demoForChisqTest) %>% 
#   left_join(brand) %>%left_join(immuno)
# for(i in unique(stratifyImmunoForChisqTest$vaccine_brand)){
#   print(i)
#   forChisqTest = stratifyImmunoForChisqTest %>% filter(vaccine_brand %in% i)
#   oddsRatioTest(table(forChisqTest$group, forChisqTest$isImmuo))
# }
# for(i in unique(stratifyImmunoForChisqTest$age_category_at_event)){
#   print(i)
#   forChisqTest = stratifyImmunoForChisqTest %>% filter(age_category_at_event %in% i)
#   oddsRatioTest(table(forChisqTest$group, forChisqTest$isImmuo))
# }
# for(i in unique(stratifyImmunoForChisqTest$gender)){
#   print(i)
#   forChisqTest = stratifyImmunoForChisqTest %>% filter(gender %in% i)
#   oddsRatioTest(table(forChisqTest$group, forChisqTest$isImmuo))
# }
