# Last updated: 09-20-2021
# Author: Cong Liu
# checked version: Yes

# source("./cohortCharacterizationAndRefine.R")


# get matched samples based on nearest neighbors.
breakthroughCovidCov = breakthroughCovidRefined %>% 
  left_join(breakthroughCovidFeatures$visit,by = "person_id",copy = TRUE) %>%
  left_join(breakthroughCovidFeatures$obDays,by = "person_id",copy = TRUE) %>%
  dplyr::select(person_id,latest_dose_date,earliest_dose_date,observation_days,count_of_visits,index_date) %>%
  mutate(sample = "breakthroughCovid") %>%
  replace_na(list(observation_days = 0, count_of_visits = 0))


nonBreakthroughPcrCovidCov = nonBreakthroughPcrCovidRefined %>% 
  left_join(nonBreakthroughPcrCovidFeatures$visit,by = "person_id",copy = TRUE) %>%
  left_join(nonBreakthroughPcrCovidFeatures$obDays,by = "person_id",copy = TRUE) %>%
  dplyr::select(person_id,latest_dose_date,earliest_dose_date,observation_days,count_of_visits,index_date) %>%
  replace_na(list(observation_days = 0, count_of_visits = 0)) %>%
  mutate(sample = "nonBreakthroughPcrCovid")


immuno = rbind(breakthroughCovidFeatures$immuno,nonBreakthroughPcrCovidFeatures$immuno)

forMatchData = rbind(breakthroughCovidCov,nonBreakthroughPcrCovidCov) %>%
  mutate(group = as.logical(sample == 'breakthroughCovid')) %>%
  mutate(ldd_category= cut(x = latest_dose_date, "months")) %>%
  mutate(status = as.numeric(group)) %>%
  mutate(time = if_else(condition = (status == 1), 
                        true = (as.integer(difftime(units = "days",index_date,latest_dose_date) - 14)) + 1, 
                        false = (as.integer(difftime(units = "days",index_date,latest_dose_date) - 14)) + 1
                        )
          )

# overall incidence rate
overallIR = 1000*epi.conf(matrix(apply(forMatchData %>% dplyr::select(status,time),2,sum),nrow = 1),ctype = 'inc.rate',method = 'exact',N = 1000)
overallIR
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

res=NULL

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
for(r in c("moderna","pfizer")){
  res = rbind(res,cbind(r,1000*epi.conf(matrix(apply(brandForTest %>% filter(vaccine_brand == r) 
                                               %>% dplyr::select(status,time),2,sum),nrow = 1),
                                  ctype = 'inc.rate',method = 'exact',N = 1000)))
}

# demo
demo = rbind(breakthroughCovidFeatures$demo,nonBreakthroughPcrCovidFeatures$demo)
# matchItData vs. forMatchData
demoForTest = forMatchData %>% left_join(demo,by = "person_id") %>%
  mutate(age_at_event = as.integer(difftime(units = "days",latest_dose_date,DOB)/365.24)) %>%
  mutate(age_category_at_event = cut(age_at_event, breaks=c(0, 65,Inf), include.lowest=TRUE) ) %>%
  mutate(race_category = case_when((race == "White") ~ "White",
                          (race == "Black or African American") ~ "Black",
                          (race == "Asian") ~ "Asian",
                          TRUE ~ "Other/Unknown")) %>% 
  mutate(ethnicity_category = case_when((ethnicity=='Hispanic or Latino')~"Hispanic",
                                        ethnicity=='Not Hispanic or Latino'~"Not Hispanic",
                                        TRUE~"Other/Unknown")) %>% distinct()

covariates = c("age_category_at_event", "race_category",  "ethnicity_category", "gender")
demoResults = do.call("rbind", sapply(covariates,
                                      function(x) univarTest(demoForTest,x,adj=c("ldd_category","count_of_visits","observation_days"))))


for(r in c("[0,65]","(65,Inf]")){
  res = rbind(res,cbind(r,
    1000*epi.conf(matrix(apply(demoForTest %>% filter(age_category_at_event == r) 
                               %>% dplyr::select(status,time),2,sum),nrow = 1),
                  ctype = 'inc.rate',method = 'exact',N = 1000)
  ))
}
for(r in c("Asian","Black","White","Other/Unknown")){
  res = rbind(res,cbind(r,
  1000*epi.conf(matrix(apply(demoForTest %>% filter(race_category == r) 
                             %>% dplyr::select(status,time),2,sum),nrow = 1),
                ctype = 'inc.rate',method = 'exact',N = 1000)
  ))
}
for(r in c("Hispanic","Not Hispanic","Other/Unknown")){
  res = rbind(res,cbind(r,
    1000*epi.conf(matrix(apply(demoForTest %>% filter(ethnicity_category == r) 
                               %>% dplyr::select(status,time),2,sum),nrow = 1),
                  ctype = 'inc.rate',method = 'exact',N = 1000)
  ))
}

for(r in c("FEMALE","MALE")){
  res = rbind(res,cbind(r,
    1000*epi.conf(matrix(apply(demoForTest %>% filter(gender == r) 
                               %>% dplyr::select(status,time),2,sum),nrow = 1),
                  ctype = 'inc.rate',method = 'exact',N = 1000)
  ))
}



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

for(r in c("Not Immuno Comprised","Immuno Comprised")){
  res = rbind(res,cbind(r,
    1000*epi.conf(matrix(apply(immunoForTest %>% filter(isImmuo == r) 
                               %>% dplyr::select(status,time),2,sum),nrow = 1),
                  ctype = 'inc.rate',method = 'exact',N = 1000)
  ))
}

for(r in covariates){
  res = rbind(res,cbind(r,
    1000*epi.conf(matrix(apply(immunoForTest %>% filter(!!as.symbol(r) == 1) 
                               %>% dplyr::select(status,time),2,sum),nrow = 1),
                  ctype = 'inc.rate',method = 'exact',N = 1000)
  ))
}


immunoResults = lapply(covariates,
                       function(x) univarTest(immunoForTest,x,
                                              adj=c("ldd_category","count_of_visits","observation_days"))
)

table2Col1 = res
demoRows = c(2,13,14,15,26,27,38,39)
brandRows = c(2)
table2Col2 = rbind(demoResults[demoRows,],(brandResults %>% as.data.frame())[brandRows,])
for(i in c(1:length(immunoResults))){
  table2Col2 = rbind(table2Col2,(immunoResults[[i]] %>% as.data.frame())[2,])
}
table2Col2


# adj for covariates
immunoResultsAdj = lapply(covariates,
                        function(x) univarTest(immunoForTest %>% left_join(demoForTest),x,
                                               adj=c("age_at_event","ldd_category","count_of_visits","observation_days")
                                               ))


brandResultsAdj = as.data.frame(univarTest(brandForTest %>% left_join(immunoForTest %>% dplyr::select(person_id,isImmuo) %>% distinct())
                                           %>% left_join(demoForTest),"vaccine_brand",
                          adj=c("age_at_event","ldd_category","count_of_visits","observation_days","isImmuo")))
brandForTest %>% ggplot(aes(y=time,x=as.factor(status))) + geom_boxplot(aes(colour = vaccine_brand))
immunobrand = brandForTest %>% left_join(immunoForTest %>% dplyr::select(person_id,isImmuo) %>% distinct())
brandRows = c(2)
table2Col3 = rbind((brandResultsAdj %>% as.data.frame())[brandRows,])
for(i in c(1:length(immunoResultsAdj))){
  table2Col3 = rbind(table2Col3,(immunoResultsAdj[[i]] %>% as.data.frame())[2,])
}
table2Col3
immunobrand %>% mutate(g = case_when(status == 1~"Vax Positive", T~"Vax Negative")) %>% 
  mutate(x = case_when(isImmuo=='Immuno Comprised'~'Immunocompromised',T~'Not Immunocompromised')) %>%
  group_by(vaccine_brand,g,x) %>% summarise(N = n()) %>% 
  ggplot(aes(x, N, fill = vaccine_brand)) + 
  geom_bar(stat="identity", position = "dodge") + facet_wrap(~as.factor(g),nrow = 2,scales = "free_y")
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
# concept = rbind(condition,drug)
conditionConceptRes = univariateTestForAllconcept(forMatchData,demoForTest = demoForTest,concept = condition)
drugConceptRes = univariateTestForAllconcept(forMatchData,demoForTest = demoForTest,concept = drug)
table3 = resultsDisplay(conceptRes = conditionConceptRes,con=con)
table3 %>% fwrite(file = './breakthroughAnalysisRiskFactorsAllCondition90daysPriorFullList.csv')
table4 = resultsDisplay(conceptRes = drugConceptRes,con=con)
table4 %>% fwrite(file = './breakthroughAnalysisRiskFactorsAllDrug90daysPriorFullList.csv')
# test.
# conceptRes = t(sapply(covariates[1:10],function(x) univarTest(matchItDataConceptWide,x)))

# matchItDataConcept = matchItDataConcept[,.(conceptNBtCount = .N - sum(group), conceptBtCount = sum(group)),by = concept_id]
# matchItDataConcept[,AllNBtCount := totalBtCount['FALSE']]
# matchItDataConcept[,AllBtCount := totalBtCount['TRUE']]
# # matchItDataCondition = matchItDataConcept[(conceptNBtCount + conceptBtCount) > 10]
# matchItDataOrResults = matchItDataConcept[,dataTableGroupOddsRatioTest(conceptNBtCount,conceptBtCount,AllNBtCount,AllBtCount),by = concept_id]



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
