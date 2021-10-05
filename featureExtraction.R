# Last updated: 09-08-2021
# Author: Cong Liu
# checked version: Yes

source("./cohortDefinition.R")



extractFeatures = function(name="#breakthroughCovid",value=breakthroughCovid,con,extractVaccineBrand=T,extractOutcomes=T){
  # load to temp db.
  writeToSqlTempdb(con,name,value)
  # extract demographic
  demo = extractDemo(con,name)
  # extract encounter count.
  visit = extractVisit(con,name)
  # extract observation period count.
  obDays = extractObservationDays(con, name)
  # extract condition
  condition = extractConditions(con,name)
  # extract drugs
  drug = extractDrugs(con,name)
  # extract Immunocompromised concepts
  immuno = extractImmuno(con,name)
  # extract vaccine brand
  brand = NULL
  if(extractVaccineBrand){
    brand = value %>% dplyr::select(person_id,vaccine_brand)
  }
  outcome = NULL
  if(extractOutcomes){
    outcome = extractOutcomes(con,name)
  }
  last = extractLastVisit(con,name,ob = T)
  rollingAvg = extractSevenDayRollingAverageCaseDeath(value)
  return(list(obDays=obDays,visit=visit,demo=demo,brand=brand,condition=condition,
              drug=drug,immuno=immuno,outcome=outcome,rollingAvg=rollingAvg,last=last))
}

#### breakthrough Cohort ####
# index date = PCR test results date.

breakthroughCovidFeatures = extractFeatures(name="#breakthroughCovid",value=breakthroughCovid,con=con,extractVaccineBrand = T,extractOutcomes=T)
# nonBreakthroughCovidFeatures = extractFeatures(name="#nonBreakthroughCovid",value=nonBreakthroughCovid,con=con,extractVaccineBrand = T)
nonBreakthroughPcrCovidFeatures = extractFeatures(name="#nonBreakthroughPcrCovid",value=nonBreakthroughPcrCovid,con=con,extractVaccineBrand = T,extractOutcomes=T)
preVaccinePcrPositiveCovidFeatures = extractFeatures(name="#preVaccinePcrPositiveCovid",value=preVaccinePcrPositiveCovid,con=con,extractVaccineBrand = F,extractOutcomes=T)
preVaccinePcrNegativeCovidFeatures = extractFeatures(name="#preVaccinePcrNegativeCovid",value=preVaccinePcrNegativeCovid,con=con,extractVaccineBrand = F,extractOutcomes=T)
postVaccinePcrPositiveCovidFeatures = extractFeatures(name="#postVaccinePcrPositiveCovid",value=postVaccinePcrPositiveCovid,con=con,extractVaccineBrand = F,extractOutcomes=T)
postVaccinePcrNegativeCovidFeatures = extractFeatures(name="#postVaccinePcrNegativeCovid",value=postVaccinePcrNegativeCovid,con=con,extractVaccineBrand = F,extractOutcomes=T)

