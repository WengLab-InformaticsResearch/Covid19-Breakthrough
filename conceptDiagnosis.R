# Last updated: 09-14-2021
# Author: Cong Liu
# checked version: No

source("./cohortCharacterization.R")

conceptList = fread("./eTable1.csv")
colnames(conceptList)[4] = 'domain_id'
personList = bind_rows(breakthroughCovidRefined %>% dplyr::select(person_id),
                       nonBreakthroughPcrCovidRefined %>% dplyr::select(person_id),
                       preVaccinePcrPositiveCovidRefined %>% dplyr::select(person_id),
                       preVaccinePcrNegativeCovidRefined %>% dplyr::select(person_id),
                       postVaccinePcrPositiveCovidRefined %>% dplyr::select(person_id),
                       postVaccinePcrNegativeCovidRefined %>% dplyr::select(person_id)
) %>% distinct()

writeToSqlTempdb(con,name = '#conceptList',value = conceptList)
writeToSqlTempdb(con,name = '#personList',value = personList)

sql = "
select ce.condition_concept_id,cl.concept_name, count(distinct ce.person_id) as person_count
from #conceptList cl
left join [dbo].[condition_era] ce
on ce.condition_concept_id = cl.concept_id
right join #personList pl
on ce.person_id = pl.person_id
where cl.domain_id = 'Condition'
group by ce.condition_concept_id,cl.concept_name
"
countCondition = dbGetQuery(con,sql)

sql = "
select ce.drug_concept_id,cl.concept_name, count(distinct ce.person_id) as person_count
from #conceptList cl
left join [dbo].[drug_era] ce
on ce.drug_concept_id = cl.concept_id
right join #personList pl
on ce.person_id = pl.person_id
where cl.domain_id = 'Drug'
group by ce.drug_concept_id,cl.concept_name
"
countDrug = dbGetQuery(con,sql)

sql = "
select ce.measurement_concept_id,cl.concept_name, count(distinct ce.person_id) as person_count
from #conceptList cl
left join [dbo].[measurement] ce
on ce.measurement_concept_id = cl.concept_id
right join #personList pl
on ce.person_id = pl.person_id
where cl.domain_id = 'Measurement'
group by ce.measurement_concept_id,cl.concept_name
"
countMeasurement = dbGetQuery(con,sql)

sql = "
select ce.procedure_concept_id,cl.concept_name, count(distinct ce.person_id) as person_count
from #conceptList cl
left join [dbo].[procedure_occurrence] ce
on ce.procedure_concept_id = cl.concept_id
right join #personList pl
on ce.person_id = pl.person_id
where cl.domain_id = 'Procedure'
group by ce.procedure_concept_id,cl.concept_name
"
countProcedure = dbGetQuery(con,sql)


sql = "
select ce.visit_source_value,count(distinct ce.person_id) as person_count
from #personList pl
left join [dbo].[visit_occurrence] ce
on ce.person_id = pl.person_id
where ce.visit_start_date > '2020-01-01'
group by ce.visit_source_value
"
countVisit = dbGetQuery(con,sql)

