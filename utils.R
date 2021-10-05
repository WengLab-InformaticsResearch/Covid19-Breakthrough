# Last updated: 09-09-2021
# Author: Cong Liu
# checked version: No

# db
library(data.table)
library(dplyr)
library(MatchIt)
library(epitools)
library(chron)
library(biostat3)
library(tibble)
library(epiR)
library(parallel)
library(survival)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(RODBC)
ohdsiConnection = function(server = 'server', database = 'ohdsi', uid = 'uid', PWD = NULL){
  library(odbc)
  ######################################################
  # FUNCTION ohdsiConnection
  # - connect to the ohdsi database
  # - return a formal class Microsoft SQL Server
  # TBD: support for configure file
  ######################################################
  con = tryCatch({
    if(is.null(PWD)){
      dbConnect(odbc(),
                Driver = 'ODBC Driver 17 for SQL Server',
                Server = server,
                Database = database,
                UID = uid,
                PWD = rstudioapi::askForPassword("Please input password"),
                Port = 1433)
    }
    else{
      dbConnect(odbc(),
                Driver = 'ODBC Driver 17 for SQL Server',
                Server = server,
                Database = database,
                UID = uid,
                PWD = PWD,
                Port = 1433)
    }
  }, error = function(e){
    print(e)
    return(NULL)
  })
  return(con)
}
tempdbConnection = function(server = 'server', database = 'ohdsi', uid = 'uid', PWD = NULL){
  library(RODBC)
  ######################################################
  # FUNCTION tempdbConnection
  # - connect to the sql tempdb
  # - return a formal class Microsoft SQL Server
  ######################################################
  con = tryCatch({
      channel <- odbcConnect(paste0("DRIVER={ODBC Driver 17 for SQL Server};
                                 server=",server,";
                                 database=","tempdb",";
                                 uid=",uid,";
                                 pwd=",rstudioapi::askForPassword("Please input password")))   
    
  }, error = function(e){
    print(e)
    return(NULL)
  })
  return(con)
}
writeToSqlTempdb = function(con,name,value){
  tryCatch({
    sql = paste0("DROP TABLE " ,name)
    DBI::dbExecute(con, sql)
  }, error = function(e){
    print(e)
    return(NULL)
  })
  tryCatch({
    dbWriteTable(con,name,value)
  }, error = function(e){
    print(e)
    return(NULL)
  })
  tryCatch({
    sql = paste0("SELECT Top 10 * from ",name)
    dbGetQuery(con,sql)
  }, error = function(e){
    print(e)
    return(NULL)
  })
}

# extraction OHDSI
extractConceptName = function(con,name){
  sql = paste0("
    select c.concept_id, concept_name, domain_id
    from ",name," b
    left join [dbo].[concept] c
    on c.concept_id = b.concept_id
  ")
  conceptName = dbGetQuery(con,sql)
  return(conceptName)
}
extractDemo = function(con,name){
  sql = paste0("
    select p.person_id,
    CONVERT(date,p.birth_datetime) as DOB,
    c1.concept_name as gender,
    c2.concept_name as race,
    c3.concept_name as ethnicity,
    SUBSTRING(l.zip, 1, 5) as zip
    from ",name," b
    left join [dbo].[person] p 
    on b.person_id = p.person_id
    left join [dbo].[concept] c1
    on c1.concept_id = p.gender_concept_id
    left join [dbo].[concept] c2
    on c2.concept_id = race_concept_id
    left join [dbo].[concept] c3
    on c3.concept_id = ethnicity_concept_id
    left join [dbo].[location] l
    on p.location_id = l.location_id
  ")
  demo = dbGetQuery(con,sql)
  return(demo)
}
extractVisit = function(con,name){
  sql = paste0("
    SELECT v.person_id, count(distinct v.visit_occurrence_id) as count_of_visits
               from ",
               name," b
    left join [dbo].[visit_occurrence] v
    on b.person_id = v.person_id
    where DATEADD(day, 90, v.visit_start_date) < b.index_date
    group by v.person_id
  ")
  visit = dbGetQuery(con,sql)
  return(visit)
}
extractObservationDays = function(con,name){
  sql = paste0("
  SELECT T.person_id, sum(T.period_days) as observation_days FROM
  (
    SELECT o.person_id, datediff(day,
    observation_period_start_date,
    observation_period_end_date) as period_days
               from ",
               name," b
    left join [dbo].[observation_period] o
    on b.person_id = o.person_id
    where DATEADD(day, 90, o.observation_period_start_date) < b.index_date
  ) T
  group by T.person_id
  ")
  visit = dbGetQuery(con,sql)
  return(visit)
}
extractConditions = function(con,name,distinct=TRUE){
  sql = paste0("
    SELECT DISTINCT c.person_id, c.condition_concept_id
               from ",
               name," b
    left join [dbo].[condition_era] c
    on b.person_id = c.person_id
    where DATEADD(day, 90, c.condition_era_start_date) < b.index_date
    and c.condition_concept_id != 0
  ")
  conditions = dbGetQuery(con,sql)
  return(conditions)
}
extractDrugs = function(con,name,distinct=TRUE){
  sql = paste0("
    SELECT DISTINCT d.person_id, d.drug_concept_id
               from ",
               name," b
    left join [dbo].[drug_era] d
    on b.person_id = d.person_id
    where DATEADD(day, 90, d.drug_era_start_date) < b.index_date
    and d.drug_concept_id != 0
  ")
  drugs = dbGetQuery(con,sql)
  return(drugs)
}
extractImmuno = function(con,name,distinct=TRUE){
  sql = paste0("
    SELECT DISTINCT c.person_id, 
    c.condition_concept_id as concept_id,
    'Condition' as domain_id,
    'immuno_deficiency' as category
               from ",
               name," b
    left join [dbo].[condition_era] c
    on b.person_id = c.person_id
    left join concept_ancestor ca 
    on c.condition_concept_id = ca.descendant_concept_id
    where ca.ancestor_concept_id in (433740)
    and DATEADD(day, 90, c.condition_era_start_date) < b.index_date
  ")
  immuno = dbGetQuery(con,sql)
  tumor = extractTumor(con,name)
  hiv = extractHiv(con,name)
  transplant = extractTransplant(con,name)
  immunoSupDrug = extractImmunoSupDrug(con,name)
  kidney = extractKidneyDisease(con,name)
  # no asplenia, Q89, etc in cumc ohdsi db.
  # asplenia = extractAsplenia(con,name)
  
  immuno = rbindlist(list(immuno,tumor,hiv,transplant,immunoSupDrug,kidney))
  return(immuno)
}
extractTumor = function(con,name,distinct=TRUE){
  sql = paste0("
  WITH all_cancer AS (
  -- Find all the possible cancer codes
  SELECT DISTINCT CA.descendant_concept_id
  FROM concept_ancestor CA JOIN concept C ON CA.descendant_concept_id = C.concept_id
  WHERE CA.ancestor_concept_id IN (
  	
  	-- All cancer dx
  	SELECT concept_id
  	FROM concept C
  	WHERE (C.vocabulary_id = 'SNOMED' AND C.standard_concept = 'S' 
  		   AND C.concept_id IN ('438112'))
  
  	)
  ), exclude_cancer AS (
  -- Find all the exclusion cancer dx codes
  SELECT DISTINCT CA.descendant_concept_id
  FROM concept_ancestor CA JOIN concept C ON CA.descendant_concept_id = C.concept_id
  WHERE CA.ancestor_concept_id IN (
  	
  	-- Exclusion dx (the first two are regarding benign cancers while
  	-- the remaining are the exceptions, with the 4179980 basal cell and the rest f hcc)
  	SELECT concept_id
  	FROM concept C
  	WHERE C.vocabulary_id = 'SNOMED' AND C.standard_concept = 'S' 
  		   AND C.concept_id IN ('435506', '4112852', '4155297', '4031756', '4116082')
  	)
  
  ), final_cancer AS (

    -- In summary:
    -- Select all cancer dx, except the recognized benign ones, hcc, and basal cell  
    SELECT C.concept_id, C.concept_name,C.domain_id
    FROM concept C
    WHERE C.concept_id IN (SELECT AC.descendant_concept_id FROM all_cancer AC)
    	  AND C.concept_id NOT IN (SELECT EC.descendant_concept_id 
    							   FROM exclude_cancer EC
    							   )
    )
    SELECT DISTINCT c.person_id, c.condition_concept_id as concept_id,
    'Condition' as domain_id,
    'tumor' as category
               from ",
               name," b
    left join [dbo].[condition_era] c
    on b.person_id = c.person_id
    inner join final_cancer f
    on c.condition_concept_id = f.concept_id
    where DATEADD(day, 90, c.condition_era_start_date) < b.index_date
    and DATEADD(day, 365*2, c.condition_era_start_date) > b.index_date
    and c.condition_concept_id != 0
  ")
  tumor = dbGetQuery(con,sql)
  return(tumor)
}
extractHiv = function(con,name,distinct=TRUE){
  sql = paste0("
    SELECT DISTINCT c.person_id, 
    c.condition_concept_id as concept_id, 
    'Condition' as domain_id,
    'hiv' as category
               from ",
               name," b
    left join [dbo].[condition_era] c
    on b.person_id = c.person_id
    left join concept_ancestor ca 
    on c.condition_concept_id = ca.descendant_concept_id
    where ca.ancestor_concept_id in (439727, 432554, 4241530)
    and DATEADD(day, 90, c.condition_era_start_date) < b.index_date
    UNION ALL
    SELECT DISTINCT d.person_id, d.drug_concept_id as concept_id, 
    'Drug' as domain_id,
    'hiv' as category
               from ",
               name," b
    left join [dbo].[drug_era] d
    on b.person_id = d.person_id
    left join concept_ancestor ca 
    on d.drug_concept_id = ca.descendant_concept_id
    where ca.ancestor_concept_id in (
    SELECT concept_id
		FROM concept
		WHERE standard_concept = 'S' AND domain_id = 'Drug' AND concept_class_id = 'Ingredient' AND
			  (LOWER(concept_name) LIKE '%abacavir%' OR LOWER(concept_name) LIKE '%didanosine%' OR 
			   LOWER(concept_name) LIKE '%emtricitabine%' OR LOWER(concept_name) LIKE '%lamivudine%' OR 
			   LOWER(concept_name) LIKE '%stavudine%' OR LOWER(concept_name) LIKE '%tenofovir%' OR 
			   LOWER(concept_name) LIKE '%zidovudine%' OR LOWER(concept_name) LIKE '%efavirenz%' OR 
			   LOWER(concept_name) LIKE '%etravirine%' OR LOWER(concept_name) LIKE '%nevirapine%' OR 
			   LOWER(concept_name) LIKE '%rilpivirine%' OR LOWER(concept_name) LIKE '%atazanavir%' OR 
			   LOWER(concept_name) LIKE '%darunavir%' OR LOWER(concept_name) LIKE '%fosampre%' OR 
			   LOWER(concept_name) LIKE '%indinavir%' OR LOWER(concept_name) LIKE '%nelfinavir%' OR 
			   LOWER(concept_name) LIKE '%ritonavir%' OR LOWER(concept_name) LIKE '%saquinavir%' OR 
			   LOWER(concept_name) LIKE '%tipranavir%' OR LOWER(concept_name) LIKE '%dolutegravir%' OR 
			   LOWER(concept_name) LIKE '%elvitegravir%' OR LOWER(concept_name) LIKE '%raltegravir%' OR 
			   LOWER(concept_name) LIKE '%enfuvirtide%' OR LOWER(concept_name) LIKE '%maraviroc%' OR 
			   LOWER(concept_name) LIKE '%cobicistat%')
    )
    and DATEADD(day, 90, d.drug_era_start_date) < b.index_date
    ")
  hiv = dbGetQuery(con,sql)
  return(hiv)
}
extractTransplant = function(con,name,distinct=TRUE){
  sql = paste0("
    SELECT DISTINCT c.person_id, 
    c.condition_concept_id as concept_id,
    'Condition' as domain_id,
    'transplant' as category
               from ",
               name," b
    left join [dbo].[condition_era] c
    on b.person_id = c.person_id
    left join concept_ancestor ca 
    on c.condition_concept_id = ca.descendant_concept_id
    where ca.ancestor_concept_id in (42537741)
    and DATEADD(day, 90, c.condition_era_start_date) < b.index_date
  ")
  transplant = dbGetQuery(con,sql)
  return(transplant)
}
extractImmunoSupDrug = function(con,name,distinct=TRUE){
  sql = paste0("
  SELECT DISTINCT d.person_id, d.drug_concept_id as concept_id, 
  'Drug' as domain_id,
  'immuno_suppress_drug' as category
  from ",name," b
  left join [dbo].[drug_era] d
  on b.person_id = d.person_id
  left join concept_ancestor ca 
  on d.drug_concept_id = ca.descendant_concept_id
  where ca.ancestor_concept_id in (
    SELECT concept_id
	FROM concept C
	WHERE C.vocabulary_id = 'RxNorm' AND
		  C.concept_code IN ('614391',  '1311600',  '327361',  '299635',  '117055',  '72435',  '1492727',  '1256',  '2047232',  '196102',  '1112973',  '1092437',  
		  '1872251',  '853491',  '709271',  '44157',  '3008',  '190353',  '1373478',  '591781',  '356988',  '2104604',  '214555',  '141704',  
		  '1012892',  '819300',  '1928588',  '191831',  '1745099',  '27169',  '342369',  '1011',  '6851',  '42405',  '7145',  '354770',  '1876366',  
		  '2288236',  '1592254',  '1369713',  '2107301',  '763450',  '2166040',  '1923319',  '1599788',  '1535218',  '2121085',  '35302',  '42316',  
		  '1310520',  '10432',  '2053436',  '612865',  '1357536',  '2196092',  '847083',  '1538097',
		  '239', '1371041', '1430438', '1232150', '1727455', '81864',
		  '5296', '683', '739', '596724', '18330', '1156', '1792776', '1875534', '1242999',
		  '1251', '1543543', '134547', '253337', '233272', '2049122', '1622', '1597258',
		  '358258','1307619',	'1147320',	'1921217',	'1828',	'996051','1363268','194000',	
		  '40048',	'1302966',	'2105',	'140587',	'1535457',	'318341',	'2346',	'2555',	'44157',	
		  '44151',	'1722365',	'1945077',	'1148495',	'3002',	'3041',	'1424911',	'3098',	'2058849',	
		  '3100',	'1721947',	'475342',	'3109',	'15657',	'214470',	'23066',	'72962',	
		  '3639',	'1919503',	'80726',	'1726104',	'1940332',	'2049106',	'3995',	'1045453',	'337525',	
		  '4089',	'4132',	'4179',	'141704',	'4488',	'24698',	'4492',	'328134',	'12574',	'1294580',	
		  '1316105',	'5552',	'1442981',	'5650',	'1544460',	'5657',	'282388',	'1942950',	'1094833',	'51499',	
		  '337523',	'1723735',	'480167',	'1603296',	'6466',	'2103164',	'227239',	'6674',	'6718',	'103',	'6851',	
		  '337068',	'1919083',	'1494066',	'6996',	'632',	'7004',	'7005',	'2054068',	'1723738',	'274771',	
		  '1940643',	'662281',	'1592737',	'1918231',	'1597876',	'974779',	'712566',	'1597582',	'1855735',	
		  '27100',	'1721560',	'32592',	'56946',	'1601374',	'263034',	'1603350',	'714438',	'34132',	
		  '1547545',	'68446',	'8011',	'1298944',	'8347',	'33764', '6995', 
		  '1364347',	'662019',	'8637',	'8702',	'196239',	'1535922',	'1312397',	
		  '1873916',	'121191',	'877510',	'1862579',	'1193326',	'1733681',	'1659191',	
		  '495881',	'10114',	'357977',	'2099704',	'4582',	'115243',	'37776',	'657797',	
		  '10362',	'10485',	'10473',	'57308',	'1716278',	'1425099',	'224905',	'38508',	
		  '10753',	'38865',	'10996',	'31435',	'1098413',	'1147220',	'1747556',	'11198',	
		  '11202',	'11204',	'39541', '1242987', '194337' )
  )
  and DATEADD(day, 90, d.drug_era_start_date) < b.index_date"
  )
  immunoSupDrug = dbGetQuery(con,sql)
  return(immunoSupDrug)
}
extractKidneyDisease = function(con,name,distinct=TRUE){
  sql = paste0("
    SELECT DISTINCT c.person_id, 
    c.condition_concept_id as concept_id,
    'Condition' as domain_id,
    'ckd' as category
               from ",
               name," b
    left join [dbo].[condition_era] c
    on b.person_id = c.person_id
    left join concept_ancestor ca 
    on c.condition_concept_id = ca.descendant_concept_id
    where ca.ancestor_concept_id in (46271022)
    and DATEADD(day, 90, c.condition_era_start_date) < b.index_date
  ")
  ckd = dbGetQuery(con,sql)
  return(ckd)
}
extractAsplenia = function(con,name,distinct=TRUE) {
  sql = paste0("
    SELECT DISTINCT c.person_id, 
    c.condition_concept_id as concept_id,
    'Condition' as domain_id,
    'asplenia' as category
               from ",
               name," b
    left join [dbo].[condition_era] c
    on b.person_id = c.person_id
    left join concept_ancestor ca 
    on c.condition_concept_id = ca.descendant_concept_id
    where ca.ancestor_concept_id in (45768671,35104781)
    and DATEADD(day, 90, c.condition_era_start_date) < b.index_date 
  ")
  asplenia = dbGetQuery(con,sql)
  return(asplenia)
}
extractOutcomes = function(con,name,distinct=TRUE){
  ventilation = extractVentilation(con,name)
  tracheostomy = extractTracheostomy(con,name)
  death = extractDeath(con,name)
  inpatient = extractInpatient(con,name)
  icu = extractIcu(con,name)
  outcome = rbindlist(list(ventilation,tracheostomy,inpatient,icu,death))
  return(outcome)
}
extractVentilation = function(con,name,earliest=TRUE){
  sql = paste0("
    SELECT DISTINCT p.person_id, 
    procedure_concept_id as concept_id,
    p.procedure_date as event_date,
    'Procedure' as domain_id,
    'ventilation' as category
               from ",
               name," b
    left join [dbo].[procedure_occurrence] p
    on b.person_id = p.person_id
    where DATEADD(day, 3, p.procedure_date) >= b.index_date and 
    DATEADD(day, -28, p.procedure_date) <= b.index_date and 
    procedure_concept_id in (765576,2007912,2008006,2008007,2008008,2008009,2106469,2106470,
    2106642,2314003,2314035,2314036,2514578,2745440,2745444,2745447,2787823,2787824,2788016,
    2788017,2788019,2788020,2788021,2788022,2788024,2788025,2788026,2788027,4013354,4026054,
    4055261,4055374,4055375,4055376,4055377,4055378,4055379,4056812,4057263,4058031,4072503,
    4072504,4072505,4072506,4072507,4072515,4072516,4072517,4072518,4072519,4072520,4072521,
    4072522,4072523,4072631,4072633,4074665,4074666,4074667,4074668,4074669,4074670,4080896,
    4080957,4082243,4085542,4097246,4113618,4119642,4120570,4134538,4134853,4140765,4149878,
    4164571,4168475,4173351,4174085,4174555,4177224,4179373,4202819,4208272,4219631,4229907,
    4230167,4235361,4236738,4237460,4244053,4245036,4251737,4254209,4283075,4283807,4287921,
    4287922,4296607,4303945,4308797,4331311,4332501,4335481,4335583,4335584,4335585,4337045,
    4337046,4337047,4337048,4337615,4337616,4337617,4337618,4339623,36676550,37116689,37116698,
    37206832,40486624,40487536,40489935,44790095,44791135)
  ")
  ventilation = dbGetQuery(con,sql)
  return(ventilation)
}
extractTracheostomy = function(con,name,earliest=TRUE){
  sql = paste0("
    SELECT DISTINCT p.person_id, 
    procedure_concept_id as concept_id,
    p.procedure_date as event_date,
    'Procedure' as domain_id,
    'tracheostomy' as category
               from ",
               name," b
    left join [dbo].[procedure_occurrence] p
    on b.person_id = p.person_id
    where DATEADD(day, 3, p.procedure_date) >= b.index_date and 
    DATEADD(day, -28, p.procedure_date) <= b.index_date and 
    procedure_concept_id in (2106470,2106564,2106570,2106571,2108641,2108642,2741578,
    2741580,2741589,2741675,2743216,2745483,2745491,2745499,2745507,2745515,2794811,2829384,
    2829386,2831237,2836115,2862930,2870619,4195473,4208093,4311023,4331311,4337047)
  ")
  tracheostomy = dbGetQuery(con,sql)
  return(tracheostomy)
}
extractDeath = function(con,name,earliest=TRUE){
  sql = paste0("
    SELECT DISTINCT d.person_id, 
    0 as concept_id,
    d.death_date as event_date,
    'Death' as domain_id,
    'Death' as category
               from ",
               name," b
    left join [dbo].[DEATH] d
    on b.person_id = d.person_id
    where DATEADD(day, 3, d.death_date) >= b.index_date and
    DATEADD(day, -28, d.death_date) <= b.index_date
  ")
  death = dbGetQuery(con,sql)
  return(death)
}
extractInpatient = function(con,name,earliest=TRUE){
  sql = paste0("
    SELECT DISTINCT v.person_id, 
    0 as concept_id,
    v.visit_start_date as event_date,
    'Visit' as domain_id,
    'Inpatient' as category
               from ",
               name," b
    left join [dbo].[visit_occurrence] v
    on b.person_id = v.person_id
    where v.visit_concept_id in (262,9201,9203,32037,32760)
    and DATEADD(day, 3, v.visit_start_date) >= b.index_date and
    DATEADD(day, -28, v.visit_start_date) <= b.index_date
  ")
  inpatient = dbGetQuery(con,sql)
  return(inpatient)
}
extractIcu = function(con,name,earliset=TRUE){
  sql = paste0("
    SELECT DISTINCT v.person_id, 
    0 as concept_id,
    v.visit_start_date as event_date,
    'Visit' as domain_id,
    'ICU' as category
               from ",
               name," b
    left join [dbo].[visit_occurrence] v
    on b.person_id = v.person_id
    where (v.visit_source_value like '%intensive%'
      or v.visit_source_value like '%icu%')
    and DATEADD(day, 3, v.visit_start_date) >= b.index_date and 
    DATEADD(day, -28, v.visit_start_date) <= b.index_date
        UNION ALL
    SELECT DISTINCT v.person_id, 
    0 as concept_id,
    v.visit_start_date as event_date,
    'Visit' as domain_id,
    'Inpatient' as category
               from ",
               name," b
    left join [dbo].[visit_detail] v
    on b.person_id = v.person_id
    where v.visit_detail_concept_id = 32037
    and DATEADD(day, 3, v.visit_start_date) >= b.index_date and
    DATEADD(day, -28, v.visit_start_date) <= b.index_date
  ") 
  icu = dbGetQuery(con,sql)
  return(icu)
}
extractLastVisit = function(con,name, ob = T){
  if(ob == F){
    sql = paste0("
    SELECT v.person_id, 
    0 as concept_id,
    max(v.visit_end_date) as censored_date,
    'Visit' as domain_id,
    'last_records' as category
               from ",
                 name," b
    left join [dbo].[visit_occurrence] v
    on b.person_id = v.person_id
    group by v.person_id
  ")
    last = dbGetQuery(con,sql)
  }else{
    sql = paste0("
    SELECT v.person_id, 
    0 as concept_id,
    max(v.observation_period_end_date) as censored_date,
    'observation_period' as domain_id,
    'last_records' as category
               from ",
               name," b
    left join [dbo].[observation_period] v
    on b.person_id = v.person_id
    group by v.person_id
  ")
  last = dbGetQuery(con,sql)
  }
  return(last)
}

# URL resourcees
extractSevenDayRollingAverageCaseDeath = function(value=breakthroughCovid,file="us-counties.csv"){
  nytDt = read.csv(file)
  nytFilterDt = nytDt %>% filter(county == 'New York City') %>%
    mutate(index_date =  as.Date(date))
  # seven-day rolling average
  sevenDayRollingAverageCaseDeath = breakthroughCovid %>% left_join(nytFilterDt) %>%
    dplyr::select(person_id,index_date,cases_avg,deaths_avg)
  return(sevenDayRollingAverageCaseDeath)
}
# math.
getMeanSd = function(x,name="indexedAge"){
  # mean_name = paste0(name,"_mean")
  # sd_name = paste0(name,"_sd")
  return(tibble(!!name:=paste0(round(mean(x,na.rm = T),1)," (",round(sd(x,na.rm = T),2),")")))
}
oddsRatioTest = function(cTable){
    mosaicplot(t(cTable), col = c("firebrick", "goldenrod1"), cex.axis = 1, sub = "", ylab = "Relative frequency of Covid breakthrough", main = "")
    # print(oddsratio(cTable))
    print(chisq.test(cTable))
    print(addmargins(cTable))
}
dataTableGroupOddsRatioTest = function(a,b,c,d){
  tryCatch({
    tapw <- c("T", "F")
    outc <- c("T", "F")	
    dat <- matrix(c(a,b,c-a,d-b),2,2,byrow=TRUE)
    dimnames(dat) <- list("Concept" = tapw, "Breakthrough" = outc)
    r = oddsratio.midp(dat, rev="c")
    odds = r$measure[2,1]
    pvalue = r$p.value[2,1]
    l = list(odds = odds, pvalue=pvalue)
    return(l)
  },error = function(e) {
    print(paste('error:', e))
  })
  return(list(odds = as.numeric(NA), pvalue= as.numeric(NA)))
}

univarTest = function(forTest,var,adj=c("age_at_event","ldd_category","count_of_visits","observation_days"),cox=F,lr=F,poisson=T){
  if(!is.null(adj)){
    adj_var = paste0("+`",paste0(adj,collapse = "`+`"),"`")
  }else{
    adj_var = ""
  }
  res = NULL
  if(cox){
    coxFit = coxph(as.formula(paste0("Surv(time, status) ~ `",var,"`  ",adj_var)),data = forTest)
    x = eform(coxFit)
    if(is.null(dim(x))){
      hr = signif(x[1], digits=3)
      hrConfLower = signif(x["2.5 %"], digits=3)
      hrConfUpper = signif(x["97.5 %"], digits=3)
      hrCI = paste0(hr, " (", 
                    hrConfLower, "-", hrConfUpper, ")")
      hrP = coef(summary(coxFit))[1,'Pr(>|z|)']
      res = cbind(res,hrCI,hrP)
      rownames(res) = rownames(coef(summary(coxFit)))
    }else{
      hr = signif(x[,1], digits=3)
      hrConfLower = signif(x[,"2.5 %"], digits=3)
      hrConfUpper = signif(x[,"97.5 %"], digits=3)
      hrCI = paste0(hr, " (", 
                    hrConfLower, "-", hrConfUpper, ")")
      hrP = coef(summary(coxFit))[,'Pr(>|z|)']
      levelNames = rownames(x)
      res = cbind(res,hrCI,hrP)
      rownames(res) = levelNames
    }          
   
  }
  if(lr){
    lrFit = glm(as.formula(paste0("status ~ `",var,"`  ",adj_var)),data = forTest,family = binomial)
    x = eform(lrFit)
    or = signif(x[,1], digits=3)
    orConfLower = signif(x[,"2.5 %"], digits=3)
    orConfUpper = signif(x[,"97.5 %"], digits=3)
    orCI = paste0(or, " (", 
                  orConfLower, "-", orConfUpper, ")")
    orP = coef(summary(lrFit))[,'Pr(>|z|)']
    levelNames = rownames(x)
    res = cbind(res,orCI,orP)
    rownames(res) = levelNames
  }
  if(poisson){
    # offset term to person-days
    poissonFit = glm(as.formula(paste0("status~offset(log(time)) + `",var,"`  ",adj_var)), data=forTest, family = "poisson")
    x = eform(poissonFit)
    irr = signif(x[,1], digits=3)
    irrConfLower = signif(x[,"2.5 %"], digits=3)
    irrConfUpper = signif(x[,"97.5 %"], digits=3)
    irrCI = paste0(irr, " (", 
                   irrConfLower, "-", irrConfUpper, ")")
    irrP = coef(summary(poissonFit))[,'Pr(>|z|)']
    levelNames = rownames(x)
    res = cbind(res,irrCI, irrP)
    rownames(res) = levelNames
  }
  return(res)
}

univarConceptScreen = function(forTest,var,adj=c("age_at_event","ldd_category","count_of_visits","observation_days"),cox=F,lr=F,poisson=T){
  adj_var = paste0(adj,collapse = "`+`")
  res = NULL
  if(cox){
    coxFit = coxph(as.formula(paste0("Surv(time, status) ~ `",var,"` + `",adj_var,"`")),data = forTest)
    x = eform(coxFit)
    hr = signif(x[,1], digits=3)
    hrConfLower = signif(x[,"2.5 %"], digits=3)
    hrConfUpper = signif(x[,"97.5 %"], digits=3)
    hrCI = paste0(hr, " (", 
                  hrConfLower, "-", hrConfUpper, ")")
    hrP = coef(summary(coxFit))[,'Pr(>|z|)']
    levelNames = rownames(x)
    res = cbind(res,hrCI,hrP)
    rownames(res) = levelNames
  }
  if(lr){
    lrFit = glm(as.formula(paste0("status ~ `",var,"` + `",adj_var,"`")),data = forTest,family = binomial)
    x = eform(lrFit)
    or = signif(x[,1], digits=3)
    orConfLower = signif(x[,"2.5 %"], digits=3)
    orConfUpper = signif(x[,"97.5 %"], digits=3)
    orCI = paste0(or, " (", 
                  orConfLower, "-", orConfUpper, ")")
    orP = coef(summary(lrFit))[,'Pr(>|z|)']
    levelNames = rownames(x)
    res = cbind(res,orCI,orP)
    rownames(res) = levelNames
  }
  if(poisson){
    # offset term to person-days
    poissonFit = glm(as.formula(paste0("status~offset(log(time)) + `",var,"` + `",adj_var,"`")), data=forTest, family = "poisson")
    x = eform(poissonFit)
    irr = signif(x[2,1], digits=3)
    irrConfLower = signif(x[2,"2.5 %"], digits=3)
    irrConfUpper = signif(x[2,"97.5 %"], digits=3)
    irrCI = paste0(irr, " (", 
                   irrConfLower, "-", irrConfUpper, ")")
    irrP = coef(summary(poissonFit))[2,'Pr(>|z|)']
    levelNames = rownames(x)[2]
    res = cbind(res,irrCI, irrP)
    rownames(res) = levelNames
  }
  return(res)
}

multivarTest = function(forTest,var_vector){
  var = paste0(var_vector,collapse = "`+`")
  coxFit = coxph(as.formula(paste0("Surv(time, status) ~ `",var,"` + count_of_visits+observation_days+ldd_category")),data = forTest)
  lrFit = glm(as.formula(paste0("status~`",var,"` + count_of_visits+observation_days+ldd_category")),data = forTest,family = binomial)
  # offset term to person-days
  poissonFit = glm(as.formula(paste0("status~offset(log(time)) + `",var,"` + count_of_visits+observation_days+ldd_category")), data=forTest, family = poisson)
  numberOflevels = dim(coxFit$var)[1] - 6
  x = eform(coxFit)
  hr = signif(x[1:numberOflevels,1], digits=3)
  hrConfLower = signif(x[1:numberOflevels,"2.5 %"], digits=3)
  hrConfUpper = signif(x[1:numberOflevels,"97.5 %"], digits=3)
  hrCI = paste0(hr, " (", 
                hrConfLower, "-", hrConfUpper, ")")
  hrP = coef(summary(coxFit))[1:numberOflevels,'Pr(>|z|)']
  x = eform(lrFit)
  or = signif(x[2:(numberOflevels+1),1], digits=3)
  orConfLower = signif(x[2:(numberOflevels+1),"2.5 %"], digits=3)
  orConfUpper = signif(x[2:(numberOflevels+1),"97.5 %"], digits=3)
  orCI = paste0(or, " (", 
                orConfLower, "-", orConfUpper, ")")
  orP = coef(summary(lrFit))[2:(numberOflevels+1),'Pr(>|z|)']
  
  x = eform(poissonFit)
  irr = signif(x[2:(numberOflevels+1),1], digits=3)
  irrConfLower = signif(x[2:(numberOflevels+1),"2.5 %"], digits=3)
  irrConfUpper = signif(x[2:(numberOflevels+1),"97.5 %"], digits=3)
  irrCI = paste0(irr, " (", 
                 irrConfLower, "-", irrConfUpper, ")")
  irrP = coef(summary(poissonFit))[2:(numberOflevels+1),'Pr(>|z|)']
  levelNames = rownames(x)[2:(numberOflevels+1)]
  res = cbind(hrCI,hrP, orCI, orP, irrCI, irrP)
  rownames(res) = levelNames
  
  return(res)
}

univariateTestForAllconcept = function(forMatchData,concept,demoForTest){
  matchItDataConcept = forMatchData %>% left_join(concept,by = "person_id") %>% left_join(demoForTest) %>%
    dplyr::select(person_id,group,concept_id,time,status,count_of_visits,observation_days,ldd_category,age_at_event) %>%
    distinct_all() %>%
    as.data.table() 
  matchItDataConceptSum = matchItDataConcept[,.(conceptNBtCount = .N - sum(group), conceptBtCount = sum(group)),by = concept_id]
  matchItDataConceptFiltered = matchItDataConceptSum[(conceptNBtCount + conceptBtCount) > 100] %>% left_join(matchItDataConcept) %>%
    dplyr::select(person_id, time, status, count_of_visits,observation_days,ldd_category, age_at_event, concept_id)
  matchItDataConceptWide = dcast(matchItDataConceptFiltered, person_id + time + status + count_of_visits + observation_days + ldd_category + age_at_event ~ concept_id,fun = length)
  covariates = colnames(matchItDataConceptWide)[9:dim(matchItDataConceptWide)[2]]
  print(length(covariates)) # 1895
  conceptRes = t(sapply(covariates,function(x) univarConceptScreen(matchItDataConceptWide,x)))
  colnames(conceptRes) = c("irrCI", "irrP")
  conceptRes = conceptRes %>% as.data.frame() %>% rownames_to_column(var = "concept_id")
  conceptRes$irrP = as.numeric(conceptRes$irrP)
  return(conceptRes) 
}

resultsDisplay = function(conceptRes,con){
  matchItDataOrTopResults = conceptRes %>% mutate(concept_id = as.integer(concept_id)) %>%
    mutate(irrFdr = p.adjust(as.numeric(irrP),method = "fdr"))
  writeToSqlTempdb(con = con,"#matchItDataOrTopResults",matchItDataOrTopResults)
  # table3 = matchItDataOrTopResults
  conceptName = extractConceptName(con,"#matchItDataOrTopResults")
  matchItDataOrTopResults = matchItDataOrTopResults %>% left_join(conceptName) %>%
    arrange(irrP)
  return(matchItDataOrTopResults)
}



  
