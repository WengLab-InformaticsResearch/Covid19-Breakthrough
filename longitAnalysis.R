# Last updated: 09-15-2021
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


UnVaccinePcrPositiveCovidPerson = postVaccinePcrPositiveCovidRefined %>%
  mutate(is_vaccinated = F) %>%
  mutate(status = 1) %>%
  mutate(time = as.integer(
    difftime(index_date, entry_date,units = "days")) + 1)

UnVaccinePcrNegativeCovidPerson = postVaccinePcrNegativeCovidRefined %>%
  mutate(is_vaccinated = F) %>%
  mutate(status = 0) %>%
  mutate(end_date = index_date) %>%
  # mutate(end_date = case_when(is.na(censor_date)~as.Date("2021-06-30"),TRUE~censor_date)) %>%
  mutate(time = as.integer(
    difftime(end_date, entry_date,units = "days")) + 1) %>%
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
matchItData$time %>% summary()


irMatrix = NULL
for(ti in seq(1,241,by = 1)){
  irMatrix = matchItData %>% left_join(nonBreakthroughPcrCovidRefined %>% dplyr::select(person_id,vaccine_brand,latest_dose_date)
                            %>% bind_rows(
                              breakthroughCovidRefined %>% dplyr::select(person_id,vaccine_brand,latest_dose_date)
                            )
  ) %>% mutate(vaccine_brand = case_when(is.na(vaccine_brand)~"Un-Vax",TRUE~vaccine_brand)) %>%
    dplyr::select(person_id,time,status,vaccine_brand) %>%
    mutate(status_ti = case_when((time < ti & status == 1)~1,
                                 TRUE~0)) %>%
    mutate(time_ti = case_when(time<=ti~time,
                               (time>ti & status == 1)~as.numeric(NA),
                               TRUE~ti)) %>%
    group_by(vaccine_brand) %>%
    summarise(IR = 1000*sum(status_ti,na.rm = T)/sum(time_ti,na.rm = T), personN = sum(status_ti,na.rm = T), personDays = sum(time_ti,na.rm = T)) %>%
    mutate(time_interval = ti) %>%
    bind_rows(irMatrix)
}

# irMatrix %>% dcast(time_interval~vaccine_brand,value.var='IR') %>%
#   mutate(irr = 1 - moderna/`Un-Vax`) %>%
#   filter(time_interval > 50) %>%
#   ggplot(aes(x=time_interval,y=irr)) +
#   geom_line() +
#   xlab("Time to fully vaccinated (days) ") +
#   ylab("VE") +
#   labs(title="(A)")

p1 = irMatrix %>% filter(vaccine_brand %in% c('pfizer','moderna')) %>%
  ggplot(aes(x=time_interval,y=personN)) + 
  geom_line(aes(color = vaccine_brand)) +
  xlab("Time to fully vaccinated (days) ") +
  ylab("Cumulative incidence count") +
  theme(legend.position = "none") + 
  labs(title="(A)")
p1
p2 = irMatrix %>% filter(vaccine_brand %in% c('pfizer','moderna')) %>%
  ggplot(aes(x=time_interval,y=IR)) +
  geom_line(aes(color = vaccine_brand)) +
  xlab("Time to fully vaccinated (days) ") +
  ylab("Inccidence rate per 1000 person-days") +
  labs(title="(B)")
p2

grid.arrange(p1, p2,nrow = 1)
# per 30 days interval breakdown
irMatrix = irMatrix %>% filter(time_interval %in% seq(30,250,30))
irMatrix$this_month_person_days = irMatrix$personDays - c(irMatrix$personDays[4:length(irMatrix$personDays)],0,0,0)
irMatrix$this_month_i = irMatrix$personN - c(irMatrix$personN[4:length(irMatrix$personN)],0,0,0)
irMatrix$this_month_ir = irMatrix$this_month_i/irMatrix$this_month_person_days * 1000

table7 = irMatrix %>% filter(vaccine_brand %in% c('pfizer','moderna')) %>% dplyr::select(vaccine_brand,this_month_person_days,this_month_i,this_month_ir)

# by calendar month.
irMatrix = NULL
calTime = c("2021-01-31","2021-02-28","2021-03-31","2021-04-30","2021-05-31","2021-06-30","2021-07-31","2021-08-31","2021-09-30")
for(now in calTime){
  irMatrix = matchItData %>% left_join(nonBreakthroughPcrCovidRefined %>% dplyr::select(person_id,vaccine_brand,latest_dose_date)
                                       %>% bind_rows(
                                         breakthroughCovidRefined %>% dplyr::select(person_id,vaccine_brand,latest_dose_date)
                                       )
  ) %>% mutate(vaccine_brand = case_when(is.na(vaccine_brand)~"Un-Vax",TRUE~vaccine_brand)) %>%
    dplyr::select(person_id,time,status,vaccine_brand,latest_dose_date) %>% 
    mutate(vaccine_brand = case_when(is.na(vaccine_brand)~"Un-Vax",TRUE~vaccine_brand)) %>%
    dplyr::select(person_id,time,status,vaccine_brand,latest_dose_date) %>%
    replace_na(list(latest_dose_date = as.Date(
    "2021-01-04"
  ))) %>%
    mutate(ti = as.integer(
      difftime(now, latest_dose_date,units = "days")) -14 + 1) %>%
    mutate(status_ti = case_when((time < ti & status == 1)~1,
                                 TRUE~0)) %>%
    mutate(time_ti = case_when(time<=ti~time,
                               (time>ti & status == 1)~as.numeric(NA),
                               ti < 0~as.numeric(NA),
                               TRUE~ti)) %>% 
    group_by(vaccine_brand) %>% 
    summarise(person_days = sum(time_ti,na.rm = T), ir = 1000*sum(status_ti,na.rm = T)/sum(time_ti,na.rm = T),i = sum(status_ti,na.rm = T)) %>%
    mutate(time_interval = now) %>%
    bind_rows(irMatrix)
}
p3 = irMatrix %>% ggplot(aes(x=time_interval,y=i)) +
  geom_bar(aes(fill = vaccine_brand),stat = "identity") + 
  xlab("Calendar month") + 
  ylab("Cumulative incidence count") + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(title="(C)")
p3

irMatrix$this_month_person_days = irMatrix$person_days - c(irMatrix$person_days[4:length(irMatrix$person_days)],0,0,0)
irMatrix$this_month_i = irMatrix$i - c(irMatrix$i[4:length(irMatrix$i)],0,0,0)
irMatrix$this_month_ir = irMatrix$this_month_i/irMatrix$this_month_person_days * 1000

p4 = irMatrix %>% ggplot(aes(x=time_interval,y=this_month_ir)) +
  geom_bar(aes(fill = vaccine_brand),stat = "identity",position=position_dodge()) + 
  ylab("Incidence rate per 1000 person-days") + 
  xlab("Calendar month") + 
  theme(legend.position = "right") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  labs(title = "(D)")
p4

p1 = p1 + theme(legend.title=element_blank())
p2 = p2 + theme(legend.title=element_blank())
p3 = p3 + theme(legend.title=element_blank())
p4 = p4 + theme(legend.title=element_blank())

grid.arrange(p1,p2,p3,p4,nrow = 2)
table8 = irMatrix %>% dplyr::select(time_interval,vaccine_brand,this_month_person_days,this_month_i,this_month_ir)

