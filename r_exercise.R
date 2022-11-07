
# Data source - https://simulacrum.healthdatainsight.org.uk/simulacrum-data/

pacman::p_load(tidyverse,here,janitor,flextable, lubridate, survival)

sim_av_patient <- read_csv(list.files(here('data'), 
                             pattern='sim_av_patient.csv', 
                             recursive=T, 
                             full.names=T))

sim_av_tumour <- read_csv(list.files(here('data'), 
                                      pattern='sim_av_tumour.csv', 
                                      recursive=T, 
                                      full.names=T))


# Quesiton 1 --------------------------------------------------------------

# a.	What is the sex distribution of patients?

(sex_distribution <- sim_av_patient %>% mutate(sex_factor=factor(SEX, levels=c(1,2,9), 
                                                                 labels=c('Males', 'Females', 'Unknown'))) %>% 
    tabyl(sex_factor) %>% 
    adorn_pct_formatting(digits=1) %>% 
    flextable() %>%  
    autofit()  %>% 
    set_header_labels(.,
                      sex_factor = "Sex",
                      n = "Count", 
                      percent = "%")
)


# Question 2 --------------------------------------------------------------

#   b.	Prepare a dataset with all lung cancer patients (SITE_ICD10_O2_3CHAR= C34) and their cause of deaths.

(lung_cancer_patients_cod <-
  sim_av_patient %>% select(PATIENTID, contains('DEATHCAUSE')) %>%
  right_join(
    sim_av_tumour %>% filter(SITE_ICD10_O2_3CHAR == 'C34') %>%
      select(PATIENTID, SITE_ICD10_O2_3CHAR),
    by = c('PATIENTID')
  )
)


# Question 3 --------------------------------------------------------------
# c.	Calculate the mean survival time in days for the lung cancer patients who were diagnosed in 2013.
(lung_cancer_patients_2013<-
   sim_av_patient %>% select(PATIENTID, NEWVITALSTATUS, VITALSTATUSDATE) %>%
   right_join(
     sim_av_tumour %>% filter(SITE_ICD10_O2_3CHAR %in% c('C34')) %>%
       mutate(year=lubridate::year(DIAGNOSISDATEBEST)) %>% filter(year==2013) %>% 
       select(PATIENTID, SITE_ICD10_O2_3CHAR, DIAGNOSISDATEBEST),
     by = c('PATIENTID')
   ) %>% 
   mutate(
     os_days = as.duration(DIAGNOSISDATEBEST %--% VITALSTATUSDATE) / ddays(1),
     flag=case_when(NEWVITALSTATUS=='D' ~ 1, 
                    TRUE ~ 0)
   )
)

print(survfit(Surv(os_days, flag) ~ 1, data = lung_cancer_patients_2013), print.rmean=TRUE)

km_fit <- survfit(Surv(os_days, flag) ~ 1, data = lung_cancer_patients_2013)
plot(km_fit, xlab="Days", main = 'K-M Plot') 

  
