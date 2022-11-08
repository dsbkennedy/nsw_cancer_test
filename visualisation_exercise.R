# 3.	Visualising Data in Power BI
# 
# Using the UK synthetic cancer registry data (Simulacrum):
#   
#   a.	Visualise the proportion of all ethnic groups among non-British male patients.
# You can find ethnic information of patients in NHS ethnic category code
# b.	Explore the data to find an interesting or insightful aspect that you can then communicate visually. Be creative!
#   

pacman::p_load(tidyverse,here,janitor,flextable, lubridate, survival, magrittr)

sim_av_patient <- read_csv(list.files(here('data'), 
                                      pattern='sim_av_patient.csv', 
                                      recursive=T, 
                                      full.names=T))

sim_av_tumour <- read_csv(list.files(here('data'), 
                                     pattern='sim_av_tumour.csv', 
                                     recursive=T, 
                                     full.names=T))

sim_av_patient$ETHNICITY_factor =  recode_factor(
  sim_av_patient$ETHNICITY,
  "A" = "(White) British",
  "B" = "(White) Irish",
  "C" = "Any other White background",
  "D" = "White and Black Caribbean",
  "E" = "White and Black African",
  "F" = "White and Asian",
  "G" = "Any other mixed background",
  "H" = "Indian",
  "J" = "Pakistani",
  "K" = "Bangladeshi",
  "L" = "Any other Asian background",
  "M" = "Caribbean",
  "N" = "African",
  "P" = "Any other Black background",
  "R" = "Chinese",
  "S" = "Any other ethnic group" ,
  "Z" = "Not stated",
  "X" = "Not known"
)

sim_av_patient$ETHNICITY_factor %<>% forcats::fct_explicit_na()

ethnicity_data <- sim_av_patient %>%
  mutate(british = case_when(ETHNICITY_factor == "(White) British" ~ 1,
                             TRUE ~ 0)) %>%
  filter(british == 0) %>% 
  mutate(known_ethnicity = case_when(
    ETHNICITY_factor %in% c('(Missing)', 'Not stated', 
                            'Not known', '0', '8', 'CA') ~ 'Unknown ethnicity',
    TRUE ~ 'Known ethnicity'
  )) 

mycolors <- c("#002664", "#D7153A")


(column_gph <- ethnicity_data %>% 
  count(known_ethnicity, sort = TRUE) %>%
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(x = known_ethnicity, y = prop, fill=factor(known_ethnicity))) +
  geom_col() +
    geom_text(aes(label = round(prop*100,1)), color = "white", size = 10, position = position_stack(vjust = 0.5)) + 
    scale_fill_manual(values=mycolors) +
  theme_bw() +
    scale_y_continuous(labels = scales::percent, breaks=seq(from = 0, to = 1, by = .1)) +
    scale_x_discrete( expand = c(0, 0)) +
    theme(legend.position='none',
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.grid = element_blank(),
      panel.spacing = unit(2, "lines"),
      text = element_text(size = 12)
    ) +
  labs(x='', y='', title='Most records have unknown ethnicity data') 
)


(point_gph <- ethnicity_data %>%
  count(known_ethnicity,ETHNICITY_factor, sort = TRUE) %>%
  mutate(prop = n / sum(n)) %>% 
  mutate(ETHNICITY_factor = fct_reorder(ETHNICITY_factor, (n))) %>%
    filter(known_ethnicity=='Known ethnicity') %>% 
  ggplot(aes(x = ETHNICITY_factor, y = prop, group=1)) +
  geom_point(size=8, aes(col=known_ethnicity)) +
    scale_color_manual(values=mycolors) +
  labs(x='', y='', title = 'The known ethnicity group is made up of 15 ethnicity classifications') +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
    theme(legend.position='none',
      axis.line = element_blank(),
      #axis.ticks = element_blank(),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "#CBEDFD", colour = "black", size = 1),
      panel.grid = element_blank(),
      panel.spacing = unit(2, "lines"),
      text = element_text(size = 12)
    ) +
  facet_wrap(~ (known_ethnicity), scales='free_y', ncol=1)
)



(missingness_overtime <- ethnicity_data %>% 
  mutate(year=lubridate::year(VITALSTATUSDATE)) %>% 
  filter(!is.na(year)) %>% 
  count(year, known_ethnicity) %>% group_by(year) %>% 
  mutate(missingness=n/sum(n)) %>% 
    filter(known_ethnicity=='Unknown ethnicity') %>% 
    ggplot(aes(x=year, y=missingness)) +
    geom_line(color='#D7153A',size=2) +
    theme_bw() +
    labs(x='Year', title='The % of records with unknown ethnicity data has increased since 2013', y='') +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks=c(2013:2019)) +
    theme(
      axis.line = element_blank(),
      #axis.ticks = element_blank(),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "#CBEDFD", colour = "black", size = 1),
      panel.grid = element_blank(),
      panel.spacing = unit(2, "lines"),
      text = element_text(size = 12)
    )
)
  
library(patchwork)


patchwork <- (column_gph/missingness_overtime) | point_gph

patchwork + plot_annotation(
  title = 'Investigating the completeness of ethnicity data in a simulated cancer registry',
  caption = 'Data source: Simulacrum',
  theme = theme(plot.title = element_text(size = 18))) 


(age_ethnicity_gph <- ethnicity_data %>% select(known_ethnicity, PATIENTID) %>% 
    left_join(sim_av_tumour %>% select(PATIENTID,AGE), by='PATIENTID') %>% 
  count(known_ethnicity,AGE) %>% group_by(known_ethnicity) %>% mutate(prop=n/sum(n)) %>% ungroup() %>% 
  ggplot() +
  geom_line(aes(x=AGE, y=prop, color=known_ethnicity),alpha = .8, size=1.5) +
    theme_bw() +
  scale_color_manual(values=mycolors) +
    scale_y_continuous(labels = scales::percent) +
    labs(x='Age', title='Unknown ethnicity is more common for those aged 25 to 40', y='', color='') +
    theme(legend.position = c(0.9, 0.8),legend.key.size = unit(2, 'cm'),legend.text = element_text(size=30),
      axis.line = element_blank(),
      #axis.ticks = element_blank(),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "#CBEDFD", colour = "black", size = 1),
      panel.grid = element_blank(),
      panel.spacing = unit(2, "lines"),
      text = element_text(size = 12))
)

patchwork2 <- (column_gph + point_gph) / (missingness_overtime + age_ethnicity_gph)

patchwork2 + plot_annotation(
  title = 'Investigating the completeness of ethnicity data in a simulated cancer registry',
  caption = 'Data source: Simulacrum',
  theme = theme(plot.title = element_text(size = 18))) 

pd = position_dodge(0.5)

age_ethnicity_data <- ethnicity_data %>% select(known_ethnicity, PATIENTID) %>% 
  left_join(sim_av_tumour %>% select(PATIENTID,AGE), by='PATIENTID') %>% 
  mutate(age_group=cut(AGE, breaks=c(-1,19,39,59,79,Inf), labels=c('0-19', '20-39', '40-59', '60-79', '80+'))) %>% 
  count(known_ethnicity,age_group) %>% 
  group_by(age_group) %>% mutate(sum=sum(n)) %>%   
  mutate(prop = map2(n, sum, ~ prop.test(.x, .y, conf.level=0.95) %>% 
                       broom::tidy())) %>%
  unnest(prop) %>% 
  ungroup()


(age_ethnicity_gph <- 
    ggplot(data=age_ethnicity_data,aes(x=age_group, y=estimate)) +
    geom_col(aes(fill=factor(known_ethnicity)), position = position_dodge2())+
    #geom_errorbar(aes(col=factor(known_ethnicity),ymin=conf.low, ymax=conf.high), width=.1, position = position_dodge2(.9, padding = .6), size=2) +
    theme_bw() +
    scale_fill_manual(values=mycolors) +
    #scale_y_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent, breaks=seq(from = 0, to = 1, by = .1)) +
    
    labs(x='Age group', title='Completeness of ethnicity data varies by age group', y='', fill='') +
    theme(legend.position = c(0.9, 0.95),legend.key.size = unit(1, 'cm'),legend.text = element_text(size=16),
          axis.line = element_blank(),
          #axis.ticks = element_blank(),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "#CBEDFD", colour = "black", size = 1),
          panel.grid = element_blank(),
          panel.spacing = unit(2, "lines"),
          text = element_text(size = 12))
)

patchwork3 <- (column_gph + point_gph) / (missingness_overtime + age_ethnicity_gph)

patchwork3 + plot_annotation(
  title = 'Investigating the completeness of ethnicity data in a simulated cancer registry',
  caption = 'Data source: Simulacrum',
  theme = theme(plot.title = element_text(size = 18))) 
