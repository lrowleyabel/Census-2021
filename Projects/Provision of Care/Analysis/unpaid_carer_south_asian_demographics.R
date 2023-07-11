###########################################
#                                         #
#  UNPAID CARER SOUTH ASIAN DEMOGRAPHICS  #
#                                         #
###########################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 07/07/2023

# DESCRIPTION: This file analyses the demographics of unpaid carers of South Asian (Bangladeshi, Pakistani and Indian) ethnicity.

library(dplyr)
library(tidyr)
library(ggplot2)
library(sociodemographics)
library(nomisr)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis")
rm(list = ls())


df_ethnicity_age_sex_care<- read.csv("../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_ethnicity_by_sex_by_age_group_2021_data.csv")


df_ethnicity_age_sex_care<- df_ethnicity_age_sex_care%>%
  rename(age = Age..18.categories.,
         sex = Sex..2.categories.,
         ethnicity = Ethnic.group..20.categories.,
         care = Unpaid.care..5.categories.,
         n = Observation)%>%
  select(age, sex, ethnicity, care, n)

total_population_ethnicity_age_sex<- read.csv("../../../Data/2021/EW Data/EW_ethnicity_by_sex_by_age_group_2021_data.csv")

total_population_ethnicity_age_sex<- total_population_ethnicity_age_sex%>%
  rename(age = Age..18.categories.,
         sex = Sex..2.categories.,
         ethnicity = Ethnic.group..20.categories.,
         total = Observation)%>%
  select(age, sex, ethnicity, total)

df_ethnicity_age_sex_care<- left_join(df_ethnicity_age_sex_care, total_population_ethnicity_age_sex, by = c("ethnicity", "age", "sex"))


df_ethnicity_age_sex_care<- df_ethnicity_age_sex_care%>%
  mutate(prop = 100*n/total)

df_ethnicity_age_sex_care<- df_ethnicity_age_sex_care%>%
  mutate(ethnicity = str_extract(ethnicity, "(?<=: ).*"))%>%
  mutate(ethnicity = ifelse(substr(ethnicity, 1, 3) == "Eng", "White British", ethnicity))



df_ethnicity_age_sex_care%>%
  mutate(age = str_remove(age, "Aged "))%>%
  filter(ethnicity %in% c("African", "Caribbean", "Chinese", "Gypsy or Irish Traveller", "Pakistani", "Bangladeshi", "Indian", "Other Asian", "White British"))%>%
  filter(!care %in% c("Does not apply", "Provides no unpaid care"))%>%
  group_by(ethnicity, age, sex)%>%
  summarise(prop = sum(prop), n = sum(n))%>%
  ungroup()%>%
  population_pyramid(count = prop, age = age, side = sex, left = "Male", linewidth = 0.5)+
  facet_wrap(~ethnicity)+
  theme(plot.background = element_rect(fill = "white"),
        plot.title.position = "plot")+
  labs(x = "Proportion providing unpaid care (%)", title = "Proportion providing unpaid care by age, sex and ethnicity")


ggsave(filename= "Ethnicity age sex.png", units = "in", height = 6, width = 9, dpi = 1000)


df_ethnicity_age_yoa_care<- read.csv("../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_ethnicity_by_year_of_arrival_in_the_UK_by_age_group_2021_data.csv")

df_ethnicity_age_yoa_care<- df_ethnicity_age_yoa_care%>%
  rename(age = Age..6.categories.,
         yoa = Year.of.arrival.in.the.UK..13.categories.,
         ethnicity = Ethnic.group..20.categories.,
         care = Unpaid.care..5.categories.,
         n = Observation)%>%
  select(age, yoa, ethnicity, care, n)

df_ethnicity_yoa_care<- df_ethnicity_age_yoa_care%>%
  group_by(ethnicity, yoa)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

total_population_ethnicity_age_sex_yoa<- read.csv("../../../Data/2021/EW Data/EW_ethnicity_by_year_of_arrival_in_UK_by_age_group_by_sex_2021_data.csv")

total_population_ethnicity_age_sex_yoa<- total_population_ethnicity_age_sex_yoa%>%
  rename(age = Age..18.categories.,
         yoa = Year.of.arrival.in.the.UK..13.categories.,
         ethnicity = Ethnic.group..20.categories.,
         sex = Sex..2.categories.,
         total = Observation)%>%
  select(age, yoa, ethnicity, sex, total)

total_population_ethnicity_age_sex_yoa<- total_population_ethnicity_age_sex_yoa%>%
  mutate(age = collapse_age_groups(age, unique(df_ethnicity_age_yoa_care$age)))%>%
  group_by(age, yoa, ethnicity, sex)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()

total_population_ethnicity_age_sex_yoa<- total_population_ethnicity_age_sex_yoa%>%
  mutate(ethnicity = str_extract(ethnicity, "(?<=: ).*"))


df_ethnicity_yoa_care<- df_ethnicity_age_yoa_care%>%
  mutate(ethnicity = str_extract(ethnicity, "(?<=: ).*"))


total_population_ethnicity_age_sex_yoa%>%
  filter(ethnicity %in% c("Pakistani", "Bangladeshi", "Indian"))%>%
  ggplot()+
    geom_col(aes(x = total, y = yoa, fill = sex))+
  facet_wrap(~ethnicity)

total_population_ethnicity_age_sex_yoa<- total_population_ethnicity_age_sex_yoa%>%
  group_by(ethnicity, age, yoa)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()


df_ethnicity_yoa_care<- df_ethnicity_age_yoa_care%>%
  mutate(care = case_when(care %in% c("Does not apply", "Provides no unpaid care") ~ "Provides no unpaid care",
                          T ~ "Provides unpaid care"))

df_ethnicity_yoa_care<- left_join(df_ethnicity_age_yoa_care, total_population_ethnicity_age_sex_yoa, by = c("age", "ethnicity", "yoa"))

df_ethnicity_age_yoa_care<- df_ethnicity_age_yoa_care%>%
  mutate(prop = 100*n/total)

df_ethnicity_age_yoa_care%>%
  filter(care == "Provides unpaid care")%>%
  filter(ethnicity == "Pakistani")%>%
  ggplot()+
    geom_col(aes(x = prop, y = yoa))
