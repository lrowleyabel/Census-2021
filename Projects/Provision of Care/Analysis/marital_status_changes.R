library(dplyr)
library(sociodemographics)

rm(list = ls())


# Read in 2021 martal, age, sex data
df_marital_age_sex<- read.csv("../../../Data/2021/EW Data/EW_marital_status_by_age_group_by_sex_2021_data.csv")

df_marital_age_sex<- df_marital_age_sex%>%
  rename(age = Age..18.categories.,
         sex = Sex..2.categories.,
         marital = Marital.and.civil.partnership.status..6.categories.,
         n = Observation)%>%
  select(age, sex, marital, n)


# Download 2011 marital, age, sex data from Nomis API
id<- nomis_search(name = "*DC1107EW*")$id[1]

nomis_codelist(id, "C_MARSTAT")
nomis_get_metadata(id, )

df_marital_age_sex_11<- nomis_get_data(id = id, geography = "TYPE499", C_SEX = 1:2, C_AGE = 1:16, C_MARSTAT = 1:6, tidy = T)%>%
  filter(geography_name == "England and Wales")%>%
  rename(age = c_age_name,
         sex = c_sex_name,
         marital = c_marstat_name,
         n = obs_value)%>%
  select(age, sex, marital, n)


# Calculate proportions for each marital group within each age-sex group
df_marital_age_sex<- df_marital_age_sex%>%
  group_by(age, sex)%>%
  mutate(total = sum(n, na.rm = T))%>%
  ungroup()

df_marital_age_sex<- df_marital_age_sex%>%
  mutate(proportion = 100*n/total)

df_marital_age_sex_11<- df_marital_age_sex_11%>%
  group_by(age, sex)%>%
  mutate(total = sum(n, na.rm = T))%>%
  ungroup()

df_marital_age_sex_11<- df_marital_age_sex_11%>%
  mutate(proportion = 100*n/total)