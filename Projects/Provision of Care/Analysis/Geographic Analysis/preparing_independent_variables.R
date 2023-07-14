######################################
#                                    #
#  PREPARING INDEPENDENT VARIABLES   #
#                                    #
######################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 13/07/2023

# DESCRIPTION: This file prepares the relevant independent variables to be used when modelling the rate change across LSOAs.


library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(sociodemographics)
library(nomisr)
library(sf)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis/Geographic Analysis")
rm(list = ls())


# Read in LSOA dataset
load("Rates Data/2021 LSOAs with Rates.Rda")


#### STEP 1: IMD ####


# Load in LSOA IMD
imd<- read.csv("../../../../Data/2019/LSOA Data/lsoa_IMD_and_domains_2019_data.csv")

imd<- imd%>%
  rename(lsoa11cd = LSOA.code..2011.)%>%
  select(lsoa11cd, contains("Rank"))%>%
  rename_with(.fn = ~str_extract(.x, ".*Rank")%>%
                str_replace_all("\\.\\.?", "_")%>%
                str_to_lower(), .cols = contains("Rank"))%>%
  rename(imd_rank = index_of_multiple_deprivation_imd_rank)

# Read in lSOA11 to LSOA21 lookup
lookup<- read.csv("../../../../Lookups/Best-Fit LSOA11 to LSOA21 to LAD22 EW.csv")%>%
  rename(lsoa11cd = LSOA11CD,
         lsoa21cd = LSOA21CD)%>%
  select(lsoa11cd, lsoa21cd)

# Remove 2021 LSOAs which are matched to more than one 2011 LSOA
duplicated_lsoa21s<- lookup$lsoa21cd[duplicated(lookup$lsoa21cd)]

lookup<- lookup%>%
  filter(!lsoa21cd %in% duplicated_lsoa21s)

# Join lookup to IMD data
imd<- left_join(imd, lookup, by = "lsoa11cd")

# Remove lsoa11cd variable from IMD data
imd<- imd%>%
  select(-lsoa11cd)

# Join IMD data to rates data by lsoa21
shp<- left_join(shp, imd, by = c("LSOA21CD" = "lsoa21cd"))

# Convert IMD variables to numeric
shp<- shp%>%
  mutate(across(contains("rank"), ~str_remove_all(.x, ",")%>%
                  as.numeric()))


#### Step 2: Living Arrangements ####


# Download living arranegement by age data for 2011 from NOMIS API
id<- nomis_search("*LC1108EW*")$id[1]

df_living_age_11<- nomis_get_data(id, geography = "TYPE298", C_LARPUK11 = c(0,4), C_AGE = c(1:5), C_SEX = 0, tidy = T)

# Select and rename relevant variables
df_living_age_11<- df_living_age_11%>%
  rename(lsoa11cd = geography_code,
         living = c_larpuk_11_name,
         age = c_age_name,
         n = obs_value)%>%
  select(lsoa11cd, living, age, n)

# Pivot to wider so total population is a separate column
df_living_age_11<- df_living_age_11%>%
  pivot_wider(id_cols = c(lsoa11cd, age), names_from = living, values_from = n)

df_living_age_11<- df_living_age_11%>%
  rename(n = `Not living in a couple: Total`,
         total = `All categories: Living arrangements`)

# Calculate proportion not living in a couple overall in each LSOA
df_living_11<- df_living_age_11%>%
  group_by(lsoa11cd)%>%
  summarise(n = sum(n, na.rm = T),
            total = sum(total, na.rm = T))%>%
  ungroup()%>%
  mutate(proportion_11 = 100*n/total)

# Join the lsoa lookup to the 2011 data
df_living_age_11<- left_join(df_living_age_11, lookup, by = "lsoa11cd")%>%
  select(-lsoa11cd)

df_living_11<- left_join(df_living_11, lookup, by = "lsoa11cd")%>%
  select(-lsoa11cd)


# Separate out age group total population sizes
total_age_population_11<- df_living_age_11%>%
  select(lsoa21cd, age, total)

df_living_age_11<- df_living_age_11%>%
  select(-total)

# Read in 2021 living arrangements by LSOA data
living<- read.csv("../../../../Data/2021/LSOA Data/lsoa_living_arrangements_2021_data.csv")

# Select and rename relevant columns
living<- living%>%
  rename(lsoa21cd = Lower.layer.Super.Output.Areas.Code,
         living = Living.arrangements..5.categories.,
         n = Observation)%>%
  select(lsoa21cd, living, n)

# Calculate proportion in each living arrangement
living<- living%>%
  group_by(lsoa21cd)%>%
  mutate(total = sum(n, na.rm = T))%>%
  ungroup()%>%
  mutate(proportion = 100*n/total)

# Filter to just "not living in a couple"
living<- living%>%
  filter(living == "Not living in a couple")

# Join the 2011 raw proportions so we can calculate the raw change
living<- left_join(living, df_living_11%>%
                     select(lsoa21cd, proportion_11), by = "lsoa21cd")

living<- living%>%
  mutate(non_couple_raw_change = proportion - proportion_11)

# Read in the 2021 care by age data so that we can use the age group populations
total_age_population_21<- read.csv("../../../../Data/2021/LSOA Data/lsoa_provision_of_unpaid_care_by_age_group_2021_data.csv")
  
total_age_population_21<- total_age_population_21%>%
  rename(lsoa21cd = Lower.layer.Super.Output.Areas.Code,
         age = Age..6.categories.,
         total = Observation)%>%
  select(lsoa21cd, age, total)%>%
  group_by(lsoa21cd, age)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()


# Convert the 2021 age groups to the same as the 2011 age groups
total_age_population_21<- total_age_population_21%>%
  filter(age != "Aged 15 years and under")%>%
  mutate(age = collapse_age_groups(age, unique(total_age_population_11$age)))

living_rates<- age_standardise_indirect(interest_counts = living,
                                        interest_age_specific_population = total_age_population_21,
                                        standard_age_specific_counts = df_living_age_11,
                                        standard_age_specific_population = total_age_population_11,
                                        count_variable = n,
                                        population_variable = total,
                                        age_variable = age,
                                        grouping_variable = lsoa21cd,
                                        single_standard_population = F)

# Join living arrangement data to main df
shp<- left_join(shp, living%>%
                  select(lsoa21cd, non_couple_raw_change), by = c("LSOA21CD" = "lsoa21cd"))

shp<- left_join(shp, living_rates%>%
                  rename(non_couple_rate = age_standardised_rate)%>%
                  select(lsoa21cd, non_couple_rate), by = c("LSOA21CD" = "lsoa21cd"))

# Convert infinite non-couple rates to NA
shp$non_couple_rate[is.infinite(shp$non_couple_rate)]<- NA



#### Step 3: Excess Deaths ####


# Read in 2011 to 2021 deaths by sex and age for each LSOA
deaths<- read.csv("../../../../Data/2011/LSOA Data/lsoa_deaths_by_age_by_sex_2011_to_2021_data.csv")

# Sum across age and sex groups
deaths<- deaths%>%
  group_by(LSOA11, REGYR)%>%
  summarise(Deaths = sum(Deaths, na.rm = T))%>%
  ungroup()

# Extract 2020 deaths
deaths_20<- deaths%>%
  filter(REGYR == 2020)

# Calculate 2015 - 2019 mean deaths for each LSOA
deaths_avg<- deaths%>%
  filter(REGYR %in% 2015:2019)%>%
  group_by(LSOA11)%>%
  summarise(avg_deaths = mean(Deaths, na.rm = T))%>%
  ungroup()

# Join five-year average to 2020 deaths
deaths_20<- left_join(deaths_20, deaths_avg, by = "LSOA11")

# Calculate excess deaths for 2020
deaths_20<- deaths_20%>%
  mutate(excess_deaths_20 = Deaths - avg_deaths)

# Plot distribution of excess deaths for 2020
ggplot(deaths_20)+
  geom_histogram(aes(x = excess_deaths_20))

mean(deaths_20$excess_deaths_20, na.rm = T)

# Read in 2019 mid-year population estimates for each LSOA
population_19<- read.csv("../../../../Data/2011/LSOA Data/lsoa_mid_year_population_estimates_2019_data.csv")

# Convert population count to numeric
population_19<- population_19%>%
  rename(lsoa11cd = LSOA.Code,
         population = All.Ages)%>%
  mutate(population = str_remove_all(population, ",")%>%
           as.numeric())

# Join 2019 population to deaths data
deaths_20<- left_join(deaths_20, population_19, by = c("LSOA11" = "lsoa11cd"))

# Calculate excess deaths per 1000 people
deaths_20<- deaths_20%>%
  mutate(excess_death_rate = 1000*excess_deaths_20/population)

# Join 2021 LSOAs to 2011 LSOA deaths data
deaths_20<- left_join(deaths_20, lookup, by = c("LSOA11" = "lsoa11cd"))%>%
  select(-LSOA11)

# Join deaths data to main df
shp<- left_join(shp, deaths_20%>%
                  select(lsoa21cd, excess_deaths_20, excess_death_rate), by = c("LSOA21CD" = "lsoa21cd"))



#### Step 4: Rural-Urban Classification ####


# Read in RUC data
ruc<- read.csv("../../../../Data/2011/LSOA Data/lsoa_rural_urban_classification_2011_data.csv")

# Join LSOA21s to RUC data
ruc<- left_join(ruc, lookup, by = c("LSOA11CD" = "lsoa11cd"))%>%
  select(-LSOA11CD)

# Join RUC data to main data
shp<- left_join(shp, ruc, by = c("LSOA21CD" = "lsoa21cd"))

####

shp%>%
  st_drop_geometry()%>%
  group_by(RUC11CD)%>%
  summarise(mean = mean(age_standardised_rate, na.rm =T))

table(shp$age_standardised_rate>=1, substr(shp$RUC11CD,1,1))%>%
  prop.table(2)

ggplot(shp)+
  geom_point(aes(x = excess_death_rate, y = age_standardised_rate))

cor.test(shp$change, shp$excess_death_rate)

lm(age_standardised_rate ~ excess_death_rate + imd_rank + non_couple_rate, data = shp)%>%
  summary()

sum(deaths_20$Deaths)
1.01
0.0004


library(spdep)
library(spatialreg)

nb<- poly2nb(shp, queen = T)
lw<- nb2listw(nb, zero.policy = T)

m1<- spatialreg::lmSLX(age_standardised_rate ~ excess_death_rate + imd_rank + non_couple_raw_change + substr(RUC11CD, 1, 1), data = shp, listw = lw, zero.policy = T)

summary(m1)

shp%>%
  st_drop_geometry()%>%
  select(where(is.numeric))%>%
  cor(use = "com")%>%
View()

m2<- errorsarlm(age_standardised_rate ~ excess_death_rate + non_couple_rate + substr(RUC11CD, 1, 1), data = shp, listw = lw, zero.policy = T, method = "LU")
summary(m2)

ggplot(shp)+
  geom_sf(aes(fill = age_standardised_rate>=1), color = NA)
