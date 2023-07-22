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


# # Download living arranegement by age data for 2011 from NOMIS API
# id<- nomis_search("*LC1108EW*")$id[1]
# 
# df_living_age_11<- nomis_get_data(id, geography = "TYPE298", C_LARPUK11 = c(0,4), C_AGE = c(1:5), C_SEX = 0, tidy = T)
# 
# # Select and rename relevant variables
# df_living_age_11<- df_living_age_11%>%
#   rename(lsoa11cd = geography_code,
#          living = c_larpuk_11_name,
#          age = c_age_name,
#          n = obs_value)%>%
#   select(lsoa11cd, living, age, n)
# 

# Read in living arrangement by age data for 2011
df_living_age_11<- read.csv("../../../../Data/2011/LSOA Data/lsoa_not_living_in_a_couple_by_age_2011_data.csv")

# Pivot age columns to long format
df_living_age_11<- df_living_age_11%>%
  pivot_longer(cols = Age.16.to.24:Age.65.and.over, names_to = "age", values_to = "n")

# Rename and select relevant variables
df_living_age_11<- df_living_age_11%>%
  rename(lsoa11cd = mnemonic)%>%
  select(lsoa11cd, living, age, n)

# Pivot to wider so total population is a separate column
df_living_age_11<- df_living_age_11%>%
  pivot_wider(id_cols = c(lsoa11cd, age), names_from = living, values_from = n)

df_living_age_11<- df_living_age_11%>%
  rename(n = `Not living in a couple`,
         total = `All categories`)

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

# Fill in lsoa/year/age/sex combinations that are missing due to there being zero deaths registered
deaths<- deaths%>%
  mutate(across(c(LSOA11, REGYR, SEX, AGEGROUP), as.factor))

deaths<- deaths%>%
  complete(LSOA11, REGYR, SEX, AGEGROUP, fill = list(Deaths = 0))


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

# Read in 2020 mid-year population estimates for each LSOA
population_20<- read.csv("../../../../Data/2011/LSOA Data/lsoa_mid_year_population_estimates_2020_data.csv")

# Convert population count to numeric
population_20<- population_20%>%
  rename(lsoa11cd = LSOA.Code,
         population = All.Ages)%>%
  select(lsoa11cd, population)%>%
  mutate(population = str_remove_all(population, ",")%>%
           as.numeric())

# Join 2020 population to deaths data
deaths_20<- left_join(deaths_20, population_20, by = c("LSOA11" = "lsoa11cd"))

# Calculate excess deaths per 1000 people
deaths_20<- deaths_20%>%
  mutate(excess_death_rate = 1000*excess_deaths_20/population)

# Join 2021 LSOAs to 2011 LSOA deaths data
deaths_20<- left_join(deaths_20, lookup, by = c("LSOA11" = "lsoa11cd"))%>%
  select(-LSOA11)

# Join deaths data to main df
shp<- left_join(shp, deaths_20%>%
                  select(lsoa21cd, excess_deaths_20, excess_death_rate), by = c("LSOA21CD" = "lsoa21cd"))


#### Step 4: 2020 Age-Standardised Mortality Rate ####


# Read in 2001 to 2010 deaths by sex and age for each LSOA
deaths01<- read.csv("../../../../Data/2011/LSOA Data/lsoa_deaths_by_age_by_sex_2001_to_2010_data.csv")

# Read in 2011 to 2021 deaths by sex and age for each LSOA
deaths11<- read.csv("../../../../Data/2011/LSOA Data/lsoa_deaths_by_age_by_sex_2011_to_2021_data.csv")

# Bind the two sets of data
deaths11<- deaths11%>%
  rename(DEATHS = Deaths)

deaths01_21<- rbind(deaths01, deaths11)

# Rename relevant variables
deaths01_21<- deaths01_21%>%
  rename(lsoa11cd = LSOA11,
         year = REGYR,
         age = AGEGROUP,
         sex = SEX,
         n = DEATHS)


# Complete missing combinations where there were zero deaths
deaths01_21<- deaths01_21%>%
  complete(lsoa11cd, year, age, sex, fill = list(n = 0))

# Sum across sex
deaths01_21<- deaths01_21%>%
  mutate(age = as.factor(age))%>%
  group_by(lsoa11cd, year, age, .drop = F)%>%
  summarise(n = sum(n, na.rm = T))


# Create a dataframe of 2007 to 2011 deaths by age by LSOA
deaths07_11<- deaths01_21%>%
  filter(year %in% 2007:2011)

rm("deaths01", "deaths11")

# Read in population estimates by age for each LSOA in 2006 to 2011
total_population_age_06_11<- read.csv("../../../../Data/2011/LSOA Data/lsoa_mid_year_population_estimates_by_age_2006_to_2011_data.csv")

# Pivot to long format
total_population_age_06_11<- total_population_age_06_11%>%
  select(-all_ages)%>%
  pivot_longer(cols = p0:p90plus, names_to = "age", values_to = "total")

# Collapse age groups to the same as the deaths data
deaths07_11$age<- deaths07_11$age%>%
  forcats::fct_relabel(~str_replace(.x, "\\<1", "0")%>%
                         str_replace("95\\+", "90 and over")%>%
                         str_replace("90-94", "90 and over"))

deaths07_11<- deaths07_11%>%
  group_by(lsoa11cd, year, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()


collapsed_age_groups<- paste(seq(0,60,15), "to", seq(14,74,15))

collapsed_age_groups[5]<- "60 and over"

age_conversion<- deaths07_11%>%
  select(age)%>%
  unique()%>%
  mutate(new_age = collapse_age_groups(age, collapsed_age_groups))

deaths07_11<- left_join(deaths07_11, age_conversion, by = "age")%>%
  select(-age)%>%
  rename(age = new_age)

deaths07_11<- deaths07_11%>%
  group_by(lsoa11cd, year, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()


age_conversion<- total_population_age_06_11%>%
  select(age)%>%
  unique()%>%
  mutate(new_age = collapse_age_groups(age, unique(deaths07_11$age)))

total_population_age_06_11<- left_join(total_population_age_06_11, age_conversion, by = "age")%>%
  select(-age)%>%
  rename(age = new_age)

total_population_age_06_11<- total_population_age_06_11%>%
  group_by(LSOA11CD, Year, age)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()

# Calculate age-specific mortality rates for each year 2007 to 2011 by LSOA
deaths07_11<- left_join(deaths07_11, total_population_age_06_11%>%
                          rename(lsoa11cd = LSOA11CD,
                                 year = Year)%>%
                          select(lsoa11cd, year, age, total), by = c("lsoa11cd", "year", "age"))

deaths07_11<- deaths07_11%>%
  mutate(mortality_rate = n/total)

# Look at infinite mortality rate values
deaths07_11%>%
  filter(is.infinite(mortality_rate))

## Replace infinite mortality rates with a rate of 1, since these are cases where the mid-year population estimate is zero because the only people in an age-group died prior to 30th June
#deaths07_11$mortality_rate[is.infinite(deaths07_11$mortality_rate)]<- 1


# See how many years we have deaths data for for each LSOA
deaths07_11%>%
  select(lsoa11cd, year)%>%
  unique()%>%
  group_by(lsoa11cd)%>%
  summarise(n = n())%>%
  with(table(n))

# Calculate average age-specific mortality rates for each LSOA across the 2007 to 2011 period
deaths_avg_07_11<- deaths07_11%>%
  group_by(lsoa11cd, age)%>%
  summarise(avg_mortality_rate_07_11 = mean(mortality_rate, na.rm = T))%>%
  ungroup()


# Read in 2020 population by age for each LSOA
total_age_population_20<- read.csv("../../../../Data/2011/LSOA Data/lsoa_mid_year_population_estimates_2020_data.csv")

# Pivot to long format
total_age_population_20<- total_age_population_20%>%
  select(-All.Ages)%>%
  mutate(across(X0:X90., ~str_remove_all(.x, ",")%>%
                  as.numeric()))%>%
  pivot_longer(cols = X0:X90., names_to = "age", values_to = "total")

# Change "90." to "90 plus"
total_age_population_20$age[total_age_population_20$age=="X90."]<- "X90_plus"

# Collapse age groups to the same as the 2007 - 2011 data
age_conversion<- total_age_population_20%>%
  select(age)%>%
  unique()%>%
  mutate(new_age = collapse_age_groups(age, unique(deaths07_11$age)))

total_age_population_20<- left_join(total_age_population_20, age_conversion, by = "age")%>%
  select(-age)%>%
  rename(age = new_age)

total_age_population_20<- total_age_population_20%>%
  group_by(LSOA.Code, age)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()

# Calculate expected deaths in 2020 based on 2007 - 2011 rates
expected_deaths_20<- left_join(total_age_population_20%>%
                                 rename(lsoa11cd = LSOA.Code), deaths_avg_07_11, by = c("lsoa11cd", "age"))

expected_deaths_20<- expected_deaths_20%>%
  mutate(expected_deaths = total*avg_mortality_rate_07_11)


# Sum expected deaths across ages
expected_deaths_20<- expected_deaths_20%>%
  group_by(lsoa11cd)%>%
  summarise(expected_deaths = sum(expected_deaths, na.rm = T))%>%
  ungroup()

# Extract actual deaths for 2020
deaths20<- deaths%>%
  filter(REGYR == 2020)

# Join expected to actual deaths for 2020
deaths20<- left_join(deaths20, expected_deaths_20, by = c("LSOA11" = "lsoa11cd"))

# Calculate age-standardsed mortality rate for 2020
deaths20<- deaths20%>%
  mutate(ASMR_20 = Deaths/expected_deaths)

# Plot distribution of age-standardised rate
deaths20%>%
  ggplot()+
  geom_histogram(aes(x = ASMR_20), bins = 100)+
  geom_vline(xintercept = 1, linetype = 2, color = "red")

deaths20%>%
  filter(is.finite(ASMR_20))%>%
  with(mean(ASMR_20))


# Join 2021 LSOAs to 2020 deaths data
deaths20<- left_join(deaths20, lookup, by = c("LSOA11" = "lsoa11cd"))%>%
  select(-LSOA11)

# Join 2020 deaths data to main df
shp<- left_join(shp, deaths20%>%
                  select(lsoa21cd, ASMR_20), by = c("LSOA21CD" = "lsoa21cd"))

# Set non-finite 2020 death rates as NA
shp$ASMR_20[is.infinite(shp$ASMR_20)]<- NA


#### Step 5: 2018 Age-Standardised Mortality Rates ####



# Read in 2018 population by age for each LSOA
total_age_population_18<- read.csv("../../../../Data/2011/LSOA Data/lsoa_mid_year_population_estimates_by_age_2018_data.csv")

# Pivot to long format
total_age_population_18<- total_age_population_18%>%
  select(-All.Ages)%>%
  mutate(across(X0:X90., ~str_remove_all(.x, ",")%>%
                  as.numeric()))%>%
  pivot_longer(cols = X0:X90., names_to = "age", values_to = "total")

# Change "90." to "90 plus"
total_age_population_18$age[total_age_population_18$age=="X90."]<- "X90_plus"

# Collapse age groups to same as deaths data
age_conversion<- total_age_population_18%>%
  select(age)%>%
  unique()%>%
  mutate(new_age = collapse_age_groups(age, unique(deaths_avg_07_11$age)))

total_age_population_18<- left_join(total_age_population_18, age_conversion, by = "age")%>%
  select(-age)%>%
  rename(age = new_age)

total_age_population_18<- total_age_population_18%>%
  group_by(Area.Codes, age)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()

# Calculate expected deaths for 2018 based on 2007 - 2011 rates
expected_deaths_age_18<- left_join(total_age_population_18, deaths_avg_07_11, by = c("Area.Codes" = "lsoa11cd", "age" = "age"))

expected_deaths_age_18<- expected_deaths_age_18%>%
  mutate(expected_deaths_18 = total*avg_mortality_rate_07_11)

# Sum expected deaths across ages
expected_deaths_18<- expected_deaths_age_18%>%
  group_by(Area.Codes)%>%
  summarise(expected_deaths_18 = sum(expected_deaths_18, na.rm = T))

# Extract deaths for 2018
deaths18<- deaths%>%
  filter(REGYR == 2018)

# Join expected to actual deaths for 2018
deaths18<- left_join(deaths18, expected_deaths_18, by = c("LSOA11" = "Area.Codes"))

# Calculate age-standardsed mortality rate for 2018
deaths18<- deaths18%>%
  mutate(ASMR_18 = Deaths/expected_deaths_18)

# Plot distribution of age-standardised rate
deaths18%>%
  ggplot()+
  geom_histogram(aes(x = ASMR_18), bins = 100)+
  geom_vline(xintercept = 1, linetype = 2, color = "red")

deaths18%>%
  filter(is.finite(ASMR_18))%>%
  with(mean(ASMR_18))

# Join 2021 LSOAs to 2018 deaths data
deaths18<- left_join(deaths18, lookup, by = c("LSOA11" = "lsoa11cd"))%>%
  select(-LSOA11)

# Join 2018 deaths data to main df
shp<- left_join(shp, deaths18%>%
                  select(lsoa21cd, ASMR_18), by = c("LSOA21CD" = "lsoa21cd"))

# Set non-finite 2020 death rates as NA
shp$ASMR_18[is.infinite(shp$ASMR_18)]<- NA



#### Step 6: Rural-Urban Classification ####


# Read in RUC data
ruc<- read.csv("../../../../Data/2011/LSOA Data/lsoa_rural_urban_classification_2011_data.csv")


# Recode RUC categories to just the letters
ruc<- ruc%>%
  mutate(RUC11CD = substr(RUC11CD, 1, 1))

# Join LSOA21s to RUC data
ruc<- left_join(ruc, lookup, by = c("LSOA11CD" = "lsoa11cd"))%>%
  select(-LSOA11CD)

# Join RUC data to main data
shp<- left_join(shp, ruc, by = c("LSOA21CD" = "lsoa21cd"))


#### Step 7: 2021 Age Profile ####


# Read in age data
age<- read.csv("../../../../Data/2021/LSOA Data/lsoa_age_group_2021_data.csv")

# Select and rename variables
age<- age%>%
  rename(lsoa21cd = Lower.layer.Super.Output.Areas.Code,
         age = Age..6.categories.,
         n = Observation)%>%
  select(lsoa21cd, age, n)

# Calculate proporiton in each age group
age<- age%>%
  group_by(lsoa21cd)%>%
  mutate(total = sum(n, na.rm = T))%>%
  ungroup()%>%
  mutate(proportion = 100*n/total)

# Pivot to wider
age<- age%>%
  pivot_wider(id_cols = lsoa21cd, names_from = age, values_from = proportion)

age%>%
  select(where(is.numeric))%>%
  cor()

# Rename age variables
colnames(age)[2:7]<- colnames(age)[2:7]%>% 
  str_replace_all(" ", "_")%>%
  str_to_lower()%>%
  paste0("proportion_", .)

# Join age data to main df
shp<- left_join(shp, age, by = c("LSOA21CD" = "lsoa21cd"))


#### Step 8: Local Authority Spending ####


# Read in pre-processed spending data
spending<- read.csv("../../../../Data/2018/LA Data/LAD_real_term_spending_per_person_aged_65_plus_2011_to_2018_data.csv")%>%
  select(-X)


# Read in LSOA11 to LAD22 lookup
lsoa_lad_lookup<- read.csv("../../../../Lookups/OA21_LSOA21_MSOA21_LAD22_EW_LU.csv")%>%
  select(lsoa21cd, lad22cd)

# Check all the LAD21s are in the LAD22 lookup
table(spending$LAD21CD %in% lsoa_lad_lookup$lad22cd)

# Join the LAD22CD to the main df
shp<- left_join(shp, lsoa_lad_lookup, by = c("LSOA21CD" = "lsoa21cd"))

# Join spending data to main df
shp<- left_join(shp, spending, by = c("lad22cd" = "LAD21CD"))

# Save LSOA data
save(shp, file = "Rates Data/2021 LSOAs with Rates and Independent Variables.Rda")




#############
#############


library(spdep)
library(spatialreg)

shp<- shp%>%
  filter(is.finite(age_standardised_rate))%>%
  filter(if_all(c(age_standardised_rate, excess_death_rate, imd_rank, non_couple_rate, age_standardised_rate_18, proportion_aged_65_years_and_over), ~!is.na(.x)))

nb<- poly2nb(shp2, queen = T)
lw<- nb2listw(nb, zero.policy = T)

moran.test(shp2$age_standardised_rate, listw = lw, zero.policy = T)

moran.plot(shp2$age_standardised_rate, listw = lw, zero.policy = T)

m1<- spatialreg::lmSLX(age_standardised_rate ~ excess_death_rate + age_standardised_rate_18 + imd_rank + non_couple_rate + substr(RUC11CD, 1, 1) + proportion_aged_65_years_and_over, data = shp2, listw = lw, zero.policy = T)
summary(m1)

m2<- errorsarlm(age_standardised_rate ~ excess_death_rate + age_standardised_rate_18 + imd_rank + non_couple_rate, data = shp2, listw = lw, zero.policy = T, method = "LU", Durbin = T)
summary(m2)


moran.test(m2$residuals, listw = lw, zero.policy = T)

moran.plot(m2$residuals, listw = lw, zero.policy = T)

?errorsarlm
