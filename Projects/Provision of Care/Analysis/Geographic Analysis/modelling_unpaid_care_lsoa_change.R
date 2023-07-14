#######################################
#                                     #
#  MODELLING UNPAID CARE LSOA CHANGE  #
#                                     #
#######################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 12/07/2023

# DESCRIPTION: This file models the change in unpaid care in each LSOA in order to explore what factors may be associated with increasing/decreasing rates of care.


library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(sociodemographics)
library(nomisr)
library(sf)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis/Geographic Analysis")
rm(list = ls())

# Load in LSOA rates
load("Rates Data/2021 LSOAs with Rates.Rda")

# Look at association between IMD and age-standardised rate
ggplot(shp)+
    geom_point(aes(x = imd_rank, y = age_standardised_rate), alpha = 0.1)+
    geom_smooth(aes(x = imd_rank, y = age_standardised_rate))

cor.test(shp$imd_rank, shp$age_standardised_rate, use = "com")


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

# Download  2011 living arrangements by LSOA data from NOMIS API
id<- nomis_search(name = "*QS108EW*")$id[1]

living_11<- nomis_get_data(id = id, geography = "TYPE298", C_ELARPUK11 = c(0,1,5), MEASURES = 20100, RURAL_URBAN = 0, tidy = T)

living_11<- living_11%>%
  rename(lsoa11cd = geography_code,
         living = c_elarpuk_11_name,
         n = obs_value)%>%
  select(lsoa11cd, living, n)

# Filter to just the "not living in a couple" category
living_11<- living_11%>%
  filter(living != "Living in a couple: Total")

# Pivot to wide format so total population is a separate column
living_11<- living_11%>%
  pivot_wider(id_cols = lsoa11cd, names_from = living, values_from = n)

living_11<- living_11%>%
  rename(n = `Not living in a couple: Total`,
         total = `All categories: Living arrangements`)

# Calculate proportion not living in a couple
living_11<- living_11%>%
  mutate(proportion_11 = 100*n/total)

# Join lsoa lookup to 2011 living arrangement data
living_11<- left_join(living_11, lookup, by = "lsoa11cd")

# Joing 2021 and 2011 living arrangement data
living<- left_join(living, living_11%>%
                     select(lsoa21cd, proportion_11))

# Caclulate change in proportion not living in a couple
living<- living%>%
  mutate(couple_change = proportion - proportion_11)

# Join living arrangement data to care data
shp<- left_join(shp, living%>%
                  select(lsoa21cd, couple_change), by = c("LSOA21CD" = "lsoa21cd"))

# Look at association between change in proportion not living in a couple and change in care provision
ggplot(shp)+
  geom_point(aes(x = couple_change, y = age_standardised_rate))+
  geom_smooth(aes(x = couple_change, y = age_standardised_rate))



cor.test(shp$age_standardised_rate, shp$transformed_couple_change, use = "com")
                     

m<- lm(change ~ imd_rank + couple_change, data = shp)
summary(m)
