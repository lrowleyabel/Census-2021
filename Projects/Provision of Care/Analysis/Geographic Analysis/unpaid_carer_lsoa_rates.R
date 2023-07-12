#############################
#                           #
#  UNPAID CARER LSOA RATES  #
#                           #
#############################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 11/07/2023

# DESCRIPTION: This file calculates the proportion providing unpaid care in each LSOA in England and Wales in 2021. It then
# looks at how this proportion has changed since 2011 by looking at the raw change and by calculating the 2021 rates indirectly
# age-standardised to the 2011 populaiton.


library(dplyr)
library(tidyr)
library(ggplot2)
library(sociodemographics)
library(nomisr)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis/Overall Analysis")
rm(list = ls())

# Read in unpaid care data by LSOA for 2021
df<- read.csv("../../../../Data/2021/LSOA Data/lsoa_provision_of_unpaid_care_2021_data.csv")

df<- df%>%
  rename(lsoa21cd = Lower.layer.Super.Output.Areas.Code,
         care = Unpaid.care..5.categories.,
         n = Observation)%>%
  select(lsoa21cd, care, n)


# Merge care categories to have binary indicator of providing care
df<- df%>%
  mutate(care = case_when(care %in% c("Does not apply", "Provides no unpaid care") ~ "Providing no unpaid care",
                          T ~ "Provides unpaid care"))%>%
  group_by(lsoa21cd, care)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Calculate proportion providing care
df<- df%>%
  group_by(lsoa21cd)%>%
  mutate(total = sum(n, na.rm = T))%>%
  ungroup()%>%
  mutate(proportion = 100*n/total)%>%
  filter(care == "Provides unpaid care")

# Download unpaid care data by LSOA for 2011 from NOMIS API


