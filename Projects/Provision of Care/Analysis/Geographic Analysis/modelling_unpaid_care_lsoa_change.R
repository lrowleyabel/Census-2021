#######################################
#                                     #
#  MODELLING UNPAID CARE LSOA CHANGE  #
#                                     #
#######################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 12/07/2023

# DESCRIPTION: This file models the change in unpaid care in each LSOA in order to explore what factors may be associated with increasing/decreasing rates of care.

load("Rates Data/2021 LSOAs with Rates and Independent Variables.Rda")

# Filter out area with outlying 2018 mortality rate
shp<- shp%>%
  filter(age_standardised_rate_18<20)

# Look at the bivariate relationships for the variables of interest
shp%>%
  st_drop_geometry()%>%
  select(age_standardised_rate, excess_death_rate, age_standardised_rate_18, imd_rank, non_couple_rate)%>%
  GGally::ggpairs()

