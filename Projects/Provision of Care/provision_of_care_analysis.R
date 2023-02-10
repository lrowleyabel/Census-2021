################################
#                              #
#  PROVISION OF CARE ANALYSIS  #
#                              #
################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 08/02/23

# DESCRIPTION: This file calculates indirectly age-standardised unpaid care provision rates for the 2021 census at MSOA level.
# It uses the 2011 age-specific provision rates as a baseline in order to look at changes in unpaid care provision between the two censuses.


library(tidyverse)
library(ggthemr)
library(MetBrewer)
library(utils)
library(patchwork)
library(sf)

setwd(choose.dir(caption = "Select working directory to current file location"))
rm(list = ls())

# Read in age data for MSOAs in 2011
age_11<- read.csv("../../Data/2011/MSOA Data/msoa_age_by_single_year_2011_data.csv")

# Standardise capitalisation of column names
age_11<- age_11%>%
  rename_with(~str_to_lower(.x))

# Read in count of people providing unpaid care by age group data for MSOAs in 2011
care_11<- read.csv("../../Data/2011/MSOA Data/msoa_provision_of_unpaid_care_by_age_group_2011_data.csv")

# Summarise the age data by the age groups used in the care data
age_11<- age_11%>%
  rowwise(msoa11cd)%>%
  summarise(aged_0_to_24 = sum(c_across(age.under.1:age.24)),
         aged_25_to_49 = sum(c_across(age.25:age.49)),
         aged_50_to_64 = sum(c_across(age.50:age.64)),
         aged_65_and_over = sum(c_across(age.65:age.100.and.over)))

# Pivot age and care data to long format
age_11_long<- age_11%>%
  pivot_longer(cols = aged_0_to_24:aged_65_and_over, names_to = "age_group", values_to = "population")%>%
  mutate(age_group = str_extract(age_group, "[0-9][0-9]?_to_[0-9][0-9]?|65_and_over"))

care_11_long<- care_11%>%
  pivot_longer(cols = carers_aged_0_to_24:carers_aged_65_and_over, names_to = "age_group", values_to = "carers")%>%
  mutate(age_group = str_extract(age_group, "[0-9][0-9]?_to_[0-9][0-9]?|65_and_over"))

# Join the age data to the care data
care_11_long<- left_join(care_11_long, age_11_long, by = c("msoa11cd", "age_group"))

# Calculate age-specific provision rates (expressed as carers per 1000 people)
care_11_long<- care_11_long%>%
  mutate(provision_rate = 1000*(carers/population))

# Read in MSOA11 boundaries
shp<- read_sf("../../Data/2011/MSOA Boundaries Super Generalised/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp")

# Join the provision rates to the boundaries
shp<- right_join(shp, care_11_long, by = c("MSOA11CD" = "msoa11cd"))

# Map the age-specific provision rates and save as one plot
p1<- shp%>%
  filter(age_group == "0_to_24")%>%
  ggplot()+
    geom_sf(aes(fill = provision_rate), color = NA)+
    scale_fill_viridis_c(name = "")+
    labs(title = "Carers per 1000 ppl aged 0 - 24")+
    theme(legend.key.width = unit(0.1, "in"),
          legend.background = element_blank(),
          legend.position = c(0.15, 0.75),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 10))

p2<- shp%>%
  filter(age_group == "25_to_49")%>%
  ggplot()+
  geom_sf(aes(fill = provision_rate), color = NA)+
  scale_fill_viridis_c(name = "")+
  labs(title = "Carers per 1000 ppl aged 25 - 49")+
  theme(legend.key.width = unit(0.1, "in"),
        legend.background = element_blank(),
        legend.position = c(0.15, 0.75),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 10))


p3<- shp%>%
  filter(age_group == "50_to_64")%>%
  ggplot()+
  geom_sf(aes(fill = provision_rate), color = NA)+
  scale_fill_viridis_c(name = "")+
  labs(title = "Carers per 1000 ppl aged 50 - 64")+
  theme(legend.key.width = unit(0.1, "in"),
        legend.background = element_blank(),
        legend.position = c(0.15, 0.75),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 10))


p4<- shp%>%
  filter(age_group == "65_and_over")%>%
  ggplot()+
  geom_sf(aes(fill = provision_rate), color = NA)+
  scale_fill_viridis_c(name = "")+
  labs(title = "Carers per 1000 ppl aged 65 plus")+
  theme(legend.key.width = unit(0.1, "in"),
        legend.background = element_blank(),
        legend.position = c(0.15, 0.75),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 10))


p<- (p1 + p2 + p3 + p4 + plot_layout(ncol = 4))+plot_annotation(title = "Age-specific rates of provision of unpaid care in England and Wales at 2011 census", subtitle = "Note: colour scale varies for each age group")

ggsave(plot = p, filename = "Plots/age_specific_rates_2011.png", units = "in", height = 5, width = 9, dpi = 1000)


# Read in count of people providing unpaid care in each MSOA for 2021
care_21<- read.csv("../../Data/2021/MSOA Data/msoa_provision_of_unpaid_care_2021_data.csv")

# Calculate total number of carers for each MSOA
care_21<- care_21%>%
  rowwise(msoa21cd)%>%
  mutate(carers = sum(c_across(provides_9_hours_or_less_unpaid_care_a_week:provides_50_or_more_hours_unpaid_care_a_week)))

# Read in age data for each MSOA for 2021
age_21<- read.csv("../../Data/2021/MSOA Data/msoa_age_by_single_year_2021_data.csv")

# Standardise capitalisation of column names
age_21<- age_21%>%
  rename_with(~str_to_lower(.x))

# Summarise the age data by the age groups used in the care data
age_21<- age_21%>%
  rowwise(msoa21cd)%>%
  summarise(aged_0_to_24 = sum(c_across(aged.under.1.year:aged.24.years)),
            aged_25_to_49 = sum(c_across(aged.25.years:aged.49.years)),
            aged_50_to_64 = sum(c_across(aged.50.years:aged.64.years)),
            aged_65_and_over = sum(c_across(aged.65.years:aged.100.years.and.over)))

# Pivot the 2021 age to long format
age_21_long<- age_21%>%
  pivot_longer(cols = aged_0_to_24:aged_65_and_over, names_to = "age_group", values_to = "population")%>%
  mutate(age_group = str_remove(age_group, "aged_"))

# Read in the lookup to join 2011 MSOAs to 2021 MSOAs
lookup<- read.csv("../../Lookups/MSOA11 to MSOA21 to LAD22 EW.csv")

# Standardise capitalisation of column names
lookup<- lookup%>%
  rename_with(~str_to_lower(.x))

# Join the MSOA21 codes to the age-specific provision rates for 2011
care_11_long<- left_join(care_11_long, select(lookup, msoa11cd, msoa21cd))

# Rename population variable in the 2011 care data and the 2021 age data to specify which year they are from
care_11_long<- care_11_long%>%
  rename(carers_11 = carers,
         population_11 = population,
         provision_rate_11 = provision_rate)

age_21_long<- age_21_long%>%
  rename(population_21 = population)

# Join the count of people in each age group for 2021 to the age-specific provision rates for 2011
care<- left_join(care_11_long, age_21_long, by = c("msoa21cd", "age_group"))

# Calculate expected number of carers for each age group for 2021, based on 2011 provision rates
care<- care%>%
  mutate(expected_carers = (provision_rate_11/1000)*population_21)

# Calculate total expected number of carers for 2021
care<- care%>%
  group_by(msoa21cd)%>%
  summarise(total_expected_carers = sum(expected_carers))

# Keep only relevant variables for 2021 observed carer data
care_21<- care_21%>%
  rename(observed_carers = carers)%>%
  select(msoa21cd, observed_carers)

# Join observed number of carers for 2021 to the expected number of carers for 2021
care<- left_join(care, care_21, by = "msoa21cd")

# Calculate ratio of observed to expected number of carers for 2021
care<- care%>%
  mutate(indirectly_standardised_rate = observed_carers/total_expected_carers)

# Look at distribution of standardised rate
summary(care$indirectly_standardised_rate)

ggplot(care)+
  geom_histogram(aes(x = indirectly_standardised_rate), bins = 100, color = "white")

# Look at number of neighbourhoods for whom care rates have increased vs decreased
table(care$indirectly_standardised_rate>1)

# Read in 2021 MSOA boundaries
shp<- read_sf("../../Data/2021/MSOA Boundaries Super Generalised/MSOA_2021_EW_BSC.shp")

# Standardise capitalisation of variable names
shp<- shp%>%
  rename_with(~str_to_lower(.x))

# Join standardised rates to 2021 boundaries
shp<- left_join(shp, care, by = "msoa21cd")

# Map standardised rates in ggplot
shp%>%
  ggplot()+
    geom_sf(aes(geometry = geometry, fill = log(indirectly_standardised_rate)), color = NA)+
    scale_fill_viridis_c()


# Map standardised rates with Leaflet
shp<- filter(shp, indirectly_standardised_rate<1)

pal<- colorNumeric("Blues", domain = shp$indirectly_standardised_rate)

leaflet(st_transform(shp, crs = "WGS84"))%>%
  addPolygons(fillColor = ~pal(indirectly_standardised_rate), fillOpacity = 1, stroke = NA)%>%
  addLegend(pal = pal, values = shp$indirectly_standardised_rate, opacity = 1)
