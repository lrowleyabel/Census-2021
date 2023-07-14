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
library(sf)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis/Geographic Analysis")
rm(list = ls())


#### STEP 1: 2021 PROPORTION ####


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

# Plot histgram of raw proportion providing care

# Set default theme and colour palette
thm<-  ggthemes::theme_fivethirtyeight()+
  theme(
    text = element_text(family = "Rubik"),
    axis.title.x = element_text(size = rel(0.9), margin = margin(10,0,0,0)),
    plot.title = ggtext::element_textbox_simple(size = rel(1.2), margin = margin(10,0,20,0)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = rel(0.8), margin = margin(0,0,20,0), lineheight = 1.3),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, margin = margin(20,0,0,0)),
    panel.grid.minor = element_line(color = "#D2D2D2"),
    strip.text = element_text(face = "bold"))

pal<- wesanderson::wes_palette("GrandBudapest1")


ggplot(df)+
  geom_histogram(aes(x = proportion), bins = 100, color = "white")+
  thm+
  theme(axis.title.y = element_text())+
  labs(title = "Histogram of Proportion of Population Providing Unpaid Care by LSOA: 2021", x= "Proportion (%)", y = "Count", caption = "Sources: England and Wales Census 2021")

ggsave("Plots/Histogram of Proportion Providing Unpaid Care by LSOA 2021.png", units = "in", width = 9, height = 5, dpi = 1000)


#### STEP 2: CHANGE FROM 2011 ####


# Download unpaid care data by LSOA for 2011 from NOMIS API
id<- nomis_search(name = "*LC3304EW*")$id[1]

df_11<- nomis_get_data(id = id, geography = "TYPE298", C_CARER = c(0,2), C_AGE = 0, tidy = T)%>%
  select(geography_code, c_carer_name, obs_value)%>%
  rename(lsoa11cd = geography_code,
         care = c_carer_name,
         n = obs_value)
nomis_get_metadata(id)
nomis_codelist(id, "C_CARER")

# Pivot to wide format so that total population is a separate variable
df_11<- df_11%>%
  pivot_wider(id_cols = lsoa11cd, names_from = care, values_from = n)

df_11<- df_11%>%
  rename(n_11 = `Provides unpaid care: Total`,
         total_11 = `All categories: Provision of unpaid care`)

# Calculate proportion providing unpaid care
df_11<- df_11%>%
  mutate(proportion_11 = 100*n_11/total_11)


# Read in best-fit lsoa11 to lsoa21 lookup
lookup<- read.csv("../../../../Lookups/Best-Fit LSOA11 to LSOA21 to LAD22 EW.csv")%>%
  rename(lsoa11cd = LSOA11CD,
         lsoa21cd = LSOA21CD)%>%
  select(lsoa11cd, lsoa21cd)

# Remove 2021 LSOAs which are matched to more than one 2011 LSOA
duplicated_lsoa21s<- lookup$lsoa21cd[duplicated(lookup$lsoa21cd)]

lookup<- lookup%>%
  filter(!lsoa21cd %in% duplicated_lsoa21s)

# Join lookup to 2011 data
df_11<- left_join(df_11, lookup, by = "lsoa11cd")

# Join 2021 raw rates and 2011 raw rates
df<- left_join(df, df_11, by = "lsoa21cd")

# Calculate raw change in proporiton providing unpaid care
df<- df%>%
  mutate(change = proportion - proportion_11)

# Plot histogram of raw change
ggplot(df)+
  geom_histogram(aes(x = change), bins = 100, color = "white")+
  thm+
  theme(axis.title.y = element_text())+
  labs(title = "Histogram of Change in Proportion of Population Providing Unpaid Care by LSOA: 2011 to 2021", x= "Change in proportion (%)", y = "Count", caption = "Sources: England and Wales Census 2021 and 2011")

ggsave("Plots/Histogram of Change in Proportion Providing Unpaid Care 2011 and 2021.png", units = "in", width = 9, height = 5, dpi = 1000)

# Read in 2021 LSOA boundaries
shp<- read_sf("../../../../Boundaries/2021/LSOA/Super Generalised/LSOA_2021_EW_BSC.shp")

# Join data to boundaries
shp<- left_join(shp, df, by = c("LSOA21CD" = "lsoa21cd"))

# Map the proportion providing care in 2021
ggplot(shp)+
  geom_sf(aes(fill = proportion), color = NA)+
  scale_fill_viridis_c(name = "Proportion (%)", guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", barwidth = 15, barheight = 0.5, title.position = "top"))+
  thm+
  theme(axis.text = element_blank())+
  labs(title = "Proportion of Population Providing Unpaid Care by LSOA: 2021", caption = "Source: England and Wales Census 2021")

ggsave(filename = "Plots/Proportion Providing Unpaid Care by LSOA 2021.png", units = "in", height = 7, width = 5, dpi = 1000)

# Map change in the proportion providing care in 2021
ggplot(shp)+
  geom_sf(aes(fill = change), color = NA)+
  scale_fill_viridis_c(name = "Change in proportion (%)", guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", barwidth = 15, barheight = 0.5, title.position = "top"), breaks = seq(-8,6,2))+
  thm+
  theme(axis.text = element_blank())+
  labs(title = "Change in Proportion of Population Providing Unpaid Care by LSOA: 2011 to 2021", caption = "Source: England and Wales Census 2021 and 2011")

ggsave(filename = "Plots/Change in Proportion Providing Unpaid Care by LSOA 2011 and 2021.png", units = "in", height = 7, width = 5, dpi = 1000)


#### STEP 3: 2021 AGE-STANDARDISED RATES ####


id<- nomis_search(name = "*LC3304EW*")$id[1]

df_age_11<- nomis_get_data(id = id, geography = "TYPE298", C_CARER = c(0,2), C_AGE = 1:6, tidy = T)%>%
  select(geography_code, c_carer_name, c_age_name, obs_value)%>%
  rename(lsoa11cd = geography_code,
         care = c_carer_name,
         age = c_age_name,
         n = obs_value)


# Pivot to wide format so that total population is a separate variable
df_age_11<- df_age_11%>%
  pivot_wider(id_cols = c(lsoa11cd, age), names_from = care, values_from = n)

df_age_11<- df_age_11%>%
  rename(n_11 = `Provides unpaid care: Total`,
         total_11 = `All categories: Provision of unpaid care`)

# Calculate proportion providing unpaid care
df_age_11<- df_age_11%>%
  mutate(proportion_11 = 100*n_11/total_11)

# Separate age-specific population for 2011 as a df
total_population_age_11<- df_age_11%>%
  select(lsoa11cd, age, total_11)

# Read in age-specific carer counts for 2021
df_age_21<- read.csv("../../../../Data/2021/LSOA Data/lsoa_provision_of_unpaid_care_by_age_group_2021_data.csv")

# Calculate total age-specific population for 2021
total_population_age_21<- df_age_21%>%
  rename(lsoa21cd = Lower.layer.Super.Output.Areas.Code,
         age = Age..6.categories.,
         n = Observation)%>%
  select(lsoa21cd, age, n)%>%
  group_by(lsoa21cd, age)%>%
  summarise(total = sum(n, na.rm = T))%>%
  ungroup()

# Calculate age-standardised rate for each LSOA

interest_counts<- df%>%
  select(lsoa21cd, n)

interest_age_population<- total_population_age_21%>%
  select(lsoa21cd, age, total)

standard_age_counts<- df_age_11%>%
  left_join(lookup, by = "lsoa11cd")%>%
  select(lsoa21cd, age, n_11)%>%
  rename(n = n_11)

standard_age_population<- df_age_11%>%
  left_join(lookup, by = "lsoa11cd")%>%
  select(lsoa21cd, age, total_11)%>%
  rename(total = total_11)

standard_age_population<- standard_age_population%>%
  mutate(age = collapse_age_groups(age, unique(interest_age_population$age)))

standard_age_counts<- standard_age_counts%>%
  mutate(age = collapse_age_groups(age, unique(interest_age_population$age)))

age_standardised_rates<- age_standardise_indirect(interest_counts = interest_counts,
                                                  interest_age_specific_population = interest_age_population,
                                                  standard_age_specific_counts = standard_age_counts,
                                                  standard_age_specific_population = standard_age_population,
                                                  count_variable = n,
                                                  population_variable = total,
                                                  age_variable = age,
                                                  grouping_variable = lsoa21cd,
                                                  single_standard_population = F)

new_rates<- age_standardised_rates


# Plot distribution of age-standardised rates
ggplot(age_standardised_rates)+
  geom_histogram(aes(x = age_standardised_rate), bins = 100, color = "white")+
  thm+
  theme(axis.title.y = element_text(),
        plot.title = ggtext::element_textbox(margin = margin(10,0,10,0)))+
  labs(x = "Age-standardised rate", y = "Count", title = "Histogram of Indirectly Age-Standardised Unpaid Care Provision Rates by LSOA: 2021", subtitle = "2021 rates standardised to the 2011 population's age-structure", caption = "Source: England and Wales Census 2021 and 2011")

ggsave("Plots/Histogram of Age-Standardised Rates of Unpaid Care Provision by LSOA 2021.png", units = "in", height = 5, width = 9, dpi = 1000)


# Map age-standardised rates
shp<- left_join(shp, select(age_standardised_rates, lsoa21cd, age_standardised_rate), by = c("LSOA21CD" = "lsoa21cd"))

ggplot(shp)+
  geom_sf(aes(fill = age_standardised_rate), color = NA)+
  scale_fill_viridis_c(name = "Age-standardised rate", guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", barwidth = 15, barheight = 0.5, title.position = "top"))+
  thm+
  theme(axis.text = element_blank())+
  labs(title = "Indirectly Age-Standardised Unpaid Care Provision Rates by LSOA: 2021", subtitle = "2021 rates standardised to the 2011 population's age-structure", caption = "Source: England and Wales Census 2021 and 2011")

ggsave(filename = "Plots/Indirectly Age-Standardised Unpaid Care Provision Rates by LSOA 2021.png", units = "in", height = 7, width = 5, dpi = 1000)


# Save rates by LSOA
save(shp, file = "Rates Data/2021 LSOAs with Rates.Rda")
