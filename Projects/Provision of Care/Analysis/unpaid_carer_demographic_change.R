#####################################
#                                   #
#  UNPAID CARER DEMOGRAPHIC CHANGE  #
#                                   #
#####################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 14/06/2023

# DESCRIPTION: This file looks at the change in the demographic profile of those providing unpaid care between the 2011 and 2021 censuses.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(systemfonts)
devtools::install_github("lrowleyabel/sociodemographics")
library(sociodemographics)
library(nomisr)
library(patchwork)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis")
rm(list = ls())


#### STEP 1: AGE AND SEX ####

# Get 2011 age- and sex-specific carer counts from Nomis API
id<- nomis_search(name = "*DC3303EWr*")$id[1]

df_age_sex_11<- nomis_get_data(id = id, geography = "TYPE499", C_CARER = 2, C_AGE = 1:21, C_SEX = 1:2, C_HEALTH = 0, tidy = T)%>%
  filter(geography_name == "England and Wales")%>%
  select(c_carer_name, c_age_name, c_sex_name, obs_value)%>%
  rename(care = c_carer_name,
         age = c_age_name,
         sex = c_sex_name,
         n = obs_value)


# Get 2011 total population by age and sex from Nomis API
id<- nomis_search(name = "*LC1117EW*")$id[1]
nomis_get_metadata(id = id)
nomis_codelist(id = id, concept = "C_SEX")%>%
  print(n = 100)

total_age_sex_population_11<- nomis_get_data(id = id, geography = "TYPE499", C_SEX = 1:2, C_AGE = 1:39, tidy = T)%>%
  filter(geography_name == "England and Wales")%>%
  rename(sex = c_sex_name,
         age = c_age_name,
         total = obs_value)%>%
  select(sex, age, total)

# Read in 2021 age group population sizes by ethnicity so we can copy the age groups used
total_ethnicity_age_population_21<- read.csv("../../../Data/2021/EW Data/EW_ethnicity_by_age_group_2021_data.csv")

df_age_sex_11<- df_age_sex_11%>%
  mutate(age = collapse_age_groups(age, unique(total_ethnicity_age_population_21$Age..18.categories.)))%>%
  group_by(care, age, sex)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

total_age_sex_population_11<- total_age_sex_population_11%>%
  mutate(age = collapse_age_groups(age, unique(total_ethnicity_age_population_21$Age..18.categories.)))%>%
  group_by(age, sex)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()


# Calculate proportion providing unpaid care by age and sex in 2011
df_age_sex_11<- left_join(df_age_sex_11, total_age_sex_population_11, by = c("age", "sex"))

df_age_sex_11<- df_age_sex_11%>%
  mutate(proportion = 100*n/total)

# Read in the 2021 rates by age and sex
load("Rates Data/Age Sex 2021 Rates.Rda")

# Join 2011 and 2021 rates
age_sex_rates<- left_join(df_age_sex_11%>%
                            mutate(sex = str_remove(sex, "(?<=ale)s"))%>%
                            rename(proportion_11 = proportion), df_total_age_sex_21%>%
                                                                  filter(care == "Provides unpaid care"), by = c("age", "sex"))

# Plot change in raw rates by age and sex

# Set default theme and colour palette
thm<-  ggthemes::theme_fivethirtyeight()+
  theme(
    text = element_text(family = "Rubik"),
    axis.title.x = element_text(size = rel(0.9), margin = margin(10,0,0,0)),
    plot.title = ggtext::element_textbox_simple(size = rel(1.2), margin = margin(10,0,10,0)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = rel(0.8), margin = margin(0,0,20,0), lineheight = 1.3),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0),
    panel.grid.minor = element_line(color = "#D2D2D2"),
    strip.text = element_text(face = "bold"),
    legend.title = element_text(size = rel(0.9)),
    legend.text = element_text(size = rel(0.9)))

pal<- wesanderson::wes_palette("GrandBudapest1")

age_sex_rates%>%
  pivot_longer(cols = c(proportion_11, proportion), names_to = "year", values_to = "proportion")%>%
  mutate(year = ifelse(year == "proportion", "2021", "2011"))%>%
  mutate(age = str_remove(age, "Aged "))%>%
  population_pyramid(count = proportion,age = age, side = sex, left = "Male", reference_indicator = year, reference_population = "2011", linewidth = 0.5, width = 0.75, reference_point_linewidth = 0.5)+
  thm+
  theme(legend.box = "horizontal")+
  labs(title = "Proportion Providing Unpaid Care by Age and Sex: 2021 and 2011", x = "Proportion providing unpaid care (%)", caption = "Source: England and Wales Census 2021 and 2011")

ggsave(filename = "Plots/Proportion Providing Unpaid Care by Age and Sex 2021 and 2011.png", units = "in", width = 9, height = 5, dpi = 1000)

age_sex_rates%>%
  mutate(age = str_remove(age, "Aged "))%>%
  mutate(age = order_age_groups(age))%>%
  ggplot()+
    geom_col(aes(x = proportion - proportion_11, y = age, fill = sex), position = position_dodge(),color = "black", size = 0.25, width = 0.75)+
  thm+
  scale_fill_manual(values = pal, name = "Sex")+
  labs(x = "Change in proportion providing unpaid care (%)", title = "Change in Proportion Providing Unpaid Care by Age and Sex: 2011 to 2021", caption = "Source: England and Wales Census 2021 and 2011")


ggsave(filename = "Plots/Change in Proportion Providing Unpaid Care by Age and Sex 2021 and 2011.png", units = "in", width = 9, height = 5, dpi = 1000)

  
#### STEP 2: ETHNICITY ####


# Get 2011 age-specific carer counts from Nomis API
id<- nomis_search(name = "*DC3303EWr*")$id[1]

standard_counts_11<- nomis_get_data(id = id, geography = "TYPE499", C_CARER = 2, C_AGE = 1:21, C_SEX = 0, C_HEALTH = 0, tidy = T)%>%
  select(geography_name, c_carer_name, c_age_name, obs_value)%>%
  filter(geography_name == "England and Wales")

# Rename columns
colnames(standard_counts_11)<- c("geography", "care", "age", "n")

# Get total age-group population for 2011 from Nomis API
id<- nomis_search(name = "*QS103EW*")$id[1]

standard_age_population_11<- nomis_get_data(id = id, geography = "TYPE499", C_AGE = 1:101, RURAL_URBAN = 0, measures = 20100, tidy = T)%>%
  filter(geography_name == "England and Wales")%>%
  rename(age = c_age_name,
         total = obs_value)%>%
  select(age, total)


# Get carer counts by ethnicity from Nomis API
id<- nomis_search(name = "*DC2301EW*")$id[1]

ethnicity_ids<- nomis_codelist(id = id, concept = "C_ETHPUK11")%>%
  filter(id != 0 & parentCode != 0)


df_ethnicity_11<- nomis_get_data(id = id, geography = "TYPE499", C_CARER = 2, C_ETHPUK11 = ethnicity_ids$id, C_HEALTH = 0, tidy = T)%>%
  filter(geography_name == "England and Wales")%>%
  select(c_carer_name, c_ethpuk_11_name, obs_value)%>%
  rename(care = c_carer_name,
         ethnicity = c_ethpuk_11_name,
         n = obs_value)

# Simplify ethnicity categories
df_ethnicity_11<- df_ethnicity_11%>%
  mutate(ethnicity = str_extract(ethnicity, "(?<=: ).*"))

# Get age group populations by ethnicity from Nomis API
id<- nomis_search(name = "*LC2109EWls*")$id[1]

total_ethnicity_age_population<- nomis_get_data(id = id, geography = "TYPE499", C_ETHPUK11 = ethnicity_ids$id, C_AGE = 1:21, tidy = T)%>%
  filter(geography_name == "England and Wales")%>%
  rename(ethnicity = c_ethpuk_11_name,
         age = c_age_name,
         total = obs_value)%>%
  select(ethnicity, age, total)

# Simplify ethnicity categories
total_ethnicity_age_population<- total_ethnicity_age_population%>%
  mutate(ethnicity = str_extract(ethnicity, "(?<=: ).*"))


# Calculate total population size by ethnicity
total_ethnicity_population<- total_ethnicity_age_population%>%
  group_by(ethnicity)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()

# Calculate raw proportion providing unpaid care by ethnicity in 2011
df_ethnicity_11<- left_join(df_ethnicity_11, total_ethnicity_population, by = "ethnicity")

ethnicity_rates_11<- df_ethnicity_11%>%
  mutate(proportion = 100*n/total)%>%
  select(ethnicity, n, proportion)


# Read in 2021 age group population sizes by ethnicity so we can copy the age groups used
total_ethnicity_age_population_21<- read.csv("../../../Data/2021/EW Data/EW_ethnicity_by_age_group_2021_data.csv")

# Recode the age groups in the 2011 data to match the 2021 data
standard_age_population_11<- standard_age_population_11%>%
  mutate(age = collapse_age_groups(age, unique(total_ethnicity_age_population_21$Age..18.categories.)))%>%
  group_by(age)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()

standard_counts_11<- standard_counts_11%>%
  mutate(age = collapse_age_groups(age, unique(total_ethnicity_age_population_21$Age..18.categories.)))%>%
  group_by(age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

total_ethnicity_age_population<- total_ethnicity_age_population%>%
  mutate(age = collapse_age_groups(age, unique(total_ethnicity_age_population_21$Age..18.categories.)))%>%
  group_by(ethnicity, age)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()


# Calculate age-standardised rates by ethnicity
ethnicity_age_standardised_rates_11<- age_standardise_indirect(interest_counts = df_ethnicity_11, interest_age_specific_population = total_ethnicity_age_population, standard_age_specific_counts = standard_counts_11, standard_age_specific_population = standard_age_population_11, count_variable = n, population_variable = total, age_variable = age, interest_grouping_variable = ethnicity)

# Join raw and age-standardised  2011 rates by ethnicity
ethnicity_rates_11<- left_join(ethnicity_rates_11, ethnicity_age_standardised_rates_11, by = "ethnicity")%>%
  rename(n = n.x)%>%
  select(ethnicity, n, proportion, age_standardised_rate)

# Read in rates for 2021
load("Rates Data/Ethnicity 2021 Rates.Rda")

# Join 2011 rates to 2021 rates
ethnicity_rates_11<- ethnicity_rates_11%>%
  mutate(ethnicity = case_when(substr(ethnicity, 1, 7) == "English" ~ "English, Welsh, Scottish, Northern Irish or British",
                               ethnicity == "Other Mixed" ~ "Other Mixed or Multiple ethnic groups",
                               T ~ ethnicity))


ethnicity_rates<- left_join(ethnicity_rates, ethnicity_rates_11%>%
                              select(ethnicity, n, proportion, age_standardised_rate)%>%
                              rename(n_11 = n,
                                     proportion_11 = proportion,
                                     age_standardised_rate_11 = age_standardised_rate))

# Plot change in raw rates

# Set default theme and colour palette
thm<-  ggthemes::theme_fivethirtyeight()+
  theme(
    text = element_text(family = "Rubik"),
    axis.title.x = element_text(size = rel(0.9), margin = margin(10,0,0,0)),
    plot.title = ggtext::element_textbox_simple(size = rel(1.2), margin = margin(10,0,10,0)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = rel(0.8), margin = margin(0,0,20,0), lineheight = 1.3),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0),
    panel.grid.minor = element_line(color = "#D2D2D2"),
    strip.text = element_text(face = "bold"),
    legend.title = element_text(size = rel(0.9)),
    legend.text = element_text(size = rel(0.9)))

pal<- wesanderson::wes_palette("GrandBudapest1")

ethnicity_rates%>%
  mutate(ethnicity = forcats::fct_reorder(ethnicity, proportion))%>%
  ggplot()+
  geom_segment(aes(x = proportion, xend = proportion_11, y = ethnicity, yend = ethnicity))+
  geom_point(aes(x = proportion, y = ethnicity, fill = "2021"), shape = 21, size = 3)+
  geom_point(aes(x = proportion_11, y = ethnicity, fill = "2011"), shape = 21, size = 3)+
  scale_fill_manual(values = pal, name = "Year")+
  thm+
  labs(x = "Raw Proportion Providing Unpaid Care (%)", title = "Raw Proportion Providing Unpaid Care by Ethnicity: 2021 and 2011", caption = "No 2011 rate for Roma ethnic group. Source: England and Wales Census 2021 and 2011.")

ggsave(filename = "Plots/Proportion Providing Unpaid Care by Ethnicity 2021 and 2011.png", units = "in", width = 9, height = 5, dpi = 1000)

ethnicity_rates%>%
  filter(ethnicity != "Roma")%>%
  mutate(change = proportion - proportion_11)%>%
  mutate(ethnicity = forcats::fct_reorder(ethnicity, change))%>%
  ggplot()+
  geom_col(aes(x = change, y = ethnicity), fill = pal[1], width = 0.75, linewidth = 0.5, color = "black")+
  scale_fill_manual(values = pal, name = "Year")+
  thm+
  labs(x = "Change in Proportion Providing Unpaid Care (%)", title = "Change in Proportion Providing Unpaid Care by Ethnicity: 2011 to 2021", caption = "No 2011 data for Roma ethnic group. Source: England and Wales Census 2021 and 2011.")

ggsave(filename = "Plots/Change in Proportion Providing Unpaid Care by Ethnicity 2021 and 2011.png", units = "in", width = 9, height = 5, dpi = 1000)



# Plot change in age-standardised rates

ethnicity_rates%>%
  mutate(ethnicity = forcats::fct_reorder(ethnicity, age_standardised_rate))%>%
  ggplot()+
  geom_segment(aes(x = age_standardised_rate, xend = age_standardised_rate_11, y = ethnicity, yend = ethnicity))+
  geom_vline(aes(xintercept = 1), linetype=2)+
  geom_point(aes(x = age_standardised_rate, y = ethnicity, fill = "2021"), shape = 21, size = 3)+
  geom_point(aes(x = age_standardised_rate_11, y = ethnicity, fill = "2011"), shape = 21, size = 3)+
  scale_x_continuous(limits = c(0.38,1.62), breaks = seq(0.4,1.6,0.2))+
  scale_fill_manual(values = pal, name = "Year")+
  thm+
  labs(x = "Age-Standardised Caring Rate", title = "Age-Standardised Caring Rate by Ethnicity: 2021", subtitle = "Rates indirectly standardised to the 2021 England and Wales general population", caption = "No 2011 rate for Roma ethnic group. Source: England and Wales Census 2021 and 2011.")

ggsave(filename = "Plots/Age-Standardised Caring Rate by Ethnicity 2021 and 2011.png", units = "in", width = 9, height = 5, dpi = 1000)
