######################################
#                                    #
#  UNPAID CARER DEMOGRAPHIC PROFILE  #
#                                    #
######################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 14/06/2023

# DESCRIPTION: This file looks at the socio-economic profile of those providing unpaid care at the 2021 census.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(systemfonts)
devtools::install_github("lrowleyabel/sociodemographics")
library(sociodemographics)
library(patchwork)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis")
rm(list = ls())



#### STEP 1: NS-SEC ####


# Read in age-specific count of carers for general population
standard_counts<- read.csv("../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_single_year_of_age_2021_data.csv")

# Select and rename relevant variables
standard_counts<- standard_counts%>%
  select(Age..101.categories., Unpaid.care..5.categories., Observation)%>%
  rename(age = Age..101.categories.,
         care = Unpaid.care..5.categories.,
         n = Observation)

# Merge the "Does not apply" and "Provides no unpaid care" categories
standard_counts<- standard_counts%>%
  mutate(care = case_when(care == "Does not apply" ~ "Provides no unpaid care",
                          T ~ care))

# Read in the age-specific population sizes for general population
standard_age_population<- read.csv("../../../Data/2021/EW Data/EW_age_group_2021_data.csv")

# Select and rename relevant variables
standard_age_population<- standard_age_population%>%
  select(Age..18.categories., Observation)%>%
  rename(age = Age..18.categories.,
         total = Observation)

# Collapse the age groups in the general population carer counts to the same as those in the population sizes data
standard_counts<- standard_counts%>%
  mutate(age = collapse_age_groups(age, unique(standard_age_population$age)))

# Read in count of carers for each NS-SEC
df_nssec_21<- read.csv("../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_NSSEC_2021_data.csv")

# Select and rename the relevant variables
df_nssec_21<- df_nssec_21%>%
  rename(nssec = National.Statistics.Socio.economic.Classification..NS.SeC...10.categories.,
         care = Unpaid.care..5.categories.,
         n = Observation)%>%
  select(nssec, care, n)

# Read in age-specific populations sizes for each NS-SEC
total_population_nssec_21<- read.csv("../../../Data/2021/EW Data/EW_NSSEC_by_age_group_2021_data.csv")

# Select and rename relevant variables
total_population_nssec_21<- total_population_nssec_21%>%
  rename(nssec = National.Statistics.Socio.economic.Classification..NS.SeC...10.categories.,
         age = Age..18.categories.,
         total = Observation)%>%
  select(nssec, age, total)

# Calculate age-standardised rates
nssec_rates<- age_standardise_indirect(interest_counts = df_nssec_21, interest_age_specific_population = total_population_nssec_21, standard_age_specific_counts = standard_counts, standard_age_specific_population = standard_age_population, count_variable = n, population_variable = total, age_variable = age, interest_grouping_variable = nssec, sum_counts_across = care, exclude_counts = "Provides no unpaid care")

# Plot rates by NS-SEC

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

# Order NSSEC categories
nssec_rates<- nssec_rates%>%
  mutate(nssec = ons_nssec(nssec))

# Create plot
p1<- nssec_rates%>%
  filter(!is.na(nssec))%>%
  filter(!nssec %in% c("Does not apply", "Never worked and long-term unemployed"))%>%
  ggplot()+
  geom_segment(aes(x = 1, xend = age_standardised_rate, y = nssec, yend = nssec))+
  geom_vline(aes(xintercept = 1), linetype=2)+
  geom_point(aes(x = age_standardised_rate, y = nssec), shape = 21, fill = pal[1], size = 3)+
  scale_x_continuous(limits = c(0.38,1.62), breaks = seq(0.4,1.6,0.2))+
  scale_y_discrete(labels = function(x)str_replace(x, " and professional", "\nand professional"))+
  thm+
  labs(x = "Age-Standardised Caring Rate", title = "Age-Standardised Caring Rate by NS-SEC: 2021", subtitle = "Rates indirectly standardised to the 2021 England and Wales general population", caption = "Source: England and Wales Census 2021")

ggsave(plot = p1, filename = "Plots/Age-Standardised Caring Rates by NSSEC 2021.png", units = "in", width = 9, height = 5, dpi = 1000)

save(nssec_rates, file = "Rates Data/NSSEC 2021 Rates.Rda")


#### STEP 2: EDUCATION ####


# Read in carer counts by education
df_education_21<- read.csv("../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_education_2021_data.csv")

# Rename and select relevant variables
df_education_21<- df_education_21%>%
  rename(education = Highest.level.of.qualification..7.categories.,
         care = Unpaid.care..5.categories.,
         n = Observation)%>%
  select(education, care, n)

# Read in age group population sizes by education
total_population_education_21<- read.csv("../../../Data/2021/EW Data/EW_education_by_age_group_2021_data.csv")

# Rename and select relevant variables
total_population_education_21<- total_population_education_21%>%
  rename(education = Highest.level.of.qualification..7.categories.,
         age = Age..18.categories.,
         total = Observation)%>%
  select(education, age, total)

education_rates<- age_standardise_indirect(interest_counts = df_education_21, interest_age_specific_population = total_population_education_21, standard_age_specific_counts = standard_counts, standard_age_specific_population = standard_age_population, count_variable = n, population_variable = total, age_variable = age, interest_grouping_variable = education, sum_counts_across = care, exclude_counts = c("Does not apply", "Provides no unpaid care"))

# Simplify education categories
education_rates<- education_rates%>%
  mutate(education = case_when(str_detect(education, "Level 1") ~ "Lower GCSEs etc. and below",
                               str_detect(education, "Level 2") ~ "Higher GSCEs etc.",
                               str_detect(education, "Level 3") ~ "A-Levels etc.",
                               str_detect(education, "Level 4") ~ "Degree etc. and above",
                               str_detect(education, "Other: ") ~ "Other",
                               T ~ education))%>%
  mutate(education = factor(education, levels = c("Other", "Degree etc. and above", "A-Levels etc.", "Higher GSCEs etc.", "Lower GCSEs etc. and below", "No qualifications")))

p2<- education_rates%>%
  filter(!education %in% c("Does not apply"))%>%
  filter(!is.na(education))%>%
  ggplot()+
  geom_segment(aes(x = 1, xend = age_standardised_rate, y = education, yend = education))+
  geom_vline(aes(xintercept = 1), linetype=2)+
  geom_point(aes(x = age_standardised_rate, y = education), shape = 21, fill = pal[1], size = 3)+
  scale_x_continuous(limits = c(0.38,1.62), breaks = seq(0.4,1.6,0.2))+
  thm+
  labs(x = "Age-Standardised Caring Rate", title = "Age-Standardised Caring Rate by Education: 2021", subtitle = "Rates indirectly standardised to the 2021 England and Wales general population", caption = "Source: England and Wales Census 2021")

ggsave(plot = p2, filename = "Plots/Age-Standardised Caring Rates by Education 2021.png", units = "in", width = 9, height = 5, dpi = 1000)

save(education_rates, file = "Rates Data/Education 2021 Rates.Rda")


#### STEP 3: ECONOMIC ACTIVITY STATUS ####


# Read in carer counts by economic activity
df_economic_activity_21<- read.csv("../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_economic_activity_status_2021_data.csv")

# Rename and select relevant variables
df_economic_activity_21<- df_economic_activity_21%>%
  rename(economic_activity = Economic.activity.status..7.categories.,
         care = Unpaid.care..5.categories.,
         n = Observation)%>%
  select(economic_activity, care, n)

# Simplfy activity categories
df_economic_activity_21<- df_economic_activity_21%>%
  mutate(economic_activity = case_when(economic_activity == "Economically active (excluding full-time students): In employment" ~ "Employed",
                                       economic_activity == "Economically active (excluding full-time students): Unemployed: Seeking work or waiting to start a job already obtained: Available to start working within 2 weeks" ~ "Unemployed",
                                       economic_activity == "Economically active and a full-time student: In employment" ~ "Full-time student",
                                       economic_activity == "Economically active and a full-time student: Unemployed: Seeking work or waiting to start a job already obtained: Available to start working within 2 weeks" ~ "Full-time student",
                                       economic_activity == "Economically inactive (excluding full-time students)" ~ "Not in the labour market",
                                       economic_activity == "Economically inactive and a full-time student" ~ "Full-time student",
                                       economic_activity == "Does not apply" ~ "Does not apply"))%>%
  group_by(economic_activity, care)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Read in age group population sizes by economic activity
total_population_economic_activity_21<- read.csv("../../../Data/2021/EW Data/EW_economic_activity_status_by_age_group_2021_data.csv")

# Rename and select relevant variables
total_population_economic_activity_21<- total_population_economic_activity_21%>%
  rename(economic_activity = Economic.activity.status..7.categories.,
         age = Age..18.categories.,
         total = Observation)%>%
  select(economic_activity, age, total)

# Simplfy activity categories
total_population_economic_activity_21<- total_population_economic_activity_21%>%
  mutate(economic_activity = case_when(economic_activity == "Economically active (excluding full-time students): In employment" ~ "Employed",
                                       economic_activity == "Economically active (excluding full-time students): Unemployed: Seeking work or waiting to start a job already obtained: Available to start working within 2 weeks" ~ "Unemployed",
                                       economic_activity == "Economically active and a full-time student: In employment" ~ "Full-time student",
                                       economic_activity == "Economically active and a full-time student: Unemployed: Seeking work or waiting to start a job already obtained: Available to start working within 2 weeks" ~ "Full-time student",
                                       economic_activity == "Economically inactive (excluding full-time students)" ~ "Not in the labour market",
                                       economic_activity == "Economically inactive and a full-time student" ~ "Full-time student",
                                       economic_activity == "Does not apply" ~ "Does not apply"))%>%
  group_by(economic_activity, age)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()



# Calculate age-standardised rates by economic activity
economic_activity_rates<- age_standardise_indirect(interest_counts = df_economic_activity_21, interest_age_specific_population = total_population_economic_activity_21, standard_age_specific_counts = standard_counts, standard_age_specific_population = standard_age_population, count_variable = n, population_variable = total, age_variable = age, interest_grouping_variable = economic_activity, sum_counts_across = care, exclude_counts = c("Does not apply", "Provides no unpaid care"))

# Re-order activity categories
economic_activity_rates<- economic_activity_rates%>%
  mutate(economic_activity = factor(economic_activity, levels = c("Not in the labour market", "Full-time student", "Unemployed", "Employed")))

# Plot rates
p3<- economic_activity_rates%>%
  filter(!is.na(economic_activity))%>%
  ggplot()+
  geom_segment(aes(x = 1, xend = age_standardised_rate, y = economic_activity, yend = economic_activity))+
  geom_vline(aes(xintercept = 1), linetype=2)+
  geom_point(aes(x = age_standardised_rate, y = economic_activity), shape = 21, fill = pal[1], size = 3)+
  scale_x_continuous(limits = c(0.38,1.62), breaks = seq(0.4,1.6,0.2))+
  thm+
  labs(x = "Age-Standardised Caring Rate", title = "Age-Standardised Caring Rate by Economic Activity: 2021", subtitle = "Rates indirectly standardised to the 2021 England and Wales general population", caption = "Source: England and Wales Census 2021")

ggsave(plot = p3, filename = "Plots/Age-Standardised Caring Rates by Economic Activity 2021.png", units = "in", width = 9, height = 5, dpi = 1000)

save(economic_activity_rates, file = "Rates Data/Economic Activity 2021 Rates.Rda")


#### STEP 4: ALIGN ALL THE PLOTS ####


# Read in the ethnicity plot
load("Plots/GGPlot Object Age-Standardised Caring Rates by Ethnicity 2021.Rda")
p4<- p
rm(p)

# Align the four plots
aligned_plots<- align_patches(p1, p2, p3, p4)

# Remove the individual plot subtitles, captions etc.
merged_plot_labs_theme<- theme(plot.title = ggtext::element_textbox_simple(size = rel(1.2), margin = margin(10,0,10,0), halign = 0.5),
                               plot.title.position = "panel",
                               plot.subtitle = element_blank(),
                               plot.caption = element_blank()) 

# Add relevant titles and layout etc.
aligned_plots<- (p1 + labs(title = "NS-SEC") + p2 + labs(title = "Education") + p3 + labs(title = "Economic Activity") + p4 + labs(title = "Ethnicity")) & merged_plot_labs_theme
aligned_plots<- aligned_plots + plot_layout(ncol = 1, heights = c(1, 1, 1, 2))
aligned_plots<- aligned_plots + plot_annotation(title = "Age-Standardised Unpaid Caring Rates across Socio-Demographic Variables: 2021", subtitle = "Rates standardised to the 2021 general population", caption = "Source: England and Wales Census 2021", theme = thm + theme(plot.title.position = "plot", plot.title = ggtext::element_textbox_simple(size = rel(1.5), margin = margin(10,0,10,0))))

# Save merged plot
ggsave(plot = aligned_plots, filename = "Plots/Age-Standardised Caring Rates across Socio-Demographic Variables.png", unit = "in", width = 9, height = 20, dpi = 1000)
