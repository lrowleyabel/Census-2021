######################################
#                                    #
#  UNPAID CARER DEMOGRAPHIC PROFILE  #
#                                    #
######################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 14/06/2023

# DESCRIPTION: This file looks at the socio-demographic profile of those providing unpaid care at the 2021 census.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(systemfonts)
library(sociodemographics)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis/Overall Analysis")
rm(list = ls())


#### STEP 1: AGE AND SEX ####


# Read in 2021 carer by age and sex data
df_age_sex_21<- read.csv("../../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_age_group_by_sex_2021_data.csv")

# Select and rename relevant columns
df_age_sex_21<- df_age_sex_21%>%
  select(Unpaid.care..5.categories., Age..18.categories., Sex..2.categories., Observation)

colnames(df_age_sex_21)<- c("care", "age", "sex", "n")

# Merge "Does not apply" and "Provides no unpaid care" categories
df_age_sex_21<- df_age_sex_21%>%
  mutate(care = case_when(care == "Does not apply" ~ "Provides no unpaid care",
                          T ~ care))%>%
  group_by(care, age, sex)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Calculate total count of people providing any amount of care within each age-sex group
df_total_age_sex_21<- df_age_sex_21%>%
  mutate(care = case_when(care == "Provides no unpaid care" ~ "Provides no unpaid care",
                          T ~ "Provides unpaid care"))%>%
  group_by(care, age, sex)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Plot age-sex structure of carers

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


df_total_age_sex_21%>%
  mutate(age = str_remove(age, "Aged "))%>%
  filter(care == "Provides unpaid care")%>%
  population_pyramid(count = n, age = age, side = sex, left = "Male", linewidth = 0.5, width = 0.75)+
  thm+
  labs(x = "Count", title = "Age-Sex Structure of Unpaid Carer Population: 2021",fill = "Sex", caption = "Source: England and Wales Census 2021")

ggsave("Plots/Age-Sex Structure of Unpaid Carers 2021.png", unit = "in", height = 5, width = 9, dpi = 1000)

# Add general population for reference

total_population_age_sex_21<- df_total_age_sex_21%>%
  group_by(age, sex)%>%
  summarise(n = sum(n, na.rm = T))

total_population_age_sex_21%>%
  mutate(care = "General population")%>%
  rbind(df_total_age_sex_21%>%
          filter(care == "Provides unpaid care"))%>%
  mutate(age = str_remove(age, "Aged "))%>%
  mutate(care = ifelse(care == "Provides unpaid care", "Unpaid carers", care))%>%
  ungroup()%>%
  population_pyramid(count = n, age = age, side = sex, left = "Male", reference_indicator = care, reference_population = "General population", proportions = T, linewidth = 0.5, width = 0.75, reference_point_linewidth = 0.5, reference_point_size = 2)+
  thm+
  theme(legend.box = "horizontal")+
  labs(x = "Proportion (%)", title = "Age-Sex Structure of Unpaid Carers Compared to General Population: 2021", subtitle = "Bars are each age-sex group's proportion within unpaid carers. Circles are each age-sex group's proportion within general population.", caption = "Souce: Enlgand and Wales Census 2021")

ggsave("Plots/Age-Sex Structure of Unpaid Carers Compared to General Population 2021.png", unit = "in", height = 5, width = 9, dpi = 1000)


# Calculate and plot rates for each age-sex group
df_total_age_sex_21<- left_join(df_total_age_sex_21, total_population_age_sex_21%>%
                                  rename(total_population = n), by = c("age", "sex"))

df_total_age_sex_21<- df_total_age_sex_21%>%
  mutate(proportion = 100*n/total_population)

df_total_age_sex_21%>%
  filter(care == "Provides unpaid care")%>%
  mutate(age = str_remove(age, "Aged "))%>%
population_pyramid(age = age, side = sex, left = "Male", count = proportion, linewidth = 0.5, width = 0.75)+
  thm+
  labs(x = "Proportion providing unpaid care (%)", title = "Proportion Providing Unpaid Care by Age and Sex: 2021", caption = "Souce: Enlgand and Wales Census 2021", fill = "Sex")

ggsave("Plots/Proportion Providing Unpaid Care by Age and Sex 2021.png", unit = "in", height = 5, width = 9, dpi = 1000)

# Save rates by age and sex
save(df_total_age_sex_21, file = "Rates Data/Age Sex 2021 Rates.Rda")

# Calculate overall rates by sex (both for overall provision and for provision split by hours)
df_age_sex_21<- left_join(df_age_sex_21, total_population_age_sex_21%>%
                            rename(total_population = n), by = c("age", "sex"))

df_hours_sex_21<- df_age_sex_21%>%
  group_by(care, sex)%>%
  summarise(n = sum(n, na.rm = T), total_population = sum(total_population, na.rm = T))%>%
  filter(care != "Provides no unpaid care")%>%
  ungroup()

df_hours_sex_21<- df_hours_sex_21%>%
  mutate(proportion = 100*n/total_population)

df_total_sex_21<- df_hours_sex_21%>%
  group_by(sex)%>%
  filter(care != "Provides no unpaid care")%>%
  summarise(n = sum(n, na.rm = T))%>%
  mutate(care = "Provides unpaid care")%>%
  select(care, sex, n)

total_population_sex_21<- total_population_age_sex_21%>%
  group_by(sex)%>%
  summarise(n = sum(n, na.rm = T))

df_total_sex_21<- left_join(df_total_sex_21, total_population_sex_21%>%
                              rename(total_population = n), by = "sex")

df_total_sex_21<- df_total_sex_21%>%
  mutate(proportion = 100*n/total_population)

# Plot overall rates by sex

format_as_change<- function(x, decimals = 2, percent = TRUE){
  x<- round(x, 2)
  x<- ifelse(x >= 0, paste0("+", x, "%"), paste0(x, "%"))
  return(x)
}

df_hours_sex_21%>%
  mutate(type = "By Hours")%>%
  rbind(df_total_sex_21%>%
          mutate(type = "Overall"))%>%
  mutate(type = factor(type, levels = c("Overall", "By Hours")))%>%
  pivot_wider(id_cols = c(care, type), names_from = sex, values_from = proportion)%>%
  ggplot()+
  geom_segment(aes(x = Female, xend = Male, y = care, yend = care))+
  geom_point(aes(x = Female, y = care, fill = "Female"), shape = 21, color = "black", size = 3)+
  geom_point(aes(x = Male, y = care, fill = "Male"), shape = 21, color = "black", size = 3)+
  geom_text(aes(x = (Female+Male)/2, y = care, label = format_as_change(Female - Male)), position = position_nudge(y = 0.4), size = 3)+
  ggforce::facet_col(~type, scales = "free_y", space = "free")+
  thm+
  scale_fill_manual(values = pal, name = "Sex")+
  labs(x = "Proportion of population (%)", title = "Proportion of Population Providing Unpaid Care by Sex: 2021", subtitle = "Rates shown for overall care and care separated by amount provided\nLabels show difference between men and women", caption = "Source: England and Wales Census 2021")

ggsave("Plots/Proportion Providing Unpaid Care by Sex 2021.png", unit = "in", width = 9, height = 5, dpi = 1000)



#### STEP 2: ETHNICITY ####


# Read in care by ethnicity data
df_ethnicity_21<- read.csv("../../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_ethnicity_2021_data.csv")

# Select and rename relevant variables
df_ethnicity_21<- df_ethnicity_21%>%
  select(Ethnic.group..20.categories., Unpaid.care..5.categories., Observation)%>%
  rename(ethnicity = Ethnic.group..20.categories.,
         care = Unpaid.care..5.categories.,
         n = Observation)

# Filter out "Does not apply" ethnicity category
df_ethnicity_21<- df_ethnicity_21%>%
  filter(ethnicity != "Does not apply")

# Simplify ethnicity categories
df_ethnicity_21<- df_ethnicity_21%>%
  mutate(ethnicity = str_extract(ethnicity, "(?<=: ).*"))

# Merge "Does not apply" and "Provides no unpaid care" categories
df_ethnicity_21<- df_ethnicity_21%>%
  mutate(care = case_when(care == "Does not apply" ~ "Provides no unpaid care",
                          T ~ care))%>%
  group_by(care, ethnicity)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Calculate overall counts of people providing any amount of care
df_total_ethnicity_21<- df_ethnicity_21%>%
  mutate(care = case_when(care != "Provides no unpaid care" ~ "Provides unpaid care",
                          T ~ care))%>%
  group_by(care, ethnicity)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Calculate population totals by ethnicity
total_population_ethnicity_21<- df_total_ethnicity_21%>%
  group_by(ethnicity)%>%
  summarise(total_population = sum(n, na.rm = T))%>%
  ungroup()

# Calculate proportion of each ethnic group providing care
df_total_ethnicity_21<- left_join(df_total_ethnicity_21, total_population_ethnicity_21, by = "ethnicity")

df_total_ethnicity_21<- df_total_ethnicity_21%>%
  mutate(proportion = 100*n/total_population)

# Plot proportions by ethnicity

df_total_ethnicity_21%>%
  filter(care != "Provides no unpaid care")%>%
  mutate(ethnicity = forcats::fct_reorder(ethnicity, proportion))%>%
  ggplot()+
  geom_segment(aes(x = 0, xend = proportion, y = ethnicity, yend = ethnicity))+
  geom_point(aes(x = proportion, y = ethnicity), fill = pal[1], shape = 21, color = "black", size = 3)+
  thm+
  scale_fill_manual(values = pal)+
  labs(x = "Proportion providing unpaid care (%)", title = "Proportion of Population Providing Unpaid Care by Ethnicity: 2021", caption = "Source: England and Wales Census 2021")

ggsave("Plots/Proportion Providing Unpaid Care by Ethnicity 2021.png", unit = "in", width = 9, height = 5, dpi = 1000)


# Calculate age-standardised rates for each ethnic group, using indirect standardisation with the general population in 2021 as the baseline

# Read in age-specific count of carers for general population
standard_counts<- read.csv("../../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_single_year_of_age_2021_data.csv")

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
standard_age_population<- read.csv("../../../../Data/2021/EW Data/EW_age_group_2021_data.csv")

# Select and rename relevant variables
standard_age_population<- standard_age_population%>%
  select(Age..18.categories., Observation)%>%
  rename(age = Age..18.categories.,
         total = Observation)

# Collapse the age groups in the general population carer counts to the same as those in the population sizes data
standard_counts<- standard_counts%>%
  mutate(age = collapse_age_groups(age, unique(standard_age_population$age)))


# Read in age group populations by ethnicity
total_population_ethnicity_age_21<- read.csv("../../../../Data/2021/EW Data/EW_ethnicity_by_age_group_2021_data.csv")

# Select and rename relevant variables
total_population_ethnicity_age_21<- total_population_ethnicity_age_21%>%
  select(Ethnic.group..20.categories., Age..18.categories. , Observation)%>%
  rename(ethnicity = Ethnic.group..20.categories.,
         age = Age..18.categories.,
         total = Observation)


# Simplify ethnicity categories
total_population_ethnicity_age_21<- total_population_ethnicity_age_21%>%
  mutate(ethnicity = str_extract(ethnicity, "(?<=: ).*"))


# Calculate age-standardised rat
ethnicity_rates<- age_standardise_indirect(interest_counts = df_ethnicity_21, interest_age_specific_population = total_population_ethnicity_age_21, standard_age_specific_counts = standard_counts, standard_age_specific_population = standard_age_population, count_variable = n, population_variable = total, age_variable = age, interest_grouping_variable = ethnicity, sum_counts_across = care, exclude_counts = "Provides no unpaid care")

# Plot age-standardised rate by ethnicity
p<- ethnicity_rates%>%
  mutate(ethnicity = forcats::fct_reorder(ethnicity, age_standardised_rate))%>%
  ggplot()+
    geom_segment(aes(x = 1, xend = age_standardised_rate, y = ethnicity, yend = ethnicity))+
    geom_vline(aes(xintercept = 1), linetype=2)+
    geom_point(aes(x = age_standardised_rate, y = ethnicity), shape = 21, fill = pal[1], size = 3)+
    scale_x_continuous(limits = c(0.38,1.62), breaks = seq(0.4,1.6,0.2))+
    thm+
    labs(x = "Age-Standardised Caring Rate", title = "Age-Standardised Caring Rate by Ethnicity: 2021", subtitle = "Rates indirectly standardised to the 2021 England and Wales general population", caption = "Source: England and Wales Census 2021")

p

ggsave(plot = p, "Plots/Age-Standardised Caring Rates by Ethnicity 2021.png", units = "in", width = 9, height = 5, dpi = 1000)

save(p, file = "Plots/GGPlot Object Age-Standardised Caring Rates by Ethnicity 2021.Rda")


# Join the raw proportions to the age-standardised rates and save

ethnicity_rates<- left_join(ethnicity_rates, df_total_ethnicity_21%>%
                              filter(care == "Provides unpaid care")%>%
                              select(ethnicity, total_population, proportion), by = "ethnicity")

save(ethnicity_rates, file = "Rates Data/Ethnicity 2021 Rates.Rda")


