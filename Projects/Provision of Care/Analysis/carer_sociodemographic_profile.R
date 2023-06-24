###############################
#                             #
#  CARER DEMOGRAPHIC PROFILE  #
#                             #
###############################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 14/06/2023

# DESCRIPTION: This file looks at the demographic profile of those provding unpaid care at
# the 2021 census. It then compares this to the profile of carers in 2011.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(systemfonts)
library(sociodemographics)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis")
rm(list = ls())


#### STEP 1: AGE AND SEX 2021 ####


# Read in 2021 carer by age and sex data
df_age_sex_21<- read.csv("../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_age_group_by_sex_2021_data.csv")

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
    plot.title = ggtext::element_textbox_simple(size = rel(1.2), margin = margin(10,0,20,0)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = rel(0.8), margin = margin(10,0,20,0), lineheight = 1.3),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0),
    panel.grid.minor = element_line(color = "#D2D2D2"),
    strip.text = element_text(face = "bold"))

pal<- wesanderson::wes_palette("GrandBudapest1")


df_total_age_sex_21%>%
  mutate(age = str_remove(age, "Aged "))%>%
  filter(care == "Provides unpaid care")%>%
  population_pyramid(count = n, age = age, side = sex, left = "Male", linewidth = 0.5, width = 0.75)+
  thm+
  labs(x = "Count", title = "Age-Sex Structure of Unpaid Carer Population: 2021",fill = "Sex", caption = "Source: England and Wales Census 2021")

ggsave("Plots/Age-Sex Structure of Unpaid Carers 2021.png", unit = "in", height = 5, width = 9, dpi = 1000)

# Add general population for reference
df_total_age_sex_21%>%
  group_by(age, sex)%>%
  summarise(n = sum(n, na.rm = T))%>%
  mutate(care = "General population")%>%
  rbind(df_total_age_sex_21%>%
          filter(care == "Provides unpaid care"))%>%
  mutate(age = str_remove(age, "Aged "))%>%
  mutate(care = ifelse(care == "Provides unpaid care", "Unpaid carers", care))%>%
  ungroup()%>%
  population_pyramid(count = n, age = age, side = sex, left = "Male", reference_indicator = care, reference_population = "General population", proportions = T, linewidth = 0.5, width = 0.75, reference_point_linewidth = 0.5, reference_point_size = 2)+
  thm+
  theme(legend.box = "horizontal")+
  labs(x = "Proportion (%)", title = "Age-Sex Structure of Unpaid Carers Compared to General Population: 2021", caption = "Souce: Enlgand and Wales Census 2021")

ggsave("Plots/Age-Sex Structure of Unpaid Carers Compared to General Population 2021.png", unit = "in", height = 5, width = 9, dpi = 1000)
