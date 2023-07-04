###########################################
#                                         #
#  DECOMPOSING UNPAID CARER COUNT CHANGE  #
#                                         #
###########################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 03/07/2023

# DESCRIPTION: This file decomposes the change in the count of unpaid carers into the change attributable to population growth, population ageing and change in the underlying rate.

library(dplyr)
library(tidyr)
library(ggplot2)
library(sociodemographics)
library(nomisr)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis")
rm(list = ls())

# 2011 age-specific population
id<- nomis_search(name = "*LC1117EW*")$id[1]
nomis_get_metadata(id = id)
nomis_codelist(id = id, concept = "C_AGE")%>%
  print(n = 100)

df_age_11<- nomis_get_data(id = id, geography = "TYPE499", sex = 0, C_AGE = 1:39, tidy = T)%>%
  filter(geography_name == "England and Wales")%>%
  rename(age = c_age_name,
         total = obs_value)%>%
  select(age, total)


# 2021 age-specific population
df_age_21<- read.csv("../../../Data/2021/EW Data/EW_age_group_2021_data.csv")%>%
  rename(age = Age..18.categories.,
         total = Observation)%>%
  select(age, total)


# Collapse 2011 age groups to same as 2021
df_age_11<- df_age_11%>%
  mutate(age = collapse_age_groups(age, unique(df_age_21$age)))%>%
  group_by(age)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()


# 2011 carer counts by age
id<- nomis_search(name = "*DC3303EWr*")$id[1]

df_care_age_11<- nomis_get_data(id = id, geography = "TYPE499", C_CARER = 2, C_AGE = 1:21, C_SEX = 0, C_HEALTH = 0, tidy = T)%>%
  filter(geography_name == "England and Wales")%>%
  select(c_carer_name, c_age_name, c_sex_name, obs_value)%>%
  rename(care = c_carer_name,
         age = c_age_name,
         sex = c_sex_name,
         n = obs_value)%>%
  mutate(age = collapse_age_groups(age, unique(df_age_11$age)))%>%
  group_by(age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# 2021 carer counts by age
df_care_age_21<- read.csv("../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_single_year_of_age_2021_data.csv")%>%
  rename(care = Unpaid.care..5.categories.,
         age = Age..101.categories.,
         n = Observation)%>%
  select(care, age, n)%>%
  mutate(care = case_when(care %in% c("Does not apply", "Provides no unpaid care") ~ "Provides no unpaid care",
                          T ~ "Provides unpaid care"))%>%
  group_by(age, care)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()%>%
  filter(care == "Provides unpaid care")%>%
  mutate(age = collapse_age_groups(age, unique(df_age_21$age)))%>%
  group_by(age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()



df_age<- rbind(df_age_11%>%
                 mutate(year = 2011), df_age_21%>%
                                        mutate(year = 2021))


df_care_age<- rbind(df_care_age_11%>%
                      mutate(year = 2011), df_care_age_21%>%
                                            mutate(year = 2021))


comps<- decompose_population_change(age_specific_interest_counts = df_care_age, age_specific_population = df_age, count_variable = n, age_variable = age, population_variable = total, time_variable = year, start = 2011)

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

pal<- wesanderson::wes_palette("Darjeeling1")

ggplot(comps)+
  geom_vline(aes(xintercept = 0), linewidth = 0.75)+
  geom_col(aes(x = raw_change, y = component, fill = raw_change>=0), width = 0.5, linewidth = 0.75, color = "black")+
  thm+
  theme(legend.position = "none")+
  scale_fill_manual(values = pal)+
  scale_x_continuous(labels = function(x){format(x, scientific = F, trim = T, big.mark = ",")}, limits = c(-10000000,10000000))+
  labs(x = "Change in count of unpaid carers", title = "Components of Observed Change in Count of Unpaid Carers: 2011 to 2021", caption = "Source: England and Wales Census 2011 and 2021")

ggsave("Plots/Components of Observed Change in Count of Unpaid Carers.png", units = "in", width = 9, height = 5, dpi = 1000)
