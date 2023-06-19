####################################
#                                  #
#  CARER SOCIODEMOGRAPHIC PROFILE  #
#                                  #
####################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 14/06/2023

# DESCRIPTION: This file looks at the socio-demographic profile of those provding unpaid care at
# the 2021 census. It then compares this to the profile of carers in 2011.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(systemfonts)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis")
rm(list = ls())


#### STEP 1: AGE ####


# Read in care provision by age data
df_age<- read.csv("../../../Data/2021/LSOA Data/lsoa_provision_of_unpaid_care_by_age_group_2021_data.csv")

# Rename columns
colnames(df_age)<- c("LSOA21CD", "LSOA21NM", "care_code", "care", "age_code", "age", "n")

# Summarise at the level of England and Wales
df_age<- df_age%>%
  group_by(care, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()


# Calculate proportions within each age group
df_age<- df_age%>%
  group_by(age)%>%
  mutate(proportion = 100*n/sum(n, na.rm = T))


# Calculate total number and proportion providing any amount of care within each age group
df_age<- df_age%>%
  filter(!care %in% c("Does not apply", "Provides no unpaid care"))%>%
  group_by(age)%>%
  summarise(n = sum(n, na.rm = T), proportion = sum(proportion, na.rm = T))%>%
  mutate(care = "Provides unpaid care")%>%
  select(care, age, n, proportion)%>%
  rbind(df_age)


# Remove extraneous text from categories
df_age<- df_age%>%
  mutate(care = str_remove(care, "Provides (?=[0-9])")%>%
                str_remove(" unpaid care a week"))


# Set default theme and colour palette
thm<-  ggthemes::theme_fivethirtyeight()+
       theme(
        text = element_text(family = "Rubik"),
        axis.title.x = element_text(size = rel(0.9), margin = margin(10,0,0,0)),
        plot.title = ggtext::element_textbox(size = rel(1.2)),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = rel(0.9), margin = margin(0,0,20,0)),
        panel.grid.minor = element_line(color = "#D2D2D2"),
        strip.text = element_text(face = "bold"))

pal<- wesanderson::wes_palette("GrandBudapest1")

# Plot overall proportions
df_age%>%
  filter(care == "Provides unpaid care")%>%
  ggplot()+
    geom_linerange(aes(xmin = 0, xmax = proportion, y = age),color = "black")+
    geom_point(aes(x = proportion, y = age), fill = pal[2], color = "black", shape = 21, size = 3)+
    #scale_x_continuous(expand = expansion(mult = c(0.1,0.5)))+
    thm +
    labs(x = "Proportion providing unpaid care (%)", title = "Provision of Unpaid Care by Age in 2021", subtitle = "Source: England and Wales Census 2021")


# Plot proportions separated by intensity
df_age%>%
  filter(!care %in% c("Provides unpaid care", "Does not apply", "Provides no unpaid care"))%>%
  ggplot(aes(group = care))+
  geom_linerange(aes(ymin = 0, ymax = proportion, x = age), color = "black", position = position_dodge(width = 0.4))+
  geom_point(aes(y = proportion, x = age,  fill = care), color = "black", shape = 21, size = 3, position = position_dodge2(width = 0.4))+
  scale_fill_manual(values = pal, name = "Intensity (hours/week)")+
  thm +
  labs(x = "Proportion providing unpaid care (%)", title = "Provision of Unpaid Care by Age and Intensity", subtitle = "Source: England and Wales Census 2021")+
  coord_flip()

# Read in care provision by age data for 2011
df_age_11<- read.csv("../../../Data/2011/LSOA Data/lsoa_provision_of_unpaid_care_by_age_group_2011_data.csv")

# Rename columns
colnames(df_age_11)[1:3]<- c("LSOA21CD", "LSOA21NM",  "care")

# Pivot to long format
df_age_11<- df_age_11%>%
  pivot_longer(cols = Age.0.to.15:Age.65.and.over, names_to = "age", values_to = "n")

# Summarise at the level of England and Wales
df_age_11<- df_age_11%>%
  group_by(care, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Filter out total and other_total care categories
df_age_11<- df_age_11%>%
  filter(!care %in% c("total", "other_total"))

# Calculate proportions within each age group
df_age_11<- df_age_11%>%
  group_by(age)%>%
  mutate(proportion = 100*n/sum(n, na.rm = T))

# Calculate total number and proprtion providing any amount of care within each age group
df_age_11<- df_age_11%>%
  filter(!care %in% c("Does not apply", "Provides no unpaid care"))%>%
  group_by(age)%>%
  summarise(n = sum(n, na.rm = T), proportion = sum(proportion, na.rm = T))%>%
  mutate(care = "Provides unpaid care")%>%
  select(care, age, n, proportion)%>%
  rbind(df_age_11)

# Remove extraneous text from categories
df_age_11<- df_age_11%>%
  mutate(care = str_remove(care, "Provides (?=[0-9])")%>%
           str_remove(" unpaid care a week"))

# Convert the care intensity categories to the same as those in the 2021 data
df_age_11<- df_age_11%>%
  mutate(care = str_replace(care, "1 to 19 hours", "19 or less hours"))

# Convert age categories to the same as those in the 2021 data
df_age_11<- df_age_11%>%
  mutate(age = str_replace_all(age, "\\.", " ")%>%
               str_replace("Age", "Aged")%>%
               str_replace("0 to 15", "15 years and under")%>%
               str_replace("65 and over", "65 years and over")%>%
               str_replace("([0-9][0-9]?) to ([0-9][0-9]?)", "\\1 to \\2 years"))

# Join the 2011 and 2021 data
colnames(df_age)[3:4]<- c("n_2021", "proportion_2021")
colnames(df_age_11)[3:4]<- c("n_2011", "proportion_2011")
df_age<- left_join(df_age, df_age_11, by = c("age", "care"))

# Calculate change in provision rates between 2011 and 2021 for each age group
df_age<- df_age%>%
  mutate(change = proportion_2021 - proportion_2011)


# Plot the change in overall provision rates
df_age%>%
  filter(care == "Provides unpaid care")%>%
  ggplot(aes(group = care))+
  geom_linerange(aes(xmin = proportion_2011, xmax = proportion_2021, y = age), color = "black")+
  geom_point(aes(x = proportion_2011, y = age, fill = "2011"), shape = 21, size = 3)+
  geom_point(aes(x = proportion_2021, y = age, fill = "2021"), shape = 21, size = 3)+
  geom_text(aes(x = (proportion_2021 + proportion_2011)/2, y = age, label = paste0(round(change,1), "%")), size = 3, family = "Rubik", position = position_nudge(y = 0.3))+
  scale_fill_manual(values = pal, name = "Year")+
  thm +
  labs(x = "Proportion providing unpaid care (%)", title = "Provision of Unpaid Care by Age in 2011 and 2021", subtitle = "Source: England and Wales Census 2011 and 2021. Labels are change between 2011 and 2021.")

ggsave("Plots/Provision of Unpaid Care by Age in 2011 and 2021.png", width = 9, height = 5, units = "in", dpi = 1000)

df_age%>%
  filter(!care %in% c("Provides unpaid care", "Does not apply", "Provides no unpaid care"))%>%
  ggplot(aes(group = care))+
  geom_linerange(aes(xmin = proportion_2011, xmax = proportion_2021, y = age), color = "black")+
  geom_point(aes(x = proportion_2011, y = age, fill = "2011"), shape = 21, size = 3)+
  geom_point(aes(x = proportion_2021, y = age, fill = "2021"), shape = 21, size = 3)+
  geom_text(aes(x = (proportion_2021 + proportion_2011)/2, y = age, label = paste0(round(change,1), "%")), size = 3, family = "Rubik", position = position_nudge(y = 0.3))+
  facet_wrap(~care)+
  scale_fill_manual(values = pal, name = "Year")+
  thm +
  labs(x = "Proportion providing unpaid care (%)", title = "Provision of Unpaid Care by Age and Intensity in 2011 and 2021", subtitle = "Source: England and Wales Census 2011 and 2021. Labels are change between 2011 and 2021.")

ggsave("Plots/Provision of Unpaid Care by Age and Intensity in 2011 and 2021.png", width = 9, height = 5, units = "in", dpi = 1000)
