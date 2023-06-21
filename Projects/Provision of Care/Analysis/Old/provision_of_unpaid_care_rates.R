####################################
#                                  #
#  PROVISION OF UNPAID CARE RATES  #
#                                  #
####################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 18/06/2023

# DESCRIPTION: This calculates the raw and age-standardised rates of provision of unpaid care in the 2021 census.
# For age standardisation, it uses the 2011 population as a baseline to caclulate directly standardised rates for 2021
# in order to see how rates have changed while accounting for changes in the population's age structure over time.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(systemfonts)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis")
rm(list = ls())


#### STEP 1: CALCULATE RAW RATES FOR 2021 ####


# Read in age-specific care provision counts for 2021
df_age_21<- read.csv("../../../Data/2021/LSOA Data/lsoa_provision_of_unpaid_care_by_age_group_2021_data.csv")

# Rename columns
colnames(df_age_21)<- c("LSOA21CD", "LSOA21NM", "care_code", "care", "age_code", "age", "n")

# Summarise at the level of England and Wales
df_age_21<- df_age_21%>%
  group_by(care, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Summarise across age groups to get an overall count for each hours category
df_hours_21<- df_age_21%>%
  group_by(care)%>%
  summarise(n = sum(n, na.rm = T))

# Merge the "Does not apply" and "Provides no unpaid care" categories
df_hours_21<- df_age_21%>%
  mutate(care = case_when(care == "Does not apply" ~ "Provides no unpaid care",
                          T ~ care))%>%
  group_by(care)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Calculate overall count of people providing any amount of care
df_total_21<- df_hours_21%>%
  mutate(care = case_when(care == "Provides no unpaid care" ~ "Provides no unpaid care",
                          T ~ "Provides unpaid care"))%>%
  group_by(care)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Calculate total population (both overall and split by hours)
df_hours_21<- df_hours_21%>%
  mutate(total = sum(n, na.rm = T))

df_total_21<- df_total_21%>%
  mutate(total = sum(n, na.rm = T))


# Calculate raw proportions of people providing care (both overall and split by hours)
df_hours_21<- df_hours_21%>%
  mutate(proportion = 100*n/total)

df_total_21<- df_total_21%>%
  mutate(proportion = 100*n/total)


#### STEP 2: CALCULATE RAW RATES FOR 2011 ####


# Read in age-specific care provision rates for 2011
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

# Filter out "total" care category
df_age_11<- df_age_11%>%
  filter(!care %in% c("total"))

# Summarise across age groups to get an overall count for each hours category and a count for overall number of people providing any care
df_hours_11<- df_age_11%>%
  group_by(care)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()%>%
  filter(care != "Provides any unpaid care")

df_total_11<- df_age_11%>%
  filter(care %in% c("Provides any unpaid care", "Provides no unpaid care"))%>%
  mutate(care = case_when(care == "Provides any unpaid care" ~ "Provides unpaid care",
                          T ~ care))%>%
  group_by(care)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Calculate total population (both overall and split by hours)
df_hours_11<- df_hours_11%>%
  mutate(total = sum(n, na.rm = T))

df_total_11<- df_total_11%>%
  mutate(total = sum(n, na.rm = T))


# Calculate raw proportions of people providing care (both overall and split by hours)
df_hours_11<- df_hours_11%>%
  mutate(proportion = 100*n/total)

df_total_11<- df_total_11%>%
  mutate(proportion = 100*n/total)


# Rename care categories in 2011 data to the same as those in 2021 data
df_hours_11<- df_hours_11%>%
  mutate(care = str_replace(care, "1 to 19 hours", "19 or less hours"))

df_total_11<- df_total_11%>%
  mutate(care = str_replace(care, "1 to 19 hours", "19 or less hours"))


# Change column names to indicate whether 2011 or 2021 data
colnames(df_hours_21)[2:4]<- paste0(colnames(df_hours_21)[2:4], "_2021")
colnames(df_total_21)[2:4]<- paste0(colnames(df_total_21)[2:4], "_2021")
colnames(df_hours_11)[2:4]<- paste0(colnames(df_hours_11)[2:4], "_2011")
colnames(df_total_11)[2:4]<- paste0(colnames(df_total_11)[2:4], "_2011")

# Join 2011 and 2021 data
df_hours<- left_join(df_hours_21, df_hours_11, by = "care")
df_total<- left_join(df_total_21, df_total_11, by = "care")

# Calculate change between 2011 and 2021
df_hours<- df_hours%>%
  mutate(change = proportion_2021 - proportion_2011)

df_total<- df_total%>%
  mutate(change = proportion_2021 - proportion_2011)


#### STEP 3: PLOT RAW RATES FOR 2021 AND 2011 ####


# Set default theme and colour palette
thm<-  ggthemes::theme_fivethirtyeight()+
  theme(
    text = element_text(family = "Rubik"),
    axis.title.x = element_text(size = rel(0.9), margin = margin(10,0,0,0)),
    plot.title = ggtext::element_textbox_simple(size = rel(1.2)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = rel(0.8), margin = margin(10,0,20,0), lineheight = 1.3),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0),
    panel.grid.minor = element_line(color = "#D2D2D2"),
    strip.text = element_text(face = "bold"))

pal<- wesanderson::wes_palette("GrandBudapest1")

# Create function to label changes as +/- ... %
label_changes<- function(x){
  if (x >= 0){
    return(paste0("+", round(x,2), "%"))
  } else {
    return(paste0(round(x,2), "%"))
  }
}

label_changes<- Vectorize(label_changes)

# Create plot
df_hours%>%
  mutate(type = "By Hours")%>%
  rbind(df_total%>%
          mutate(type = "Overall"))%>%
  mutate(type = factor(type, levels = c("Overall", "By Hours")))%>%
  filter(care != "Provides no unpaid care")%>%
  ggplot()+
  geom_segment(aes(x = proportion_2021, xend = proportion_2011, y = care, yend = care))+
  geom_point(aes(x = proportion_2021, y = care, fill = "2021"), shape = 21, color = "black", size = 3)+
  geom_point(aes(x = proportion_2011, y = care, fill = "2011"), shape = 21, color = "black", size = 3)+
  geom_text(aes(x = (proportion_2021+proportion_2011)/2, y = care, label = label_changes(change)), position = position_nudge(y = 0.3), size = 3)+
  ggforce::facet_col(~type, scales = "free_y", space = "free")+
  thm+
  scale_fill_manual(values = pal, name = "Year")+
  labs(x = "Proportion of population (%)", title = "Raw Provision of Unpaid Care Rates: 2021 Compared to 2011", subtitle = "Rates shown for overall care and care separated by amount provided\nLabels show change between 2011 and 2021", caption = "Source: England and Wales Census 2011 and 2021")

ggsave("Plots/Raw Provision of Unpaid Care Rates.png", unit = "in", width = 9, height = 5, dpi = 1000)


#### STEP 4: CALCULATE AGE-STANDARDISED RATES FOR 2021 ####


# Get age-specific counts both for overall care and care separated by hours
df_age_hours_21<- df_age_21%>%
  mutate(care = case_when(care == "Does not apply" ~ "Provides no unpaid care",
                          T ~ care))%>%
  group_by(care, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

df_age_total_21<- df_age_21%>%
  mutate(care = case_when(care %in% c("Does not apply", "Provides no unpaid care") ~ "Provides no unpaid care",
                          T ~ "Provides unpaid care"))%>%
  group_by(care, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()



# Calculate total population in each age group for 2021
df_age_group_totals_21<- df_age_21%>%
  group_by(age)%>%
  summarise(total = sum(n, na.rm = T))

# Join age totals to age-specific counts
df_age_hours_21<- left_join(df_age_hours_21, df_age_group_totals_21, by = "age")
df_age_total_21<- left_join(df_age_total_21, df_age_group_totals_21, by = "age")

# Calculate age-specific proportions
df_age_hours_21<- df_age_hours_21%>%
  mutate(age_specific_proportion = 100*n/total)

df_age_total_21<- df_age_total_21%>%
  mutate(age_specific_proportion = 100*n/total)



# Read in England and Wales 2011 population age structure
age_11<- read.csv("../../../Data/2011/EW Data/EW_age_by_single_year_2011_data.csv")

# Remove first row with the total population
age_11<- age_11%>%
  filter(Age != "Total")

# Convert age variable to number
age_11<- age_11%>%
  mutate(Age = str_replace(Age, "under 1", "0")%>%
           str_extract("[0-9][0-9]?[0-9]?")%>%
           as.numeric())

# Create variable for age groups
age_11<- age_11%>%
  mutate(age_group = case_when(Age <= 15 ~ "Aged 15 years and under",
                               Age >= 16 & Age <= 24 ~ "Aged 16 to 24 years",
                               Age >= 25 & Age <= 34 ~ "Aged 25 to 34 years",
                               Age >= 35 & Age <= 49 ~ "Aged 35 to 49 years",
                               Age >= 50 & Age <= 64 ~ "Aged 50 to 64 years",
                               Age >= 65 ~ "Aged 65 years and over"))

# Calculate number and proportion in each age group
age_11<- age_11%>%
  group_by(age_group)%>%
  summarise(age_group_n_2011 = sum(n, na.rm = T))%>%
  ungroup()%>%
  mutate(age_group_proportion_2011 = 100*age_group_n_2011/sum(age_group_n_2011, na.rm = T))

# Join 2011 age group proportions to 2021 age-specific rates (for both overall care and care separated by hours)
df_age_hours_21<- left_join(df_age_hours_21, age_11, by = c("age" = "age_group"))
df_age_total_21<- left_join(df_age_total_21, age_11, by = c("age" = "age_group"))

# Calculate weighted age-specific proportions
df_age_hours_21<- df_age_hours_21%>%
  mutate(weighted_age_specific_proportion = age_specific_proportion*age_group_proportion_2011)

df_age_total_21<- df_age_total_21%>%
  mutate(weighted_age_specific_proportion = age_specific_proportion*age_group_proportion_2011)

# Calculate age-standardised proportions
df_standardised_hours_21<- df_age_hours_21%>%
  group_by(care)%>%
  summarise(age_standardised_proportion = sum(weighted_age_specific_proportion, na.rm = T)/100)%>%
  ungroup()

df_standardised_total_21<- df_age_total_21%>%
  group_by(care)%>%
  summarise(age_standardised_proportion = sum(weighted_age_specific_proportion, na.rm = T)/100)%>%
  ungroup()

# Join age-standardised 2021 rates to raw rates
df_hours<- left_join(df_hours, df_standardised_hours_21, by = "care")
df_total<- left_join(df_total, df_standardised_total_21, by = "care")

# Calculate age-standardised change
df_hours<- df_hours%>%
  mutate(age_standardised_change = age_standardised_proportion - proportion_2011)

df_total<- df_total%>%
  mutate(age_standardised_change = age_standardised_proportion - proportion_2011)


#### STEP 5:PLOT AGE-STANDARDISED RATES FOR 2021 AGAINST RAW RATES FOR 2011 ####


# Create plot
df_hours%>%
  mutate(type = "By Hours")%>%
  rbind(df_total%>%
          mutate(type = "Overall"))%>%
  mutate(type = factor(type, levels = c("Overall", "By Hours")))%>%
  filter(care != "Provides no unpaid care")%>%
  ggplot()+
  geom_segment(aes(x = age_standardised_proportion, xend = proportion_2011, y = care, yend = care))+
  geom_point(aes(x = age_standardised_proportion, y = care, fill = "2021"), shape = 21, color = "black", size = 3)+
  geom_point(aes(x = proportion_2011, y = care, fill = "2011"), shape = 21, color = "black", size = 3)+
  geom_text(aes(x = (age_standardised_proportion+proportion_2011)/2, y = care, label = label_changes(age_standardised_change)), position = position_nudge(y = 0.4), size = 3)+
  ggforce::facet_col(~type, scales = "free_y", space = "free")+
  thm+
  scale_fill_manual(values = pal, name = "Year")+
  labs(x = "Proportion of population (%)", title = "Age-Standardised Rate of Provision of Unpaid Care: 2021 Compared to 2011", subtitle = "Rates shown for overall care and care separated by amount provided\nLabels show change between 2011 and 2021", caption = "Source: England and Wales Census 2011 and 2021")

ggsave("Plots/Age-Standardised Provision of Unpaid Care Rates.png", unit = "in", width = 9, height = 5, dpi = 1000)

# Create table
library(gt)

tbl<- df_total%>%
  select(care, proportion_2011, proportion_2021, age_standardised_proportion)%>%
  rbind(df_hours%>%
          select(care, proportion_2011, proportion_2021, age_standardised_proportion))%>%
  filter(care != "Provides no unpaid care")%>%
  gt(rowname_col = "care")%>%
  tab_row_group(rows = c(1), label = md("**Overall**"))%>%
  tab_row_group(rows = c(2:4), label = md("**By Hours**"))%>%
  row_group_order(groups = c("**Overall**", "**By Hours**"))%>%
  fmt_number(decimal = 2)%>%
  cols_label(proportion_2011 = md("**2011**"),
             proportion_2021 = md("**2021**"),
             age_standardised_proportion = html("<strong>2021<br>Age-Standardised</strong>"))%>%
  tab_spanner(label = md("**Proportion of population (%)**"), columns = everything())%>%
  tab_header(title = md("**Provision of Unpaid Care Rates in 2011 and 2021**"))%>%
  tab_footnote(md("_Source: England and Wales Census 2011 and 2021  
                  Standardised 2021 rate is standardised to 2011 age structure_"))%>%
  tab_stub_indent(rows = everything(), indent = 5)%>%
  opt_table_font(font = "Rubik")%>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  )%>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  )%>%
  cols_width(c(2:4) ~ px(160),
             1 ~ px(200))

gtsave(tbl, "Tables/Provision of Unpaid Care Rates 2011 and 2021.html")
save(tbl, file = "Tables/Provision of Unpaid Care Rates 2011 and 2021.Rda")


