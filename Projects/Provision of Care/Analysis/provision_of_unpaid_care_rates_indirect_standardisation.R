####################################
#                                  #
#  PROVISION OF UNPAID CARE RATES  #
#                                  #
####################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 18/06/2023

# DESCRIPTION: This calculates the raw and age-standardised rates of provision of unpaid care in the 2021 census.
# For age standardisation, it uses the 2011 population as a baseline to caclulate indirectly standardised ratios for 2021
# in order to see how rates have changed while accounting for changes in the population's age structure over time.

library(nomisr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(systemfonts)
library(gt)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis")
rm(list = ls())
source("demography_functions.R")


#### STEP 1: CALCULATE RAW RATES FOR 2021 ####


# Read in age-specific care provision counts for 2021
df_age_21<- read.csv("../../../Data/2021/EW Data/EW_provision_of_unpaid_care_by_age_group_2021_data.csv")

# Rename columns
colnames(df_age_21)<- c("country_code", "country_name", "care_code", "care", "age_code", "age", "n")

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
  mutate(total_population_21 = sum(n, na.rm = T))

df_total_21<- df_total_21%>%
  mutate(total_population_21 = sum(n, na.rm = T))


# Calculate raw proportions of people providing care (both overall and split by hours)
df_hours_21<- df_hours_21%>%
  mutate(proportion = 100*n/total_population_21)

df_total_21<- df_total_21%>%
  mutate(proportion = 100*n/total_population_21)


#### STEP 2: CALCULATE RAW RATES FOR 2011 ####


# Download 2011 carer age data from Nomis API (for both overall and separated by hours)
id<- nomis_search(name = "*DC3303EWr*")$id[1]

df_total_age_11<- nomis_get_data(id = id, geography = "TYPE499", C_CARER = 2, C_AGE = 1:21, C_SEX = 0, C_HEALTH = 0, tidy = T)%>%
  select(geography_name, c_carer_name, c_age_name, obs_value)%>%
  filter(geography_name == "England and Wales")

df_hours_age_11<- nomis_get_data(id = id, geography = "TYPE499", C_CARER = 3:5, C_AGE = 1:21, C_SEX = 0, C_HEALTH = 0, tidy = T)%>%
  select(geography_name, c_carer_name, c_age_name, obs_value)%>%
  filter(geography_name == "England and Wales")


# Rename columns
colnames(df_total_age_11)<- c("geography", "care", "age", "n")
colnames(df_hours_age_11)<- c("geography", "care", "age", "n")

# Get total age-group population for 2011
total_age_population_11<- nomis_get_data(id = id, geography = "TYPE499", C_CARER = 0, C_AGE = 1:21, C_SEX = 0, C_HEALTH = 0, tidy = T)%>%
  select(geography_name, c_carer_name, c_age_name, obs_value)%>%
  filter(geography_name == "England and Wales")%>%
  rename(age = c_age_name,
         total_age_group_population_11 = obs_value)%>%
  select(age, total_age_group_population_11)


# Join total age-specific population to carer counts (both overall and split by hours)
df_total_age_11<- left_join(df_total_age_11, total_age_population_11, by = "age")
df_hours_age_11<- left_join(df_hours_age_11, total_age_population_11, by = "age")


# Calculate overall raw rates for 2011
df_total_11<- df_total_age_11%>%
  group_by(care)%>%
  summarise(n = sum(n, na.rm = T), total_population_11 = sum(total_age_group_population_11, na.rm = T))%>%
  mutate(proportion = 100*n/total_population_11)

df_hours_11<- df_hours_age_11%>%
  group_by(care)%>%
  summarise(n = sum(n, na.rm = T), total_population_11 = sum(total_age_group_population_11, na.rm = T))%>%
  mutate(proportion = 100*n/total_population_11)

# Change care categories in 2011 data to match 2021 data
df_total_11<- df_total_11%>%
  mutate(care = "Provides unpaid care")

df_hours_11<- df_hours_11%>%
  mutate(care = case_when(care == "Provides 1 to 19 hours unpaid care a week" ~ "Provides 19 or less hours unpaid care a week",
                          T ~ care))


#### STEP 3: PLOT RAW RATES FOR 2011 AND 2021 ####


# Change column names in 2021 and 2011 data so that they can be distinguished when joined
colnames(df_total_21)[c(2,4)]<- paste0(colnames(df_total_21)[c(2,4)], "_21")
colnames(df_hours_21)[c(2,4)]<- paste0(colnames(df_hours_21)[c(2,4)], "_21")
colnames(df_total_11)[c(2,4)]<- paste0(colnames(df_total_11)[c(2,4)], "_11")
colnames(df_hours_11)[c(2,4)]<- paste0(colnames(df_hours_11)[c(2,4)], "_11")

# Join raw 2021 rates and raw 2011 rates
raw_total<- left_join(df_total_11, df_total_21, by = "care")
raw_hours<- left_join(df_hours_11, df_hours_21, by = "care")

# Calculate change in raw proportions
raw_total<- raw_total%>%
  mutate(change = proportion_21 - proportion_11)

raw_hours<- raw_hours%>%
  mutate(change = proportion_21 - proportion_11)


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
raw_hours%>%
  mutate(type = "By Hours")%>%
  rbind(raw_total%>%
          mutate(type = "Overall"))%>%
  mutate(type = factor(type, levels = c("Overall", "By Hours")))%>%
  filter(care != "Provides no unpaid care")%>%
  ggplot()+
  geom_segment(aes(x = proportion_21, xend = proportion_11, y = care, yend = care))+
  geom_point(aes(x = proportion_21, y = care, fill = "2021"), shape = 21, color = "black", size = 3)+
  geom_point(aes(x = proportion_11, y = care, fill = "2011"), shape = 21, color = "black", size = 3)+
  geom_text(aes(x = (proportion_21+proportion_11)/2, y = care, label = label_changes(change)), position = position_nudge(y = 0.4), size = 3)+
  ggforce::facet_col(~type, scales = "free_y", space = "free")+
  thm+
  scale_fill_manual(values = pal, name = "Year")+
  labs(x = "Proportion of population (%)", title = "Raw Proportion of Population Providing Unpaid Care: 2011 and 2021", subtitle = "Rates shown for overall care and care separated by amount provided\nLabels show change between 2011 and 2021", caption = "Source: England and Wales Census 2011 and 2021")

ggsave("Plots/Raw Provision of Unpaid Care Rates 2021 and 2011.png", unit = "in", width = 9, height = 5, dpi = 1000)


#### STEP 4: CALCULATE AGE-STANDARDISED CARING RATE FOR 2021 ####


# Calculate raw age-specific rate of people providing care in 2011 (both overall and split by hours)
df_hours_age_11<- df_hours_age_11%>%
  mutate(age_specific_rate = n/total_age_group_population_11)

df_total_age_11<- df_total_age_11%>%
  mutate(age_specific_rate = n/total_age_group_population_11)


# Rename care categories in 2011 data to the same as those in 2021 data
df_hours_age_11<- df_hours_age_11%>%
  mutate(care = str_replace(care, "1 to 19 hours", "19 or less hours"))

# Read in the 2021 age group counts
total_age_population_21<- read.csv("../../../Data/2021/EW Data/EW_age_by_single_year_2021_data.csv")

# Select and rename relevant columns
colnames(total_age_population_21)[4:5]<- c("age", "total_age_group_population_21")
total_age_population_21<- total_age_population_21%>%
  select(age, total_age_group_population_21)

# Recode the 2021 age groups to the 2011 age groups
total_age_population_21<- total_age_population_21%>%
  mutate(age = collapse_age_groups(age, target_groups = df_total_age_11$age))%>%
  group_by(age)%>%
  summarise(total_age_group_population_21 = sum(total_age_group_population_21, na.rm = T))%>%
  ungroup()

# Join the 2021 age-group populations to the 2011 age-specific rates
df_total_age_11<- left_join(df_total_age_11, total_age_population_21, by = "age")
df_hours_age_11<- left_join(df_hours_age_11, total_age_population_21, by = "age")

# Calculate the expected count of carers within each age group if the 2011 rate were applied to the 2021 population
df_total_age_11<- df_total_age_11%>%
  mutate(expected_count = age_specific_rate*total_age_group_population_21)

df_hours_age_11<- df_hours_age_11%>%
  mutate(expected_count = age_specific_rate*total_age_group_population_21)

# Calculate the overall expected count
df_total_expected<- df_total_age_11%>%
  summarise(care = "Provides unpaid care", expected_count = sum(expected_count, na.rm = T))

df_hours_expected<- df_hours_age_11%>%
  group_by(care)%>%
  summarise(expected_count = sum(expected_count, na.rm = T))%>%
  ungroup()


# Join expected counts to actual 2021 counts
df_total_21<- left_join(df_total_21, df_total_expected, by = "care")
df_hours_21<- left_join(df_hours_21, df_hours_expected, by = "care")

# Calculate standardised rate (ie: ovserved/expected)
df_total_21<- df_total_21%>%
  mutate(standardised_caring_rate = n_21/expected_count)

df_hours_21<- df_hours_21%>%
  mutate(standardised_caring_rate = n_21/expected_count)



#### STEP 5: PLOT 2021 AGE-STANDARDISED CARING RATIO ####


# Create plot
df_hours_21%>%
  mutate(type = "By Hours")%>%
  rbind(df_total_21%>%
          mutate(type = "Overall"))%>%
  mutate(type = factor(type, levels = c("Overall", "By Hours")))%>%
  filter(care != "Provides no unpaid care")%>%
  ggplot()+
  geom_segment(aes(x = 1, xend = standardised_caring_rate, y = care, yend = care))+
  geom_point(aes(x = standardised_caring_rate, y = care), fill = pal[1], shape = 21, color = "black", size = 3)+
  geom_vline(aes(xintercept = 1), linetype = 2)+
  ggforce::facet_col(~type, scales = "free_y", space = "free")+
  thm+
  labs(x = "Age-Standardised Caring Rate", title = "Age-Standardised Caring Rate: 2021 Compared to 2011", subtitle = "Rates shown for overall care and care separated by amount provided", caption = "Source: England and Wales Census 2011 and 2021")


ggsave("Plots/Age-Standardised Caring Rate 2021.png", unit = "in", width = 9, height = 5, dpi = 1000)


#### STEP 6: CREATE SUMMARY TABLE SHOWING RAW AND AGE-STANDARDISED RATES ####


# Join 2011 raw rates with 2021 raw rates and age-standardised rates
df_total<- left_join(df_total_11, df_total_21%>%
                       filter(care != "Provides no unpaid care"), by = "care")
df_hours<- left_join(df_hours_11, df_hours_21%>%
                       filter(care != "Provides no unpaid care"), by = "care")

tbl1<- df_total%>%
  select(care, proportion_11, proportion_21, standardised_caring_rate)%>%
  rbind(df_hours%>%
          select(care, proportion_11, proportion_21, standardised_caring_rate))%>%
  gt(rowname_col = "care")%>%
  tab_row_group(rows = c(1), label = md("**Overall**"))%>%
  tab_row_group(rows = c(2:4), label = md("**By Hours**"))%>%
  row_group_order(groups = c("**Overall**", "**By Hours**"))%>%
  fmt_number(decimal = 2)%>%
  cols_label(proportion_11 = md("**2011 Raw Proportion (%)**"),
             proportion_21 = md("**2021 Raw Proportion (%)**"),
             standardised_caring_rate = md("**2021 Age-Standardised Caring Rate**"))%>%
  tab_header(title = md("**Raw and Age-Standardised Unpaid Caring Rates in 2011 and 2021**"))%>%
  tab_footnote(md("_Source: England and Wales Census 2011 and 2021_"))%>%
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

tbl1

gtsave(tbl1, "Tables/Raw and Age-Standardised Unpaid Caring Rates in 2011 and 2021.html")
save(tbl1, file = "Tables/Raw and Age-Standardised Unpaid Caring Rates in 2011 and 2021.Rda")





#### STEP 7: CREATE TABLES SHOWING CALCULATIONS ####


# Table of 2011 age-sepcific rates
tbl2<- df_total_age_11%>%
  select(age, n, total_age_group_population_11, age_specific_rate)%>%
  gt(rowname_col = "age")%>%
  fmt_number(columns = "age_specific_rate", decimals = 3)%>%
  cols_label(n = md("**2011<br>Count of Carers**"),
             total_age_group_population_11 = md("**2011<br>Age-Group Population**"),
             age_specific_rate = md("**2011<br>Age-Specific Rate**"))%>%
  tab_header(title = md("**2011 Age-Specific Caring Rates**"))%>%
  tab_footnote(md("_Source: England and Wales Census 2011_"))%>%
  opt_table_font(font = "Rubik")%>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  )%>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  )%>%
  cols_width(gt::everything() ~ px(220))

gtsave(tbl2, "Tables/2011 Age-Specific Caring Rates.html")
save(tbl2, file = "Tables/2011 Age-Specific Caring Rates.Rda")



# Table of expected counts based on 2011 rates and 2021 population
tbl3<- df_total_age_11%>%
  select(age, age_specific_rate, total_age_group_population_21, expected_count)%>%
  gt(rowname_col = "age")%>%
  grand_summary_rows(columns = c("total_age_group_population_21", "expected_count"), fns = Total ~ sum(.))%>%
  fmt_number(columns = "age_specific_rate", decimals = 3)%>%
  fmt_number(columns = "expected_count", decimals = 0, sep_mark = "")%>%
  cols_label(age_specific_rate = md("**2011<br>Age-Specific Rate**"),
             total_age_group_population_21 = md("**2021<br>Age-Group Population**"),
             expected_count = md("**2021<br>Expected Count of Carers**"))%>%
  tab_header(title = md("**2021 Expected Count of Carers**"))%>%
  tab_footnote(md("_Source: England and Wales Census 2011_"))%>%
  opt_table_font(font = "Rubik")%>%
  tab_style(
    style = cell_text(align = "center"),
    locations = list(cells_body(), cells_column_labels(), cells_grand_summary())
  )%>%
  cols_width(gt::everything() ~ px(220))


gtsave(tbl3, "Tables/2021 Expected Count of Carers.html")
save(tbl3, file = "Tables/2021 Expected Count of Carers.Rda")

