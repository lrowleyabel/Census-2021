#######################################
#                                     #
#  LOCAL AUTHORITY SPENDING ANALYSIS  #
#                                     #
#######################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 17/07/2023

# DESCRIPTION: This file calculates rates of provision of unpaid care by Local Authority in 2021 and then looks at whether the change in rates since 2011 is associated with the change in social care
# spending in each Local Authority.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(sociodemographics)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis/Geographic Analysis")
rm(list = ls())


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




#### Step 1: 2021 Rates ####


# Read in care data by LA for 2021
df_care<- read.csv("../../../../Data/2021/LAD Data/LAD_provision_of_unpaid_care_2021_data.csv")

# Select and rename variables
df_care<- df_care%>%
  rename(LAD21CD = Lower.tier.local.authorities.Code,
         LAD21NM = Lower.tier.local.authorities,
         care = Unpaid.care..5.categories.,
         n = Observation)%>%
  select(LAD21CD, LAD21NM, care, n)


# Drop Wales
df_care<- df_care%>%
  filter(substr(LAD21CD,1,1) != "W")

# Merge "Does not apply" and "Provides no unpaid care" categories
df_care<- df_care%>%
  mutate(care = case_when(care == "Does not apply" ~ "Provides no unpaid care",
                          T ~ care))%>%
  group_by(LAD21CD, care)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Calculate proportion in each care category
df_care<- df_care%>%
  group_by(LAD21CD)%>%
  mutate(total = sum(n, na.rm = T))%>%
  ungroup()%>%
  mutate(proportion = 100*n/total)

# Pivot to wide format
df_care<- df_care%>%
  pivot_wider(id_cols = LAD21CD, names_from = care, values_from = c(proportion, n))

# Rename variables
colnames(df_care)<- colnames(df_care)%>%
  str_replace("Provides 19 or less hours unpaid care a week", "care_19_less")%>%
  str_replace("Provides 20 to 49 hours unpaid care a week", "care_20_49")%>%
  str_replace("Provides 50 or more hours unpaid care a week", "care_50_plus")%>%
  str_replace("Provides no unpaid care", "care_none")

# Calculate proportion providing any amount of care
df_care<- df_care%>%
  mutate(n_care = n_care_19_less + n_care_20_49 + n_care_50_plus,
         proportion_care = proportion_care_19_less + proportion_care_20_49 + proportion_care_50_plus)



#### Step 2: 2011 Rates ####


# Read in care data by LAD for 2011
df_care_11<- read.csv("../../../../Data/2011/LAD Data/LAD_provision_of_unpaid_care_2011_data.csv")

# Rename and select variables
df_care_11<- df_care_11%>%
  rename(LAD13CD = mnemonic,
         LAD13NM = local.authority..district...unitary..prior.to.April.2015.,
         care_19_less = Provides.1.to.19.hours.unpaid.care.a.week,
         care_20_49 = Provides.20.to.49.hours.unpaid.care.a.week,
         care_50_plus = Provides.50.or.more.hours.unpaid.care.a.week,
         care_none = Provides.no.unpaid.care)%>%
  select(LAD13CD, LAD13NM, care_19_less, care_20_49, care_50_plus, care_none)


# Drop Wales
df_care_11<- df_care_11%>%
  filter(substr(LAD13CD,1,1) != "W")

# Get lookup between 2013 and 2021 LADs
lad_lookup<- across_yr_lookup(2013, 2021, geog = "LAD")%>%
  select(LAD13CD, LAD21CD)

# Check for LAD codes missing from the lookup
table(df_care_11$LAD13CD %in% lad_lookup$LAD13CD)

table(df_care$LAD21CD %in% lad_lookup$LAD21CD)

# The lookup is missing two changes:
#   - LAD13CD E07000190 became LAD21CD E07000246
#   - LAD13CD E07000191 became LAD21CD E07000246

# Amend the lookup to relfect these changes
lad_lookup$LAD21CD[lad_lookup$LAD13CD=="E07000190"]<- "E07000246"
lad_lookup$LAD21CD[lad_lookup$LAD13CD=="E07000191"]<- "E07000246"

table(df_care$LAD21CD %in% lad_lookup$LAD21CD)

# Check if LAD codes are unique, or if they megred/split
lad_lookup$LAD13CD[duplicated(lad_lookup$LAD13CD)]
lad_lookup$LAD21CD[duplicated(lad_lookup$LAD21CD)]

duplicated_LAD21s<- lad_lookup$LAD21CD[duplicated(lad_lookup$LAD21CD)]

lad_lookup$LAD13CD[lad_lookup$LAD21CD %in% duplicated_LAD21s]

# 25 LAD13s were merged into 12 LAD21s

# Attach the LAD21s to the 2011 data
df_care_11<- left_join(df_care_11, lad_lookup, by = "LAD13CD")

# Sum counts across merged LAD13s
df_care_11<- df_care_11%>%
  group_by(LAD21CD)%>%
  summarise(care_19_less = sum(care_19_less, na.rm = T),
            care_20_49 = sum(care_20_49, na.rm = T),
            care_50_plus = sum(care_50_plus, na.rm = T),
            care_none = sum(care_none, na.rm = T))%>%
  ungroup()

# Pivot to long format
df_care_11<- df_care_11%>%
  pivot_longer(cols = care_19_less:care_none, names_to = "care", values_to = "n")

# Calculate proportion in each care category
df_care_11<- df_care_11%>%
  group_by(LAD21CD)%>%
  mutate(total = sum(n, na.rm = T))%>%
  ungroup()%>%
  mutate(proportion = 100*n/total)

# Pivot to wide format
df_care_11<- df_care_11%>%
  pivot_wider(id_cols = LAD21CD, names_from = care, values_from = c(proportion, n))

# Calculate proportion providing any amount of care
df_care_11<- df_care_11%>%
  mutate(n_care = n_care_19_less + n_care_20_49 + n_care_50_plus,
         proportion_care = proportion_care_19_less + proportion_care_20_49 + proportion_care_50_plus)


#### Step 3: Calculate Raw Change ####


# Rename 2011 variables to distinguish from 2021 variables
colnames(df_care_11)[2:11]<- colnames(df_care_11)[2:11]%>%
  paste0("_2011")

# Join 2011 data to 2021 data
df<- left_join(df_care, df_care_11, by = "LAD21CD")

# Calculate raw change
df<- df%>%
  mutate(care_change = proportion_care - proportion_care_2011,
         care_19_less_change = proportion_care_19_less - proportion_care_19_less_2011,
         care_20_49_change = proportion_care_20_49 - proportion_care_20_49_2011,
         care_50_plus_change = proportion_care_50_plus - proportion_care_50_plus_2011)


#### Step 4: Analyse Raw Care Change and Spending Change ####


# Read in spending data
spending<- read.csv("../../../../Data/2018/LA Data/LA_adult_social_care_expenditure_2011_to_2018_data.csv")

# Get lookup between 2018 and 2021 LADs
lad_lookup<- across_yr_lookup(2018, 2021, geog = "LAD")%>%
  select(LAD18CD, LAD21CD)

# Check for missing LAD18s
table(spending$LTLA18CD %in% lad_lookup$LAD18CD)
table(df$LAD21CD %in% lad_lookup$LAD21CD)

# The lookup is missing two changes:
#   - LAD18CD E07000190 became LAD21CD E07000246
#   - LAD18CD E07000191 became LAD21CD E07000246

# Amend the lookup to relfect these changes
lad_lookup$LAD21CD[lad_lookup$LAD18CD=="E07000190"]<- "E07000246"
lad_lookup$LAD21CD[lad_lookup$LAD18CD=="E07000191"]<- "E07000246"

table(df$LAD21CD %in% lad_lookup$LAD21CD)

# Join LAD21 to spending data
spending<- left_join(spending, lad_lookup, by = c("LTLA18CD" = "LAD18CD"))

# Sum spending amount and population across merged LADs
spending<- spending%>%
  group_by(LAD21CD, Year)%>%
  summarise(population = sum(Pop_Est_Adult, na.rm = T),
            nominal_spend = sum(Adult_Social_Care_GrossExpen, na.rm = T),
            inflation_rate = mean(Inflation.Ratio.to.2018))%>%
  ungroup()

# Calculate real-terms spend
spending<- spending%>%
  mutate(real_spend = nominal_spend*inflation_rate)

# Calculate real-terms spend per capita
spending<- spending%>%
  mutate(real_spend_pc = real_spend/population)


# Plot change in spending over time by LA
spending%>%
  filter(real_spend_pc<1)%>%
  mutate(Year = as.factor(Year))%>%
  ggplot()+
    geom_boxplot(aes(x = Year, y = real_spend_pc))+
    thm

# Plot change in mean spending over time
spending%>%
  group_by(Year)%>%
  summarise(real_spend = 1000*mean(real_spend_pc, na.rm = T))%>%
  ggplot()+
  geom_point(aes(x = Year, y = real_spend),  color = pal[2])+
  geom_line(aes(x = Year, y = real_spend), linewidth = 1, color = pal[2])+
  scale_y_continuous()+
  thm+
  theme(axis.title.y = element_text(margin = margin(0,10,0,0)))+
  labs(x = "Year", y = "Mean spending\n(£s per person aged 18+)", title = "Mean Gross Adult Social Care Spending per Adult across English Local Authorities: 2011 - 2018", subtitle = "Spending expressed in real terms adjusted to 2018 pounds", caption = "Source: Place-Based Longitudinal Data Resource")

ggsave("Plots/Gross Adult Social Care Spending by All English Local Authorities 2011 - 2018.png", units = "in", width = 9, height = 5, dpi = 1000)

# Pivot to wide format
spending<- spending%>%
  pivot_wider(id_cols = LAD21CD, names_from = Year, values_from = real_spend_pc, names_prefix = "spending_year_")

# Calculate 2011 to 2015 change
spending<- spending%>%
  mutate(spending_change_11_15 = spending_year_2015 - spending_year_2011)%>%
  mutate(spending_change_11_15_prop = spending_year_2015/spending_year_2011)

# Calculate 2011 to 2018 change
spending<- spending%>%
  mutate(spending_change_11_18 = spending_year_2018 - spending_year_2011)%>%
  mutate(spending_change_11_18_prop = spending_year_2018/spending_year_2011)


# Look at distribution of spending change from 2010 to 2013
ggplot(spending)+
  geom_histogram(aes(x = spending_change_11_15))

ggplot(spending)+
  geom_histogram(aes(x = spending_change_11_15_prop))

# Look at distribution of spending change from 2010 to 2018
ggplot(spending)+
  geom_histogram(aes(x = spending_change_11_18))

ggplot(spending)+
  geom_histogram(aes(x = spending_change_11_18_prop))


# Join spending data to care data
df<- left_join(df, spending%>%
                 select(LAD21CD, spending_year_2011, spending_year_2015, spending_year_2018, spending_change_11_15, spending_change_11_15_prop, spending_change_11_18, spending_change_11_18_prop), by = "LAD21CD")

# Plot spending change against care change
df%>%
  ggplot()+
    geom_point(aes(x = spending_change_11_15, y = care_change))+
    geom_smooth(aes(x = spending_change_11_15, y = care_change))

# Remove the outlier 
df%>%
  filter(spending_change_11_15>-0.3)%>%
  ggplot()+
  geom_point(aes(x = 1000*spending_change_11_15, y = care_change), size = 1, color = pal[3], fill = pal[2], shape = 21)+
  geom_smooth(aes(x = 1000*spending_change_11_15, y = care_change), color = pal[3])+
  geom_vline(xintercept = 0, linetype = 2)+
  thm+
  theme(axis.title.y =element_text(margin = margin(0,10,0,0)),
        plot.caption = element_text(margin = margin(20,0,0,0)),
        plot.title = ggtext::element_textbox(margin = margin(10,0,20,0)))+
  labs(x = "Change in spending from 2011 to 2015 (£s per capita)", y = "Change in proportion providing unpaid\ncare from 2011 to 2021 (%)", title = "Local Authority Change in Unpaid Care Provision by Cuts to Adult Social Care Spending", caption = "Source: Census 2021 and 2011, Place-Based Longitudinal Data Resource")

ggsave("Plots/Local Authority Change in Unpaid Care Provision by Cuts to Adult Social Care Spending.png", units = "in", width = 9, height = 5, dpi = 1000)


# Model the relationship
m<- df%>%
  filter(spending_change_11_15>-0.3)%>%
  lm(care_change ~ spending_change_11_15 + I(spending_change_11_15^2) + I(spending_change_11_15^3), data = .)

summary(m)

save(m, file = "Models/M1_170723.Rda")

df%>%
  filter(spending_change_11_15>-0.3)%>%
  ggplot()+
  geom_segment(aes(x = spending_change_11_15, xend = spending_change_11_15, y = care_change, yend = m$fitted.values))+
  geom_point(aes(x = spending_change_11_15, y = care_change), size = 0.5)+
  geom_point(aes(x = m$model$spending_change_11_15, y = m$fitted.values, color = "red"))
  

# Analyse baseline spending figure for 2011 to account for different starting points
df%>%
  filter(spending_year_2011<1)%>%
  ggplot()+
  geom_point(aes(x = spending_year_2011, y = spending_change_11_15))+
  geom_smooth(aes(x = spending_year_2011, y = spending_change_11_15))



# Plot spending change against care change
df%>%
  filter(spending_change_11_15>-0.3)%>%
  ggplot()+
  geom_point(aes(x = spending_change_11_15, y = care_20_49_change))+
  geom_smooth(aes(x = spending_change_11_15, y = care_20_49_change))


df%>%
  filter(spending_change_11_15>-0.3)%>%
  pivot_longer(cols = care_19_less_change:care_50_plus_change, names_to = "care", values_to = "change")%>%
  mutate(care = case_when(care == "care_19_less_change" ~ "19 hours or less per week",
                            care == "care_20_49_change" ~ "20 - 49 hours per week",
                            care == "care_50_plus_change" ~ "50 hours or more per week"))%>%
  ggplot()+
  geom_point(aes(x = 1000*spending_change_11_15, y = change), size = 1, color = pal[3], fill = pal[2], shape = 21)+
  geom_smooth(aes(x = 1000*spending_change_11_15, y = change), color = pal[3])+
  facet_wrap(~care, scales = "free")+
  geom_vline(xintercept = 0, linetype = 2)+
  thm+
  theme(axis.title.y =element_text(margin = margin(0,10,0,0)),
        plot.caption = element_text(margin = margin(20,0,0,0)),
        plot.title = ggtext::element_textbox(margin = margin(10,0,20,0)))+
  labs(x = "Change in spending from 2011 to 2015 (£s per capita)", y = "Change in proportion providing unpaid\ncare from 2011 to 2021 (%)", title = "Local Authority Change in Unpaid Care Provision by Cuts to Adult Social Care Spending", subtitle = "Care separated by number of hours per week", caption = "Source: Census 2021 and 2011, Place-Based Longitudinal Data Resource")

ggsave("Plots/Local Authority Change in Unpaid Care Provision by Hours by Cuts to Adult Social Care Spending.png", units = "in", width = 9, height = 5, dpi = 1000)

df%>%
  #filter(spending_change_11_15>-0.3)%>%
  pivot_longer(cols = care_19_less_change:care_50_plus_change, names_to = "care", values_to = "change")%>%
  mutate(care = case_when(care == "care_19_less_change" ~ "19 hours or less per week",
                          care == "care_20_49_change" ~ "20 - 49 hours per week",
                          care == "care_50_plus_change" ~ "50 hours or more per week"))%>%
  ggplot()+
  geom_point(aes(x = 1000*spending_change_11_15, y = change), size = 1, color = pal[3], fill = pal[2], shape = 21)+
  geom_smooth(aes(x = 1000*spending_change_11_15, y = change), color = pal[3])+
  facet_wrap(~care, scales = "free")+
  geom_vline(xintercept = 0, linetype = 2)+
  thm+
  theme(axis.title.y =element_text(margin = margin(0,10,0,0)),
        plot.caption = element_text(margin = margin(20,0,0,0)),
        plot.title = ggtext::element_textbox(margin = margin(10,0,20,0)))+
  labs(x = "Change in spending from 2011 to 2015 (£s per capita)", y = "Change in proportion providing unpaid\ncare from 2011 to 2021 (%)", title = "Local Authority Change in Unpaid Care Provision by Cuts to Adult Social Care Spending", subtitle = "Care separated by number of hours per week", caption = "Source: Census 2021 and 2011, Place-Based Longitudinal Data Resource")

ggsave("Plots/Outlier Local Authority Change in Unpaid Care Provision by Hours by Cuts to Adult Social Care Spending.png", units = "in", width = 9, height = 5, dpi = 1000)


#### Step 5: Age-Standardised Upaid Caring Rates ####


# Read in 2011 age-specifc care rates
df_care_age_11<- read.csv("../../../../Data/2011/LAD Data/LAD_provision_of_unpaid_care_by_age_group_2011_data.csv")


# Rename and select variables
df_care_age_11<- df_care_age_11%>%
  rename(LAD13CD = mnemonic,
         care = Care)%>%
  select(LAD13CD, care, Age.0.to.15:Age.65.and.over)


# Drop Wales
df_care_age_11<- df_care_age_11%>%
  filter(substr(LAD13CD,1,1) != "W")

# Get lookup between 2013 and 2021 LADs
lad_lookup<- across_yr_lookup(2013, 2021, geog = "LAD")%>%
  select(LAD13CD, LAD21CD)

# The lookup is missing two changes:
#   - LAD13CD E07000190 became LAD21CD E07000246
#   - LAD13CD E07000191 became LAD21CD E07000246

# Amend the lookup to relfect these changes
lad_lookup$LAD21CD[lad_lookup$LAD13CD=="E07000190"]<- "E07000246"
lad_lookup$LAD21CD[lad_lookup$LAD13CD=="E07000191"]<- "E07000246"

# Add 2021 LADs to 2011 data
df_care_age_11<- left_join(df_care_age_11, lad_lookup, by = "LAD13CD")

# Sum counts across merged LADs
df_care_age_11<- df_care_age_11%>%
  group_by(LAD21CD, care)%>%
  summarise(Age.0.to.15 = sum(Age.0.to.15, na.rm = T),
            Age.16.to.24 = sum(Age.16.to.24, na.rm = T),
            Age.25.to.34 = sum(Age.25.to.34, na.rm = T),
            Age.35.to.49 = sum(Age.35.to.49, na.rm = T),
            Age.50.to.64 = sum(Age.50.to.64, na.rm = T),
            Age.65.and.over = sum(Age.65.and.over, na.rm = T))%>%
  ungroup()


# Pivot to long format
df_care_age_11<- df_care_age_11%>%
  pivot_longer(cols = Age.0.to.15:Age.65.and.over, names_to = "age", values_to = "n")

# Drop rows with total counts for any amount of unpaid care
df_care_age_11<- df_care_age_11%>%
  filter(care != "Provides unpaid care")

# Calculate age-specific rate in each care category
df_care_age_11<- df_care_age_11%>%
  group_by(LAD21CD, age)%>%
  mutate(total = sum(n, na.rm = T))%>%
  ungroup()%>%
  mutate(age_specific_rate = n/total)

# Calculate rate for providing any amount of unpaid care
df_total_care_age_11<- df_care_age_11%>%
  filter(care != "Provides no unpaid care")%>%
  group_by(LAD21CD, age)%>%
  summarise(n = sum(n, na.rm = T),
            total = mean(total, na.rm = T))%>%
  ungroup()%>%
  mutate(age_specific_rate = n/total)%>%
  mutate(care = "Provides unpaid care")%>%
  select(LAD21CD, care, age, n, total, age_specific_rate)

df_care_age_11<- rbind(df_care_age_11, df_total_care_age_11)

rm(df_total_care_age_11)


# Read in 2021 age group populations
total_age_population_21<- read.csv("../../../../Data/2021/LAD Data/LAD_age_group_2021_data.csv")

# Rename and select variables
total_age_population_21<- total_age_population_21%>%
  rename(LAD21CD = Lower.tier.local.authorities.Code,
         age = Age..6.categories.,
         total = Observation)%>%
  select(LAD21CD, age, total)

# Recode 2011 age categories to the same as those in the 2021 data
df_care_age_11<- df_care_age_11%>%
  mutate(age = collapse_age_groups(age, unique(total_age_population_21$age)))

# Join 2011 age-specific rates to 2021 age group populations
expected_age_counts_21<- left_join(total_age_population_21, df_care_age_11%>%
                                      select(LAD21CD, age, care, age_specific_rate), by = c("LAD21CD", "age"))

# Calculate expected count for each care category in each age group
expected_age_counts_21<- expected_age_counts_21%>%
  mutate(expected_count = age_specific_rate*total)

# Sum expect counts across age groups
expected_counts_21<- expected_age_counts_21%>%
  group_by(LAD21CD, care)%>%
  summarise(expect = sum(expected_count, na.rm = T))%>%
  ungroup()

# Join expected counts for 2021 to observed counts for 2021
observed_counts_21<- df_care%>%
  select(-contains("proportion"))%>%
  pivot_longer(cols = matches("^n_care.*"), names_to = "care", values_to = "observed_count")%>%
  mutate(care = str_remove(care, "^n_"))

expected_counts_21<- expected_counts_21%>%
    mutate(care = case_when(care == "Provides 1 to 19 hours unpaid care a week" ~ "care_19_less",
                            care == "Provides 20 to 49 hours unpaid care a week" ~ "care_20_49",
                            care == "Provides 50 or more hours unpaid care a week" ~ "care_50_plus",
                            care == "Provides no unpaid care" ~ "care_none",
                            care == "Provides unpaid care" ~ "care"))
  

expected_counts_21<- left_join(expected_counts_21, observed_counts_21, by = c("LAD21CD", "care"))

# Calculate age-standardised rate for 2021
age_standardised_rate_21<- expected_counts_21%>%
  mutate(age_standardised_rate_21 = observed_count/expect)

# Plot distribution of rates
age_standardised_rate_21%>%
  filter(!care %in% c("care_none", "care"))%>%
  filter(!is.na(care))%>%
  mutate(care = case_when(care == "care_19_less" ~ "19 hours or less per week",
                          care == "care_20_49" ~ "20 - 49 hours per week",
                          care == "care_50_plus" ~ "50 hours or more per week"))%>%
  ggplot()+
  geom_histogram(aes(x = age_standardised_rate_21), bins = 50, color = "black", fill = pal[1])+
  geom_vline(xintercept = 1, linetype = 2)+
  facet_wrap(~care)+
  thm+
  labs(x = "Age-standardised upaid caring rate", y = "Count of Local Authorities", title = "Histograms of Age-Standardised Unpaid Caring Rates across Local Authorities: 2021", subtitle = "Care separated by hours provided per week. 2021 rates indirectly age-standardised to the 2011 population.", caption = "Source: England Census 2021 and 2011")

ggsave("Plots/Histograms of Age-Standardised Rates by Local Authority 2021.png", units = "in", height = 5, width = 9, dpi = 1000)

# Join age-standardised rates to main df
age_standardised_rate_21<- age_standardised_rate_21%>%
  pivot_wider(id_cols = LAD21CD, names_from = care, values_from = age_standardised_rate_21, names_prefix = "age_stand_")


df<- left_join(df, age_standardised_rate_21%>%
                 select(LAD21CD, age_stand_care_19_less:age_stand_care), by = "LAD21CD")



#### Step 6: Age-Standardised Spending ####


# Read in LAD population estimates by age for 2001 to 2018
lad_population<- read.csv("../../../../Data/2018/LA Data/LAD_mid_year_population_estimates_2001_to_2018_data.csv")

# Sum across ages and sex to get count of people aged 65 plus
lad_population<- lad_population%>%
  filter(Age>=65)%>%
  group_by(lad2018_code)%>%
  summarise(across(population_2001:population_2018, ~sum(.x, na.rm = T)))%>%
  ungroup()

# Pivot to long format
lad_population<- lad_population%>%
  pivot_longer(cols = population_2001:population_2018, names_to = "Year", values_to = "total_65_plus")

# Filter to only England
lad_population<- lad_population%>%
  filter(substr(lad2018_code,1,1)=="E")

# Convert year variable to integer
lad_population<- lad_population%>%
  mutate(Year = str_extract(Year, "[0-9]{4}")%>%
           as.integer())

# Read in spending data
spending<- read.csv("../../../../Data/2018/LA Data/LA_adult_social_care_expenditure_2011_to_2018_data.csv")

# Check we have population data for all the LADs for which we have spending data
spending$LTLA18CD[!spending$LTLA18CD %in% lad_population$lad2018_code]

# Join LAD population 65 plus counts to spending data
spending<- left_join(spending, lad_population%>%
                       select(lad2018_code, Year, total_65_plus), by = c("LTLA18CD" = "lad2018_code", "Year" = "Year"))

# Get lookup between 2018 and 2021 LADs
lad_lookup<- across_yr_lookup(2018, 2021, geog = "LAD")%>%
  select(LAD18CD, LAD21CD)

# Check for missing LAD18s
table(spending$LTLA18CD %in% lad_lookup$LAD18CD)
table(df$LAD21CD %in% lad_lookup$LAD21CD)

# The lookup is missing two changes:
#   - LAD18CD E07000190 became LAD21CD E07000246
#   - LAD18CD E07000191 became LAD21CD E07000246

# Amend the lookup to relfect these changes
lad_lookup$LAD21CD[lad_lookup$LAD18CD=="E07000190"]<- "E07000246"
lad_lookup$LAD21CD[lad_lookup$LAD18CD=="E07000191"]<- "E07000246"

table(df$LAD21CD %in% lad_lookup$LAD21CD)

# Join LAD21 to spending data
spending<- left_join(spending, lad_lookup, by = c("LTLA18CD" = "LAD18CD"))

# Sum spending amount and population across merged LADs
spending<- spending%>%
  group_by(LAD21CD, Year)%>%
  summarise(population = sum(Pop_Est_Adult, na.rm = T),
            total_65_plus = sum(total_65_plus, na.rm = T),
            nominal_spend = sum(Adult_Social_Care_GrossExpen, na.rm = T),
            inflation_rate = mean(Inflation.Ratio.to.2018))%>%
  ungroup()

# Calculate real-terms spend
spending<- spending%>%
  mutate(real_spend = nominal_spend*inflation_rate)

# Calculate real-terms spend per capita
spending<- spending%>%
  mutate(real_spend_pc = real_spend/total_65_plus)


# Plot change in spending per person aged 65+ over time by LA
spending%>%
  mutate(Year = as.factor(Year))%>%
  ggplot()+
  geom_boxplot(aes(x = Year, y = real_spend_pc))+
  thm

# Plot change in mean spending per person aged 65+ over time 
spending%>%
  group_by(Year)%>%
  summarise(real_spend = 1000*mean(real_spend_pc, na.rm = T))%>%
  ggplot()+
  geom_point(aes(x = Year, y = real_spend),  color = pal[2])+
  geom_line(aes(x = Year, y = real_spend), linewidth = 1, color = pal[2])+
  scale_y_continuous(labels = function(x){format(x, width = 3)})+
  thm+
  theme(axis.title.y = element_text(margin = margin(0,10,0,0)))+
  labs(x = "Year", y = "Mean spending\n(£s per person aged 65+)", title = "Mean Gross Adult Social Care Spending per Older Adult across English Local Authorities: 2011 - 2018", subtitle = "Spending expressed in real terms adjusted to 2018 pounds", caption = "Source: Place-Based Longitudinal Data Resource")

ggsave("Plots/Mean Gross Adult Social Care Spending per Person Aged 65+ across English Local Authorities 2011 - 2018.png", units = "in", width = 9, height = 5, dpi = 1000)


# Calculate chaneg in age-adjusted spending from 2011 to 2018
spending<- spending%>%
  pivot_wider(id_cols = LAD21CD, names_from = Year, values_from = real_spend_pc, names_prefix = "real_spend_pc_")%>%
  mutate(spending65_change_11_18 = real_spend_pc_2018 - real_spend_pc_2011,
         spending65_change_11_17 = real_spend_pc_2017 - real_spend_pc_2011)
  


#### Step 7: Analyse Age-Standardised Caring and Age-Standardised Spending ####


# Join age-adjusted spending to main df
df<- left_join(df, spending%>%
                 rename(spending65_year_2011 = real_spend_pc_2011,
                        spending65_year_2017 = real_spend_pc_2017,
                        spending65_year_2018 = real_spend_pc_2018)%>%
                 select(LAD21CD, contains("spending65")), by = "LAD21CD")




ggplot(df)+
  geom_point(aes(x = spending65_change_11_18, y = age_stand_care))

ggplot(df)+
  geom_point(aes(x = spending65_change_11_18, y = age_stand_care_19_less))

ggplot(df)+
  geom_point(aes(x = spending65_change_11_18, y = age_stand_care_20_49))

ggplot(df)+
  geom_point(aes(x = spending65_change_11_18, y = age_stand_care_50_plus))


df%>%
  #filter(spending65_change_11_18>-3)%>%
  #filter(age_stand_care_50_plus<1.4)%>%
  pivot_longer(cols = age_stand_care_19_less:age_stand_care_50_plus, names_to = "care", values_to = "change")%>%
  mutate(care = case_when(care == "age_stand_care_19_less" ~ "19 hours or less per week",
                          care == "age_stand_care_20_49" ~ "20 - 49 hours per week",
                          care == "age_stand_care_50_plus" ~ "50 hours or more per week"))%>%
  ggplot()+
  geom_point(aes(x = 1000*spending65_change_11_18, y = change), size = 1, color = pal[3], fill = pal[2], shape = 21)+
  geom_smooth(aes(x = 1000*spending65_change_11_18, y = change), color = pal[3])+
  facet_wrap(~care)+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_hline(yintercept = 1, linetype = 2)+
  thm+
  theme(axis.title.y =element_text(margin = margin(0,10,0,0)),
        plot.caption = element_text(margin = margin(20,0,0,0)),
        plot.title = ggtext::element_textbox(margin = margin(10,0,20,0)))+
  labs(x = "Change in spending from 2011 to 2018 (£s per person aged 65+)", y = "Age-Standardised Caring Rate", title = "Local Authority Change in Unpaid Care Provision by Cuts to Adult Social Care Spending", subtitle = "Care separated by number of hours per week. 2021 rates age-standardised to 2011 population.", caption = "Source: Census 2021 and 2011, Place-Based Longitudinal Data Resource")

ggsave("Plots/Age-Adjusted Local Authority Change in Unpaid Care Provision by Hours by Cuts to Adult Social Care Spending.png", units = "in", width = 9, height = 5, dpi = 1000)

# Model the relationship
# Controlling for 2011 baseline
m1<- lm(age_stand_care_50_plus ~ spending65_change_11_18 + spending65_year_2011, data = df)
summary(m1)

# Controlling for 2018 end-line
m2<- lm(age_stand_care_50_plus ~ spending65_change_11_18 + spending65_year_2018, data = df)
summary(m2)


#### Step 8: IMD ####


# Read in IMD data
imd<- read.csv("../../../../Data/2019/LAD Data/LAD_imd_2019_data.csv")

# Rename variables
imd<- imd%>%
  rename(LAD19CD = Local.Authority.District.code..2019.,
          imd_avg_rank = IMD...Average.rank)%>%
  select(LAD19CD, imd_avg_rank)

# Get lookup from LAD19 to LAD21
lad_lookup<- across_yr_lookup(2019, 2021, "LAD")%>%
  select(LAD19CD, LAD21CD)

# Check for missing LADs
imd$LAD19CD[!imd$LAD19CD %in% lad_lookup$LAD19CD]

# The lookup is missing E07000246 (it has it as E07000190 and E07000191)
# Amend the lookup to relfect these changes
lad_lookup<- lad_lookup%>%
  filter(!LAD19CD %in% c("E07000190", "E07000191"))

lad_lookup<- lad_lookup%>%
  rbind(data.frame(LAD19CD = "E07000246", LAD21CD = "E07000246"))

imd$LAD19CD[!imd$LAD19CD %in% lad_lookup$LAD19CD]

# Join 2021 LADs to IMD data
imd<- left_join(imd, lad_lookup, by = "LAD19CD")

# Join IMD data to main df
df<- left_join(df, imd%>%
                 select(LAD21CD, imd_avg_rank), by = "LAD21CD")

# Look at associations of IMD and variables
ggplot(df)+
  geom_point(aes(x = imd_avg_rank, y = age_stand_care))

ggplot(df)+
  geom_point(aes(x = imd_avg_rank, y = age_stand_care_19_less))

ggplot(df)+
  geom_point(aes(x = imd_avg_rank, y = age_stand_care_20_49))

ggplot(df)+
  geom_point(aes(x = imd_avg_rank, y = age_stand_care_50_plus))

# Model the relationship
df$scaled_imd<- scale(df$imd_avg_rank)

m<- lm(age_stand_care_50_plus ~ scaled_imd, data = df)
summary(m)

m<- lm(age_stand_care_50_plus ~ scaled_imd + spending65_year_2011 + spending65_change_11_18, data = df)
summary(m)

m<- lm(age_stand_care_50_plus ~ scaled_imd + spending65_year_2011 + spending65_change_11_18 + proportion_care_50_plus_2011, data = df)
summary(m)


df%>%
  filter(spending65_change_11_18>-3)%>%
ggplot()+
  geom_point(aes(x = imd_avg_rank, y = spending65_change_11_18))

ggplot(df)+
  geom_point(aes(x = imd_avg_rank, y = proportion_care_50_plus))

ggplot(df)+
  geom_point(aes(x = imd_avg_rank, y = proportion_care_50_plus_2011))

ggplot(df)+
  geom_point(aes(x = proportion_care_50_plus_2011, y = age_stand_care_50_plus, color = spending65_year_2018))+
  scale_color_viridis_c()
  
ggplot(df)+
  geom_point(aes(x = spending65_year_2018, y = age_stand_care_50_plus, color = scaled_imd))

m<- lm(age_stand_care_50_plus ~ I(scale(spending65_year_2018)) + scaled_imd + proportion_care_50_plus_2011, df)
summary(m)


# Model suggests:
# - If an area already had lots of people giving unpaid care, there wasn't as big of an increase
# - If an area had higher levels of spending in 2018 on social care, there wasn't as big of an increase
# - If an area was more deprived, there wasn't as big of an increase
# Add excess mortality to the above model and see if it affects deprivation finding.



#### Step 9: Mortality rates ####


# Read in LAD deaths data
deaths<- read.csv("../../../../Data/2021/LAD Data/LAD_deaths_by_sex_by_age_1981_to_2021_data.csv")

# Drop Wales
deaths<- deaths%>%
  filter(substr(LA.code, 1, 1) == "E")

# Work out what version of the LAD code is being used
deaths_lad_codes<- unique(deaths$LA.code)

deaths_lad_codes[!deaths_lad_codes %in% lad_lookup$LAD21CD]

# Looks like they're 2021 codes

# Rename and select columns
deaths<- deaths%>%
  rename(LAD21CD = LA.code,
         year = Year,
         sex = Sex)%>%
  select(LAD21CD, year, sex, X.1:X95.)

colnames(deaths)[4:24]<- colnames(deaths)[4:24]%>%
  str_replace("X", "aged_")%>%
  str_replace("\\.", "_")

colnames(deaths)[4]<- "aged_under_1"
colnames(deaths)[24]<- "aged_95_plus"

# Pivot to longer
deaths<- deaths%>%
  pivot_longer(cols = aged_under_1:aged_95_plus, names_to = "age", values_to = "n")

# Sum deaths across sex
deaths<- deaths%>%
  group_by(LAD21CD, year, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Filter to just 2001 onwards
deaths<- deaths%>%
  filter(year>=2001)

# Merge deaths in the 90 - 94 and 95 plus categories
deaths<- deaths%>%
  mutate(age = case_when(age == "aged_90_94" | age == "aged_95_plus" ~ "aged_90_plus",
                   T ~ age))%>%
  group_by(LAD21CD, year, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Read in population counts
total_age_population_01_20<- read.csv("../../../../Data/2021/LAD Data/LAD_mid_year_population_estimates_2001_to_2021_data.csv")

# Rename and select variables
total_age_population_01_20<- total_age_population_01_20%>%
  rename(LAD21CD = ladcode21)%>%
  select(LAD21CD, sex, age, population_2001:population_2020)

# Pivot to longer
total_age_population_01_20<- total_age_population_01_20%>%
  pivot_longer(cols = population_2001:population_2020, names_to = "year", values_to = "total")

# Recode "90" in age variable to "90 plus"
total_age_population_01_20$age[total_age_population_01_20$age==90]<- "90_plus"

# Recode the year variable
total_age_population_01_20<- total_age_population_01_20%>%
  mutate(year = str_remove(year, "population_")%>%
           as.numeric())

# Collapse the age groups to the same as those in the deaths data
ages<- total_age_population_01_20%>%
  select(age)%>%
  unique()

ages<- ages%>%
  mutate(new_age = collapse_age_groups(age, unique(deaths$age)))

total_age_population_01_20<- left_join(total_age_population_01_20, ages, by = "age")

total_age_population_01_20<- total_age_population_01_20%>%
  select(-age)%>%
  rename(age = new_age)%>%
  group_by(LAD21CD, year, age)%>%
  summarise(total = sum(total, na.rm = T))%>%
  ungroup()

# Separate out 2011 as baseline
deaths_11<- deaths%>%
  filter(year == 2011)

total_population_11<- total_age_population_01_20%>%
  filter(year == 2011)

# Calculate age-specific deaths rates for 2011
deaths_11<- left_join(deaths_11, total_population_11, by = c("LAD21CD", "year", "age"))

deaths_11<- deaths_11%>%
  mutate(age_specific_mortality_rate_11 = n/total)

deaths_11$age_specific_mortality_rate_11


# Calculated expected deaths in each age group in each LAD in each year
expected_aged_deaths<- left_join(total_age_population_01_20, deaths_11%>%
                                   select(LAD21CD, age, age_specific_mortality_rate_11), by = c("LAD21CD", "age"))

expected_aged_deaths<- expected_aged_deaths%>%
  mutate(expected_deaths = total * age_specific_mortality_rate_11)

# Sum the expected deaths across age groups
expected_deaths<- expected_aged_deaths%>%
  group_by(LAD21CD, year)%>%
  summarise(expected_deaths = sum(expected_deaths, na.rm = T))%>%
  ungroup()

# Calculate observed deaths across all age groups for each LAD for each year
observed_deaths<- deaths%>%
  group_by(LAD21CD, year)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

# Join expected deaths to observed deaths
age_standardised_mortality_rates_01_21<- left_join(observed_deaths, expected_deaths, by = c("LAD21CD", "year")) 

# Calculate age-standardised mrotality rate
age_standardised_mortality_rates_01_21<- age_standardised_mortality_rates_01_21%>%
  mutate(age_standardised_mortality_rate = n/expected_deaths)

# Plot ASMR over time
ggplot(age_standardised_mortality_rates_01_21)+
  geom_boxplot(aes(x = as.factor(year), y = age_standardised_mortality_rate))

# Plot distribution of ASMR in 2018
age_standardised_mortality_rates_01_21%>%
  filter(year == 2018)%>%
ggplot()+
  geom_histogram(aes(x = age_standardised_mortality_rate))


# Plot distribution of ASMR in 2020
age_standardised_mortality_rates_01_21%>%
  filter(year == 2020)%>%
  ggplot()+
  geom_histogram(aes(x = age_standardised_mortality_rate))

# Pivot to wide format
age_standardised_mortality_rates_01_21<- age_standardised_mortality_rates_01_21%>%
  pivot_wider(id_cols = LAD21CD, names_from = year, values_from = age_standardised_mortality_rate, names_prefix = "ASMR_")

# Join ASMR data to main data
df<- left_join(df, age_standardised_mortality_rates_01_21, by = "LAD21CD")

# Look at relationship of mortality to change in caring rates
df%>%
  ggplot()+
    geom_point(aes(x = ASMR_2020, y = age_stand_care))

df%>%
  ggplot()+
  geom_point(aes(x = ASMR_2020, y = age_stand_care_19_less))

df%>%
  ggplot()+
  geom_point(aes(x = ASMR_2020, y = age_stand_care_20_49))

df%>%
  ggplot()+
  geom_point(aes(x = ASMR_2020, y = age_stand_care_50_plus))

df%>%
  ggplot()+
  geom_point(aes(x = ASMR_2018, y = age_stand_care))

df%>%
  ggplot()+
  geom_point(aes(x = ASMR_2018, y = age_stand_care_19_less))

df%>%
  ggplot()+
  geom_point(aes(x = ASMR_2018, y = age_stand_care_20_49))

df%>%
  ggplot()+
  geom_point(aes(x = ASMR_2018, y = age_stand_care_50_plus))

# Model the relationship
m<- lm(age_stand_care_50_plus ~ I(scale(spending65_year_2011)) + I(scale(spending65_change_11_18))+ scaled_imd + proportion_care_50_plus_2011 + ASMR_2020 + ASMR_2018, df)
summary(m)

m<- lm(age_stand_care_50_plus ~ I(scale(spending65_year_2011)) + I(spending65_year_2018/spending65_year_2011)+ scaled_imd + proportion_care_50_plus_2011 + ASMR_2020 + ASMR_2018, df)
summary(m)

# Above suggests:
# - Areas with higher proportion providing care in 2011 saw less of an increase
# - Areas with larger increases in mortality rates in 2020 (compared to 2011) saw less of an increase
# - Areas with larger increases in mortality rates in 2018 (compared to 2011) did not see a significantly different change
# - Areas with higher levels of spending in 2011 saw less of an increase. Same if using 2018 variable.
# - When looking at the change in spending between 2011 and 2018, the absolute change is non-significant, but the relative change shows that areas with increased spending saw bigger increases.
# - Once controlling for mortality rates, deprivation is not significant


#### Step 10: Export ####


# Export the Local Authority spending data to be incorporated into the LSOA models
write.csv(spending, "../../../../Data/2018/LA Data/LAD_real_term_spending_per_person_aged_65_plus_2011_to_2018_data.csv")
