library(dplyr)
library(stringr)
library(ggplot2)
library(sociodemographics)
library(nomisr)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis/Overall Analysis")
rm(list = ls())


# Read in 2021 living arrangements, age, sex data
df_living_age_sex<- read.csv("../../../../Data/2021/EW Data/EW_living_arrangements_by_age_group_by_sex_2021_data.csv")

# Rename and select relevant variables
df_living_age_sex<- df_living_age_sex%>%
  rename(age = Age..18.categories.,
         sex = Sex..2.categories.,
         living = Living.arrangements..11.categories.,
         n = Observation)%>%
  select(age, sex, living, n)


# Change the living categories to match 2011 data roughly
df_living_age_sex<- df_living_age_sex%>%
  mutate(living = case_when(str_detect(living, "Cohabiting") ~ "Living in a couple: Cohabiting",
                                str_detect(living, "^Living in a.* couple: Married") ~ "Living in a couple: Married or in a registered same-sex civil partnership",
                                str_detect(living, "Single") ~ "Not living in a couple: Single (never married or never registered a same-sex civil partnership)",
                                str_detect(living, "Not living in a couple: Married") ~ "Not living in a couple: Married or in a registered same-sex civil partnership",
                                str_detect(living, "Not living in a couple: Separated") ~ "Not living in a couple: Separated (but still legally married or still legally in a same-sex civil partnership)",
                                str_detect(living, "Not living in a couple: Divorced") ~ "Not living in a couple: Divorced or formerly in a same-sex civil partnership which is now legally dissolved",
                                str_detect(living, "Not living in a couple: Widowed") ~ "Not living in a couple: Widowed or surviving partner from a same-sex civil partnership",
                                T ~ living
  ))%>%
  group_by(sex, age, living)%>%
  summarise(n = sum(n, na..rm = T))


# Download 2011 living arrangements, age, sex data from NOMIS API
id<- nomis_search(name = "*DC1108EW*")$id[1]

df_living_age_sex_11<- nomis_get_data(id = id, geography = "TYPE499", C_AGE = 1:16, C_SEX = 1:2, C_LARPUK11 = c(2:3, 5:9), tidy = T)%>%
  filter(geography_name == "England and Wales")%>%
  rename(age = c_age_name,
         sex = c_sex_name,
         living = c_larpuk_11_name,
         n = obs_value)%>%
  select(age, sex, living, n)


# Calculate proportion in each living arrangement within each age-sex group
df_living_age_sex<- df_living_age_sex%>%
  group_by(age, sex)%>%
  mutate(total = sum(n, na.rm = T))%>%
  ungroup()%>%
  mutate(proportion = 100*n/total)


df_living_age_sex_11<- df_living_age_sex_11%>%
  group_by(age, sex)%>%
  mutate(total = sum(n, na.rm = T))%>%
  ungroup()%>%
  mutate(proportion = 100*n/total)

# Calculate proportions not living in a couple
df_couple_age_sex<- df_living_age_sex%>%
  mutate(couple = ifelse(substr(living, 1,3) == "Not", "No", "Yes"))%>%
  group_by(age, sex, couple)%>%
  summarise(proportion = sum(proportion, na.rm = T))%>%
  ungroup()

df_couple_age_sex_11<- df_living_age_sex_11%>%
  mutate(couple = ifelse(substr(living, 1,3) == "Not", "No", "Yes"))%>%
  group_by(age, sex, couple)%>%
  summarise(proportion = sum(proportion, na.rm = T))%>%
  ungroup()

# Collapse age groups in 2011 so they are the same as 2021
df_couple_age_sex_11<- df_couple_age_sex_11%>%
  mutate(age = collapse_age_groups(age, unique(df_couple_age_sex$age)))%>%
  group_by(age, sex, couple)%>%
  summarise(proportion = sum(proportion, na.rm  = T))%>%
  ungroup()

# Rename 2011 variables
df_couple_age_sex_11<- df_couple_age_sex_11%>%
  rename(proportion_11 = proportion)

# Rename 2011 sex categories
df_couple_age_sex_11<- df_couple_age_sex_11%>%
  mutate(sex = str_remove(sex, "s"))

df_couple_age_sex<- left_join(df_couple_age_sex, df_couple_age_sex_11, by = c("age", "sex", "couple"))

# Calculate change in proportion living in a couple by age-sex group
df_couple_age_sex<- df_couple_age_sex%>%
  filter(couple == "Yes")%>%
  mutate(change = proportion - proportion_11)

# Plot change
df_couple_age_sex%>%
  mutate(age = order_age_groups(age))%>%
  ggplot()+
    geom_col(aes(x = change, y = age, fill = sex), position = position_dodge(), linewidth = 0.5, width = 0.5, colour = "black")+
  thm+
  scale_fill_manual(values = pal)


# Read in unpaid caring rates by age and sex for 2021
load("Rates Data/Age Sex 2021 Rates.Rda")


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

total_age_sex_population_11<- nomis_get_data(id = id, geography = "TYPE499", C_SEX = 1:2, C_AGE = 1:39, tidy = T)%>%
  filter(geography_name == "England and Wales")%>%
  rename(sex = c_sex_name,
         age = c_age_name,
         total = obs_value)%>%
  select(sex, age, total)


# Recode age groups
df_age_sex_11<- df_age_sex_11%>%
  mutate(age = collapse_age_groups(age, unique(df_couple_age_sex$age)))%>%
  group_by(care, age, sex)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

total_age_sex_population_11<- total_age_sex_population_11%>%
  mutate(age = collapse_age_groups(age, unique(df_couple_age_sex$age)))%>%
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
                            rename(proportion_11 = proportion,
                                   n_11 = n,
                                   total_population_11 = total)%>%
                            select(-care), df_total_age_sex_21%>%
                            filter(care == "Provides unpaid care")%>%
                            select(-care), by = c("age", "sex"))

# Calculate change in rates
age_sex_rates<- age_sex_rates%>%
  mutate(care_change = proportion - proportion_11)


# Join couple data and care data
df<- left_join(select(age_sex_rates, age, sex, care_change), df_couple_age_sex)

# Plot change in care against change in couples


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

df%>%
  mutate(age = order_age_groups(age))%>%
  filter(age %in% levels(age)[10:18])%>%
  ggplot()+
    geom_point(aes(x = change, y = care_change, fill = sex, text = age), shape = 21, size = 3)+
    geom_vline(xintercept = 0, color = "grey70")+
    geom_hline(yintercept = 0, color = "grey70")+
    annotate(geom = "curve", xend = -2.5, x = -0.75, yend = -3.5, y = -1.5, curvature = 0.25, arrow = arrow(length = unit(5, "pt")))+
    annotate(geom = "label", x = -0.5, y = -0.75, hjust = 0, vjust = 1, label = "Age-sex groups which saw a decrease in\npeople living in couples also saw the biggest\ndecreases in unpaid care provision", family = "Rubik", size = 3, alpha = 0.5, label.size = 0)+
  thm+
  theme(axis.title.y = element_text(margin = margin(0,10,0,0)))+
  scale_fill_manual(values = pal, name = "Sex")+
  labs(x = "Change in proportion living in a couple (%)", y = "Change in proportion providing unpaid care (%)", title = "Change in proportion living in a couple and proportion providing unpaid care for each age-sex group: 2011 to 2021", subtitle = "Each point is an age-sex group. Groups for aged 50 and over only.", caption  = "Source: England and Wales Census 2021 and 2011")

ggsave("Plots/Age-sex proportion living in couples by proportion unpaid care change.png", unit = "in", width = 9, height = 7, dpi = 1000)



# Look at reasons for not living in a couple

df_living_age_sex<- df_living_age_sex%>%
  mutate(age = str_replace(age, "Aged", "Age")%>%
           str_remove(" years"))

not_couple<- left_join(df_living_age_sex, df_living_age_sex_11%>%
                       mutate(sex = str_remove(sex, "s"))%>%
                       rename(proportion_11 = proportion)%>%
                       select(age, sex,living, proportion_11))

not_couple<- not_couple%>%
  mutate(change = proportion - proportion_11)

not_couple%>%
  mutate(age = order_age_groups(age))%>%
  filter(age %in% levels(age)[9:18])%>%
  filter(!is.na(change))%>%
  mutate(couple = ifelse(substr(living, 1,3) == "Not", "Not living in a couple", "Living in a couple"))%>%
  mutate(living = paste0(couple, ": ", str_extract(living, "(?<=[lL]iving in a couple: )[^ ]*")%>%
           str_to_sentence()))%>%
  ggplot()+
    geom_col(aes(x = change, y = age, fill = sex), position = position_dodge(), linewidth = 0.25, colour = "black", width = 0.5)+
    facet_wrap(~living)+
  thm+
  scale_fill_manual(values = pal)

