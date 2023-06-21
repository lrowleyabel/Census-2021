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
str_detect("4 or less", "under|less|lower")

order_age_groups<- function(x){
  
  # Get lowest age for each group
  lower_bounds<- lapply(x, FUN = function(age_group){
    if (str_detect(age_group, "under|less|lower|younger")){
      return(0)
    } else {
      return(as.numeric(str_extract(age_group, "[0-9]{1,3}")))  
    }
  })%>%
    unlist()
  
  # Create vector of ordered age groups to use as levels
  age_group_levels<- unique(x[order(lower_bounds)])
  
  # Return original vector as factor with new levels
  return(factor(x, levels = age_group_levels))
  
}

df_total_age_sex_21$age<- order_age_groups(df_total_age_sex_21$age)

pyramid_negation<- function(n, sex){
  if (sex == "Male" | sex == "male" | sex == "Males" | sex == "male"){
    return(-1*n)
  } else {
    return(n)
  }
}

pyramid_negation<- Vectorize(pyramid_negation)

pyramid_axis_labels<- function(x){
  if (x < 0){
    x<- x*-1
  }
  x<- format(x, big.mark = ",", scientific = F, width = 0)
  return(x)
}

pyramid_axis_labels<- Vectorize(pyramid_axis_labels)



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
  mutate(age = order_age_groups(age))%>%
  filter(care == "Provides unpaid care")%>%
  ggplot()+
      geom_col(aes(x = pyramid_negation(n, sex), y = age, fill = sex))+
  scale_fill_manual(values = pal, name = "Sex")+
  scale_x_continuous(labels = function(x) pyramid_axis_labels(x))+
  thm+
  labs(x = "Count", title = "Age-Sex Structure of Unpaid Carer Population: 2021", caption = "Source: England and Wales Census 2021")

ggsave("Plots/Age-Sex Structure of Unpaid Carers 2021.png", unit = "in", height = 5, width = 9, dpi = 1000)


#### STEP 2: AGE AND SEX 2011 ####


# Download 2011 carer age and sex data from Nomis API
id<- nomis_search(name = "*DC3303EWr*")$id[1]
df_age_sex_11<- nomis_get_data(id = id, geography = "TYPE499", C_CARER = 1:5, C_AGE = 1:21, C_SEX = 1:2, C_HEALTH = 0, tidy = T)%>%
  select(geography_name, c_carer_name, c_sex_name, c_age_name, obs_value)%>%
  filter(geography_name == "England and Wales")

# Rename columns
colnames(df_age_sex_11)<- c("geography", "care", "sex", "age", "n")

# Split out total counts for any amount of care
df_total_age_sex_11<- df_age_sex_11%>%
  filter(care %in% c("Provides unpaid care: Total", "Provides no unpaid care"))%>%
  mutate(care = str_remove(care, ": Total"))

# Order age groups
df_total_age_sex_11<- df_total_age_sex_11%>%
  mutate(age = order_age_groups(age))

levels(df_total_age_sex_11$age)
levels(df_total_age_sex_21$age)

collapse_age_groups<- function(start_groups, target_groups){

  # Create dataframe of taregt groups with group name, lower bound and upper bound
  start_lower_bounds<- lapply(start_groups, FUN = function(age_group){
    if (str_detect(age_group, "under|less|lower|younger")){
      return(0)
    } else {
      return(as.numeric(str_extract(age_group, "[0-9]{1,3}")))  
    }
  })%>%
    unlist()
  
  start_upper_bounds<- lapply(start_groups, FUN = function(age_group){
    if (str_detect(age_group, "over|more|higher|older")){
      return(150)
    } else {
      extracted_ages<- str_extract_all(age_group, "[0-9]{1,3}")%>%
        unlist()
      if (length(extracted_ages)>1){
        return(as.numeric(extracted_ages[[2]]))
      } else {
        return(as.numeric(extracted_ages[[1]]))
      }
    }
  })%>%
    unlist()
  
  start_df<- data.frame(
    start_name = start_groups,
    start_lower = start_lower_bounds,
    start_upper = start_upper_bounds
  )
    
  # Create dataframe of taregt groups with group name, lower bound and upper bound
  target_lower_bounds<- lapply(target_groups, FUN = function(age_group){
    if (str_detect(age_group, "under|less|lower|younger")){
      return(0)
    } else {
      return(as.numeric(str_extract(age_group, "[0-9]{1,3}")))  
    }
  })%>%
    unlist()
  
  target_upper_bounds<- lapply(target_groups, FUN = function(age_group){
    if (str_detect(age_group, "over|more|higher|older")){
      return(150)
    } else {
      extracted_ages<- str_extract_all(age_group, "[0-9]{1,3}")%>%
        unlist()
      if (length(extracted_ages)>1){
        return(as.numeric(extracted_ages[[2]]))
      } else {
        return(as.numeric(extracted_ages[[1]]))
      }
    }
  })%>%
    unlist()
  
  target_df<- data.frame(
    target_name = target_groups,
    target_lower = target_lower_bounds,
    target_upper = target_upper_bounds
  )
  
  # Loop through start groups
  matched_names<- lapply(1:nrow(start_df), FUN = function(i){
    
    # Loop through target groups
    for (j in 1:nrow(target_df)){
      
      # Get start and target bounds
      start_lower<- start_df$start_lower[i]
      start_upper<- start_df$start_upper[i]
      target_lower<- target_df$target_lower[j]
      target_upper<- target_df$target_upper[j]
      
      # If start lower bound is greater than or equal to current target lower bound
      # and start upper bound is less than or equal to current target upper bound
      # then return current target group name
      
      if (start_lower >= target_lower){
        if (start_upper <= target_upper){
          return(target_df$target_name[j])
        }
      }
      
    }
  })%>%
    unlist()
  
  # Error if groups not matched
  if (length(matched_names) != length(start_groups)){
    stop("Some age groups not matched")
  }
  
  # Return matched groups
  return(matched_names)
  
}

df_total_age_sex_11<- df_total_age_sex_11%>%
  mutate(age = collapse_age_groups(start_groups = age, target_groups = df_age_sex_21$age))%>%
  group_by(care, sex, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()

df_total_age_sex_11%>%
  mutate(age = str_remove(age, "Aged "))%>%
  mutate(age = order_age_groups(age))%>%
  filter(care == "Provides unpaid care")%>%
  ggplot()+
  geom_col(aes(x = pyramid_negation(n, sex), y = age, fill = sex))+
  scale_fill_manual(values = pal, name = "Sex")+
  scale_x_continuous(labels = function(x) pyramid_axis_labels(x))+
  thm+
  labs(x = "Count", title = "Age-Sex Structure of Unpaid Carer Population: 2011", caption = "Source: England and Wales Census 2011")

ggsave("Plots/Age-Sex Structure of Unpaid Carers 2011.png", unit = "in", height = 5, width = 9, dpi = 1000)


#### STEP 3: COMPARING AGE AND SEX IN 2011 AND 2021 ####

# Join 2011 and 2021
df_total_age_sex<- df_total_age_sex_11%>%
  rename(n_2011 = n)%>%
  mutate(sex = str_replace(sex, "ales", "ale"))%>%
  left_join(df_total_age_sex_21%>%
          rename(n_2021 = n))

df_total_age_sex%>%
  filter(care == "Provides unpaid care")%>%
  mutate(age = str_remove(age, "Aged "))%>%
  mutate(age = order_age_groups(age))%>%
  ggplot()+
    geom_col(aes(x = pyramid_negation(n_2021, sex), y = age, fill = sex), color = "black", size = 0.3)+
    scale_fill_manual(values = pal, name = "2021")+
    guides(fill = guide_legend(keywidth = 1, keyheight = 0.5))+
    ggnewscale::new_scale_fill()+
    geom_point(aes(x = pyramid_negation(n_2011, sex), y = age, fill = sex), shape = 21)+
    scale_fill_manual(values = pal, name = "2011")+
    scale_x_continuous(breaks = c(-400000, -200000, 0, 200000, 400000), labels = function(x) pyramid_axis_labels(x), limits = c(-450000,450000))+
    guides(fill = guide_legend(keyheight = 0.5, override.aes = list(size = 3)))+
    thm+
    theme(legend.box = "horizontal",
          legend.box.just = "bottom")+
    labs(x = "Count", title = "Age-Sex Structure of Unpaid Carer Population: 2021 Comapred to 2011", caption = "Source: England and Wales Census 2021 and 2011")

ggsave("Plots/Age-Sex Structure of Unpaid Carers 2021 and 2011.png", unit = "in", height = 5, width = 9, dpi = 1000)

df_age_sex_11%>%
  filter(care == "Provides 50 or more hours unpaid care a week")%>%
  mutate(sex = str_replace(sex, "ales", "ale"))%>%
  mutate(age = collapse_age_groups(age, target_groups = df_age_sex_21$age))%>%
  group_by(care, sex, age)%>%
  summarise(n = sum(n, na.rm = T))%>%
  ungroup()%>%
  rename(n_2011 = n)%>%
  left_join(df_age_sex_21%>%
              filter(care == "Provides 50 or more hours unpaid care a week")%>%
              rename(n_2021 = n))%>%
  mutate(age = order_age_groups(age))%>%
  ggplot()+
  geom_col(aes(x = pyramid_negation(n_2021, sex), y = age, fill = sex))+
  geom_point(aes(x = pyramid_negation(n_2011, sex), y = age, fill = sex), shape = 21)+
  scale_fill_manual(values = pal, name = "Sex")+
  scale_x_continuous(labels = function(x) pyramid_axis_labels(x))+
  thm+
  labs(x = "Count", title = "Age-Sex Structure of Unpaid Carer Population (50+ hours): 2021 Compared to 2011", caption = "Source: England and Wales Census 2011")

  
  
