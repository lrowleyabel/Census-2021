library(foreign)
library(sf)
library(dplyr)

df<- st_drop_geometry(shps)


crowd<- read.csv("msoa_overcrowding_2011_data.csv")
crowd<- crowd%>%
  mutate(neg = rating_negative_1 + rating_negative_2less)%>%
  mutate(overcrowded = 100*neg/total_households)%>%
  select(MSOA11CD, overcrowded)

df<- left_join(df, crowd)

care<- read.csv("msoa_care_home_beds_data_2022.csv")

pop<- read.csv("msoa_population_estimates_2019_data.csv")

pop<- pop%>%
  select(MSOA.Code, Population)%>%
  mutate(Population = stringr::str_remove_all(Population, ",")%>%
           as.numeric())

colnames(pop)[1]<- "MSOA11CD"

care<- left_join(care, pop)

care$care_beds<- 1000*care$total_beds/care$Population

care<- select(care, MSOA11CD, care_beds, Population)

df<- left_join(df, care)

ethn<- read.csv("msoa_ethnicity_2011_data.csv")

ethn<- ethn%>%
  select(mnemonic, Asian.Asian.British, Black.African.Caribbean.Black.British, Mixed.multiple.ethnic.groups)

colnames(ethn)<- c("MSOA11CD", "Asian", "Black", "Mixed_race")

df<- left_join(df, ethn)

ns_sec<- read.csv("msoa_ns_sec_2011_data.csv")

ns_sec<- ns_sec%>%
  mutate(routine_occupations = 100*(Semi_routine_occupations + Routine_occupations)/total_usual_residents_16_74)%>%
  select(MSOA11CD, routine_occupations)

df<- left_join(df, ns_sec)

colnames(df)<- stringr::str_to_lower(colnames(df))

df<- df%>%
  select(-area_name)%>%
  select(-local_authority)

write.dta(df, "MSOA_Vaccine_Data.dta")

table(df$urbanicity)

save(df, file= "Merged MSOA Data.Rda")
