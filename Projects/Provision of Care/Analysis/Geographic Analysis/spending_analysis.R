load("Rates Data/2021 LSOAs with Rates and Independent Variables.Rda")

shp<- shp%>%
  filter(is.finite(age_standardised_rate))

lad_lookup<- ukgeog::across_yr_lookup(year1 = 2018, year2 = 2022, geog = "LAD")

lad_lookup<- lad_lookup%>%
  select(LAD18CD, LAD22CD)%>%
  unique()

spend<- read.csv("../../../../Data/2018/LA Data/LA_social_care_expenditure.csv")

spend<- spend%>%
  select(Year, LTLA18CD, Real.Term.Social.Care.Spening.Per.Capita)%>%
  rename(spending = Real.Term.Social.Care.Spening.Per.Capita)%>%
  filter(Year %in% c(2007, 2018))%>%
  pivot_wider(id_cols = LTLA18CD, names_from = Year, values_from = spending)

spend<- spend%>%
  mutate(spending_change = `2018`-`2007`)

ggplot(spend)+
  geom_histogram(aes(x = spending_change))

spend<- left_join(spend, lad_lookup, by = c("LTLA18CD" = "LAD18CD"))

lsoa_lad_lookup<- read.csv("../../../../Lookups/OA21_LSOA21_MSOA21_LAD22_EW_LU.csv")%>%
  select(lsoa21cd, lad22cd)%>%
  unique()

shp<- left_join(shp, lsoa_lad_lookup, by = c("LSOA21CD" = "lsoa21cd"))


shp<- left_join(shp, spend, by = c("lad22cd" = "LAD22CD"))

table(shp$lad22cd %in% spend$LAD22CD)

shp$lad22cd[!shp$lad22cd %in% lad_lookup$LAD22CD]%>%
  unique()

spend%>%
  filter(is.na(LAD22CD))

shp%>%
  filter(age_standardised_rate_18<20)%>%
  ggplot()+
  geom_point(aes(x = imd_rank, y = spending_change))+
  geom_smooth(aes(x = imd_rank, y = spending_change))

shp%>%
  filter(age_standardised_rate_18<20)%>%
  ggplot()+
  geom_point(aes(x = spending_change, y = age_standardised_rate))+
  geom_smooth(aes(x = spending_change, y = age_standardised_rate))

ggplot(shp)+
  geom_point(aes(x = `2007`, y = age_standardised_rate))+
  geom_smooth(aes(x = `2007`, y = age_standardised_rate))


cor.test(shp$age_standardised_rate, shp$spending_change, use = "com")

shp%>%
  filter(is.na(spending_change))%>%
  View()

cor.test(shp$spending_change, shp$imd_rank, use = "com")

table(shp$lad22cd %in% lad_lookup$LAD22CD)


df<- shp%>%
  st_drop_geometry()

lads<- df%>%
  group_by(lad22cd)%>%
  summarise(n = sum(n, na.rm = T),
            total = sum(total, na.rm = T),
            n_11 = sum(n_11, na.rm = T),
            total_11 = sum(total_11, na.rm = T),
            spending_change = mean(spending_change, na.rm = T),
            spending07 = mean(`2007`, na.rm = T),
            spending18 = mean(`2018`, na.rm = T),
            imd_rank = mean(imd_rank, na.rm = T))%>%
  ungroup()%>%
  mutate(proportion = 100*n/total,
         proportion_11 = 100*n_11/total_11)%>%
  mutate(change = proportion - proportion_11)

lads%>%
  filter(spending_change>-0.3)%>%
  #mutate(sqrd_spen = spending_change^2)%>%
  lm(change ~ spending_change + I(spending_change^2) + imd_rank, data = .)%>%
  summary()
lads%>%
  filter(spending_change>-0.3)%>%
ggplot()+
  geom_point(aes(x = spending_change, y = change, color = imd_rank))+
  geom_smooth(aes(x = spending_change, y = change), method = "loess")+
  scale_color_viridis_c()

library(lme4)

df$imd_rank<- scale(df$imd_rank)
df$proportion_aged_65_years_and_over

m<- lmer(change ~ scale(imd_rank) + spending_change + I(spending_change^2) + proportion_aged_65_years_and_over + (1|lad22cd) , data = shp)

summary(m)

ggplot(df)+
  geom_point(aes(x = spending_change, y = age_standardised_rate))+
  geom_smooth(aes(x = spending_change, y = age_standardised_rate))
