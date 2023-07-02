Provision of Unpaid Care
================

This project looks at who is providing unpaid care in 2021 according to the England and Wales census. The aim is to look at whether the provision of unpaid care has increased or decreased since the last census in 2011, and how the socio-demographic profile of unpaid carers has changed over that period.

### Workflow:

  - Step 1: Calculate raw proportions and age-standardised rates of provision of unpaid care in 2021 and 2011 in England and Wales:
    - [provision_of_unpaid_care_rates_indirect_standardisation.R](Analysis/provision_of_unpaid_care_rates_indirect_standardisation.R)
  - Step 2: Calculate rates for 2021 across socio-demographic subpopulations in England and Wales:
    - [unpaid_carer_demographic_profile.R](Analysis/unpaid_carer_demographic_profile.R)
    - [unpaid_carer_socioeconomic_profile.R](Analysis/unpaid_carer_socioeconomic_profile.R)
  - Step 3: Calculate change between 2011 and 2021 in rates across socio-demographic subpopulations in England and Wales:
    - [unpaid_carer_demographic_change.R](Analysis/unpaid_carer_demographic_change.R)
    - [unpaid_carer_socioeconomic_change.R](Analysis/unpaid_carer_socioeconomic_change.R)
  - Step 4: Calculate age-standardised rate in 2021 for each LSOA
  - Step 5: Model the LSOA age-standardised rates


**_As the project progresses, I will add plots and a brief write-up of the output. This output will form the basis for a report._** 

### Step 1: Overall Provision of Unpaid Care Rates

The below plot shows the raw proportion of people providing unpaid care in 2011 and 2021 in England and Wales. Proportions are shown both for overall care and for care separated by the number of hours provided per week.

<div style="width: 100%; padding-top: 10px;">

<img src="Analysis/Plots/Raw Provision of Unpaid Care Rates 2021 and 2011.png" style="width: 100%;" alt="Click to see the source">

</div>

<br>

In order to account for the underlying change in the population's age structure between 2011 and 2021, the table below shows the age-standardised rate for 2021 alongside the raw rates.

<div style="width: 100%; padding-top: 10px;">

<img src="Analysis/Tables/Raw and Age-Standardised Unpaid Caring Rates in 2011 and 2021.svg" style="width: 100%;" alt="Click to see the source">

</div>

### Step 2: Provision of Unpaid Care Rates by Subpopulation

The below plot shows the raw proportion of men and women in each age group providing unpaid care. The heavy skew towards women taking on this work is visible, particularly in middle age. The higher proportion for the oldest men compared to the oldest women is in part driven by the fact that the male population over 85 is smaller than the female population - this can be seen in the second plot below, which compared the age-sex structure of the unpaid carer population with the general population.

<div style="width: 100%; padding-top: 10px;">

<img src="Analysis/Plots/Proportion Providing Unpaid Care by Age and Sex 2021.png" style="width: 100%;" alt="Click to see the source">

</div>

<br>

<div style="width: 100%; padding-top: 10px;">

<img src="Analysis/Plots/Age-Sex Structure of Unpaid Carers Compared to General Population 2021.png" style="width: 100%;" alt="Click to see the source">

</div>

<br>

The below plots shows the age-standardised caring rates across various socio-demographic variables.

<div style="width: 100%; padding-top: 10px;">

<img src="Analysis/Plots/Age-Standardised Caring Rates across Socio-Demographic Variables 2021.png" style="width: 100%;" alt="Click to see the source">

</div>


### Step 3: Change in Provision of Unpaid Care across Subpopulations

The below plots show how the raw proportions providing unpaid care have changed across age-sex groups and ethnic groups between 2011 and 2021.

<div style="width: 100%; padding-top: 10px;">

<img src="Analysis/Plots/Change in Proportion Providing Unpaid Care by Age and Sex 2021 and 2011.png" style="width: 100%;" alt="Click to see the source">

</div>

<div style="width: 100%; padding-top: 10px;">

<img src="Analysis/Plots/Change in Proportion Providing Unpaid Care by Ethnicity 2021 and 2011.png" style="width: 100%;" alt="Click to see the source">

</div>

While for almost every ethnic group, the raw proportion providing unpaid care has decreased over the last decade, for some groups it has decreased more than others - namely, White British and South Asian ethnicities.

The below plot looks at the change in the age-standardised rate by ethnicity, between 2011 and 2021. Some ethnicities - Gypsy/Irish travellers and some mixed ethnic groups - have become more strongly associated with providing unpaid care over the last decade, in that their rates have become even more elevated compared to the general population. On the other hand, the rates of providing care for Pakistani and Bangladeshi individuals have moved towards being more in line with the general population of the last decade, though they still experience higher than average rates. Notably, Indian individuals have gone from being slightly more likely to provide unpaid care than the general population to slightly less likely.  

<div style="width: 100%; padding-top: 10px;">

<img src="Analysis/Plots/Age-Standardised Caring Rate by Ethnicity 2021 and 2011.png" style="width: 100%;" alt="Click to see the source">

</div>
