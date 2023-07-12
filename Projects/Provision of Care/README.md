Provision of Unpaid Care
================

This project looks at who is providing unpaid care in 2021 according to the England and Wales census. The aim is to look at whether the provision of unpaid care has increased or decreased since the last census in 2011, and how the socio-demographic profile of unpaid carers has changed over that period.

### Workflow:

  - Step 1: Calculate raw proportions and age-standardised rates of provision of unpaid care in 2021 and 2011 in England and Wales:
    - [provision_of_unpaid_care_rates_indirect_standardisation.R](Analysis/Overall%20Analysis/provision_of_unpaid_care_rates_indirect_standardisation.R)
  - Step 2: Decompose unpaid carer count change into population growth, ageing and underlying rate change for the change between 2011 and 2021 in England and Wales:
    - [unpaid_carer_change_decomposition.R](Analysis/Overall%20Analysis/unpaid_carer_change_decomposition.R)
  - Step 3: Calculate rates for 2021 across socio-demographic subpopulations in England and Wales:
    - [unpaid_carer_demographic_profile.R](Analysis/Overall%20Analysis/unpaid_carer_demographic_profile.R)
    - [unpaid_carer_socioeconomic_profile.R](Analysis/Overall%20Analysis/unpaid_carer_socioeconomic_profile.R)
  - Step 4: Calculate change between 2011 and 2021 in rates across socio-demographic subpopulations in England and Wales:
    - [unpaid_carer_demographic_change.R](Analysis/Overall%20Analysis/unpaid_carer_demographic_change.R)
    - [unpaid_carer_socioeconomic_change.R](Analysis/Overall%20Analysis/unpaid_carer_socioeconomic_change.R)
  - Step 5: Calculate age-standardised rate in 2021 for each LSOA:
    - [unpaid_carer_lsoa_rates.R](Analysis/Geographic%20Analysis/unpaid_carer_lsoa_rates.R)
  - Step 6: Model the LSOA age-standardised rates


**_As the project progresses, I will add plots and a brief write-up of the output. This output will form the basis for a report._**

### Step 1: Overall Provision of Unpaid Care Rates

The below plot shows the raw proportion of people providing unpaid care in 2011 and 2021 in England and Wales. Proportions are shown both for overall care and for care separated by the number of hours provided per week.

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">

<img src="Analysis/Overall Analysis/Plots/Raw Provision of Unpaid Care Rates 2021 and 2011.png" style="width: 100%;" alt="Click to see the source">

</div>

<br>

In order to account for the underlying change in the population's age structure between 2011 and 2021, the table below shows the age-standardised rate for 2021 alongside the raw rates.

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">

<img src="Analysis/Overall Analysis/Tables/Raw and Age-Standardised Unpaid Caring Rates in 2011 and 2021.svg" style="width: 100%;" alt="Click to see the source">

</div>

### Step 2: Decompose Change in Unpaid Carers

The below plot shows the observed change in the count of unpaid carers between 2011 and 2021 and then breaks it down into the following components:

  - **Population growth component**: the change in the count that we would expect to see from the overall change in the size of the population.
  - **Population ageing component**: the change in the count that we would expect to see from the change in the age structure of the population.
  - **Rate change component**: the change in the count that is attributable to a change in the underlying likelihood of the event occurring, independent of population growth and ageing.

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">

<img src="Analysis/Overall Analysis/Plots/Components of Observed Change in Count of Unpaid Carers.png" style="width: 100%;" alt="Click to see the source">

</div>

### Step 3: Provision of Unpaid Care Rates by Subpopulation

The below plot shows the raw proportion of men and women in each age group providing unpaid care. The skew towards women taking on this work is visible, particularly in middle age.

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">

<img src="Analysis/Overall Analysis/Plots/Proportion Providing Unpaid Care by Age and Sex 2021.png" style="width: 100%;" alt="Click to see the source">

</div>

<br>

The below plots shows the age-standardised caring rates across various socio-demographic variables.

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">

<img src="Analysis/Overall Analysis/Plots/Age-Standardised Caring Rates across Socio-Demographic Variables 2021.png" style="width: 100%;" alt="Click to see the source">

</div>


### Step 4: Change in Provision of Unpaid Care across Subpopulations

Although overall the rates of unpaid care provision have decreased, this decrease is not evenly spread across the population. For this reason, the below looks at how the rate has changed within different subpopulations.

Firstly, we can simply look at how the raw proportions providing unpaid care have changed across age-sex groups and ethnic groups between 2011 and 2021.

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">

<img src="Analysis/Overall Analysis/Plots/Change in Proportion Providing Unpaid Care by Age and Sex 2021 and 2011.png" style="width: 100%;" alt="Click to see the source">

</div>

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">

<img src="Analysis/Overall Analysis/Plots/Change in Proportion Providing Unpaid Care by Ethnicity 2021 and 2011.png" style="width: 100%;" alt="Click to see the source">

</div>

But we can also think about whether these different changes in the raw proportions have led to unpaid care being spread more or less evenly across these subpopulations.

The below shows how the provision of unpaid care has become even more gendered over the last decade. In 2011, women had a higher odds than men of providing unpaid care, but in 2021 that difference became even bigger.

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">

<img src="Analysis/Overall Analysis/Plots/Odds Ratio for Providing Unpaid Care by Sex 2021 and 2011.png" style="width: 100%;" alt="Click to see the source">

</div>

On the other hand, unpaid care provision has become more evenly spread across age groups, with those in later middle age having less elevated odds of providing unpaid care.

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">

<img src="Analysis/Overall Analysis/Plots/Odds Ratios for Providing Unpaid Care by Age Group 2021 and 2011.png" style="width: 100%;" alt="Click to see the source">

</div>



We can also consider how the distribution of unpaid care across ethnicities. However, instead of looking at odds ratios, we can use indirectly age-standardised rates in order to control for the differences in ethnic groups' age structures. Some ethnicities - Gypsy/Irish travellers and some mixed ethnic groups - have become more strongly associated with providing unpaid care over the last decade, in that their rates have become even more elevated compared to the general population. On the other hand, the rates of providing care for Pakistani and Bangladeshi individuals have moved towards being more in line with the general population of the last decade, though they still experience higher than average rates. Notably, Indian individuals have gone from being slightly more likely to provide unpaid care than the general population to slightly less likely.  

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">

<img src="Analysis/Overall Analysis/Plots/Age-Standardised Caring Rate by Ethnicity 2021 and 2011.png" style="width: 100%;" alt="Click to see the source">

</div>


#### Step 5: Calculate unpaid care rates for each LSOA


The amount of unpaid care being provided also varies geographically. The below maps the proportion of the population providing unpaid care in each LSOA (small neighbourhoods) for 2021.

<div style="width: 100%; padding-top: 20px; padding-bottom: 20px">
<div style ="width: 60%; margin: auto;">
<img src="Analysis/Geographic Analysis/Plots/Proportion Providing Unpaid Care by LSOA 2021.png" style="width: 100%;" alt="Click to see the source">
</div>
</div>

In order to look at the change between 2011 and 2021 in each neighbourhood, we can look at the rate of care provision in an area indirectly age-standardised to the age structure of that area's 2011 population.


<div style="width: 100%; padding-top: 20px; padding-bottom: 20px;">
<div style ="width: 60%; margin: auto;">
<img src="Analysis/Geographic Analysis/Plots/Age-Standardised Unpaid Care Provision Rates by LSOA 2021.png" style="width: 100%;" alt="Click to see the source">
</div>
</div>
