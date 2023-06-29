Provision of Unpaid Care
================

This project looks at who is providing unpaid care in 2021 according to the England and Wales census. The aim is to look at whether the provision of unpaid care has increased or decreased since the last census in 2011, and how the socio-demographic profile of unpaid carers has changed over that period.

### Workflow:

  - Step 1: Calculate raw proportions and age-standardised rates of provision of unpaid care in 2021 and 2011 in England and Wales:
    - [provision_of_unpaid_care_rates_indirect_standardisation.R](Analysis/provision_of_unpaid_care_rates_indirect_standardisation.R)
  - Step 2: Calculate rates for 2021 across socio-demographic subpopulations in England and Wales:
    - [unpaid_carer_demographic_profile.R](Analysis/unpaid_carer_demographic_profile.R)
    - [unpaid_carer_socioeconomic_profile.R](Analysis/unpaid_carer_socioeconomic_profile.R)
  - Step 3: Calculate change between 2011 and 2021 in rates across socio-demographic subpopulations in England and Wales
  - Step 4: Calculate age-standardised rate in 2021 for each LSOA
  - Step 5: Model the LSOA age-standardised rates


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
