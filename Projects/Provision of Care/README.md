Provision of Unpaid Care
================

This project looks at who is providing unpaid care in 2021 according to the England and Wales census. The aim is to look at whether the provision of unpaid care has increased or decreased since the last census in 2011, and how the socio-demographic profile of unpaid carers has changed over that period.

### Workflow:

  - Step 1: Calculate raw proportions and age-standardised rates of provision of unpaid care in 2021 and 2011 in England and Wales: [provision_of_unpaid_care_rates_indirect_standardisation.R](../Analysis/_provision_of_unpaid_care_rates_indirect_standardisation.R)
  - Step 2: Calculate age-standardised rates across socio-demographic subpopulations in England and Wales
  - Step 3: Calculate age-standardised rate in 2021 for each LSOA
  - Step 4: Model the LSOA age-standardised rates


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
