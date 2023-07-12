#######################################
#                                     #
#  UNPAID CARER SOCIOECONOMIC CHANGE  #
#                                     #
#######################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 14/06/2023

# DESCRIPTION: This file looks at the change in the socioeconomic profile of those providing unpaid care between the 2011 and 2021 censuses.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(systemfonts)
devtools::install_github("lrowleyabel/sociodemographics")
library(sociodemographics)
library(nomisr)
library(patchwork)

setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/General Research/Census 2021/Projects/Provision of Care/Analysis/Overall Analysis")
rm(list = ls())


#### STEP 1: NS-SEC ####


