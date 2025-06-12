#------------------------------------------------------------------------------
# main.R 
# 
# Runs code for analysis of tariff distribution by household income
# 
# Contact: 
# - John Ricco (john.ricco@yale.edu)
# - Maddie Lee (maddie.lee.ml3273@yale.edu)
#------------------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(Hmisc)

#----------------
# Set parameters
#----------------

# Set parameters
pce_2019 = 14437.5 # Source: BEA
pce_2026 = 21504   # Source: CBO outlook

# Effect of June 1st tariffs on PCE price index 
# (exogenous input from Ernie's model, here: https://budgetlab.yale.edu/research/state-us-tariffs-june-1-2025)
pce_effect = list(
  overall     = 0.0149417, 
  by_quintile = c(0.015145, 0.015381, 0.015636, 0.015773, 0.014535)
) 

# CBO estimates of the distributional impact of OBBBA
cbo_obbba = c(-0.02, -0.005, 0.01, 0.02, 0.03)

# Whether to load precalculate tax offset or not (T if external to TBL)
load_precalculated_tax_offset = F

#--------------
# Run analysis
#--------------

# Estimate consumption shares by equivalized after-tax-and-transfer income using CEX
source('./src/process_cex.R')

# Calculate or read tax offset
source('./src/calc_tax_offset.R')

# Calculate distributional tariff effects based on CBO distribution of household income
source('./src/calc_tariffs.R')

