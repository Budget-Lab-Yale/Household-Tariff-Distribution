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

# Effect of June 1st tariffs on PCE price index 
# (exogenous input from Ernie's model, here: https://budgetlab.yale.edu/research/state-us-tariffs-june-1-2025)
pce_effect = list(
  overall   = 0.01832, 
  by_decile = c(0.0199, 0.0181, 0.0190, 0.0183, 0.0185, 0.0191, 0.0193, 0.0187, 0.0182, 0.0170)
)

# Whether to load precalculate tax offset or not (T if external to TBL)
load_precalculated_tax_offset = T

# OBBBA version ('house' or 'passed')
obbba_version = 'passed'

#--------------
# Run analysis
#--------------

# Estimate consumption shares by equivalized after-tax-and-transfer income using CEX
source('./src/process_cex.R')

# Calculate or read tax offset
source('./src/calc_tax_offset.R')

# Calculate distributional tariff effects based on CBO distribution of household income
source('./src/calc_tariffs.R')

