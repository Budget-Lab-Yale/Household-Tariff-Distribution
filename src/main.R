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
  overall   = 0.0149417, 
  by_decile = c(0.015475829, 
                0.014814164,
                0.015661254,
                0.015100163,
                0.015517472,	
                0.015755464,	
                0.016052386,	
                0.015493089,	
                0.015151623,
                0.013919) 
)

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

