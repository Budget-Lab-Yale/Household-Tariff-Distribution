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

# Effect of tariffs on PCE price index 
pce_effect = list(
  overall   = 0.01234993908, 
  by_decile = c(0.012486111, 
                0.011646179, 
                0.012324758, 
                0.011979526, 
                0.01234642, 
                0.012620015, 
                0.012870445, 
                0.012613274, 
                0.012536559, 
                0.011980749) 
)

# Whether to load precalculate tax offset or not (T if external to TBL)
load_precalculated_tax_offset = F

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

