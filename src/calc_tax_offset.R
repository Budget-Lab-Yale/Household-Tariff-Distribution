#------------------------------------------------------------------------------
# calc_tax_offset.R
# 
# Calculates impact of tariffs on individual income tax revenue by 
# after-tax income group using results from TBL's Tax-Simlulator model. 
# Done at the tax unit level and not household level due to data limitations.
#------------------------------------------------------------------------------

#------------------------------------------------------
# If you have IRS PUF access and can run our models...
#------------------------------------------------------

if (!load_precalculated_tax_offset) {

  # Read microdata for 2026
  tax_simulator_microdata = fread('./resources/tax-simulator/microdata/2026.csv') %>% 
    tibble() %>% 
    left_join(
      fread('./resources/tax-simulator/microdata/tariffs_2026.csv') %>% 
        tibble() %>% 
        select(id, liab_iit_net_tariffs = liab_iit_net), 
      by = 'id'
    )
  
  tax_offset = tax_simulator_microdata %>% 
    filter(dep_status == 0) %>% 
    
    # Calculate quintiles of equivalized posttax income
    filter(expanded_inc - liab_iit - liab_pr_ee > 0) %>% 
    mutate(
      equiv_atti = (expanded_inc - liab_iit - liab_pr_ee) / sqrt(1 + (filing_status == 2) + n_dep), 
      decile = cut(
        x      = equiv_atti,
        breaks = c(0, wtd.quantile(x = equiv_atti, weights = weight, probs = seq(0.1, 0.9, 0.1)), Inf), 
        labels = F
      )
    ) %>% 
    
    # Calculate average change in liability
    group_by(decile) %>% 
    summarise(tax_offset = sum((liab_iit_net_tariffs - liab_iit_net) * weight) / sum(weight))
    
  # Save output
  write_csv(tax_offset, './resources/tax-simulator/summary/tax_offset.csv')

#---------------------------------------------
# ...otherwise read pre-calculated tax offset
#---------------------------------------------

} else {
  tax_offset = read_csv('./resources/tax-simulator/summary/tax_offset.csv')
}
