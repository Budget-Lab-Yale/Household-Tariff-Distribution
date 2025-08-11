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

  # Read microdata for 2026-2034
  tax_simulator_microdata = 2026:2034 %>% 
    map_dfr(
      .f = ~ paste0('./resources/tax-simulator/microdata/baseline/', .x, '.csv') %>% 
        fread() %>% 
        tibble() %>% 
        filter(dep_status == 0) %>% 
        select(id, weight, filing_status, n_dep, expanded_inc, liab_iit_net, liab_pr_ee) %>% 
        left_join(
          paste0('./resources/tax-simulator/microdata/tariffs/', .x, '.csv') %>% 
            fread() %>% 
            tibble() %>%  
            select(id, liab_iit_net_tariffs = liab_iit_net), 
          by = 'id'
        ) %>% 
        mutate(year = .x, .before = everything())
    )
  

  tax_offset = tax_simulator_microdata %>% 
    
    # Deflate to 2015 dollars
    left_join(
      read_csv('./resources/macro-projections/projections.csv', show_col_types = F) %>% 
        mutate(inflation_index = cpiu / cpiu[year == 2025]) %>% 
        select(year, inflation_index), 
      by = 'year'
    ) %>% 
    
    # Calculate quintiles of equivalized posttax income
    group_by(year) %>% 
    filter(expanded_inc - liab_iit_net - liab_pr_ee > 0) %>% 
    mutate(
      equiv_atti = (expanded_inc - liab_iit_net - liab_pr_ee) / sqrt(1 + (filing_status == 2) + n_dep), 
      decile = cut(
        x      = equiv_atti,
        breaks = c(0, wtd.quantile(x = equiv_atti, weights = weight, probs = seq(0.1, 0.9, 0.1)), Inf), 
        labels = F
      )
    ) %>% 
    
    # Calculate average change in liability over the window
    group_by(decile) %>% 
    summarise(
      tax_offset = sum(((liab_iit_net_tariffs - liab_iit_net) / inflation_index) * weight) / sum(weight), 
      .groups = 'drop'
    )
    
  # Save output
  write_csv(tax_offset, './resources/tax-simulator/summary/tax_offset.csv')

#---------------------------------------------
# ...otherwise read pre-calculated tax offset
#---------------------------------------------

} else {
  tax_offset = read_csv('./resources/tax-simulator/summary/tax_offset.csv')
}
