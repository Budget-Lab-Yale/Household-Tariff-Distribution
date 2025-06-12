#------------------------------------------------------------------------------
# calc_tariffs.R 
# 
# Calculates effects of tariffs in a way that's consistent with CBO's 
# distribution of household income products
#------------------------------------------------------------------------------

# Read CBO data
cbo_estimates1 = read_csv('./resources/cbo/households_ranked_by_inc_after_trans_tax_table_01_demographics_1979_2021.csv')
cbo_estimates3 = read_csv('./resources/cbo/households_ranked_by_inc_after_trans_tax_table_03_average_household_income_1979_2021.csv')
cbo_estimates5 = read_csv('./resources/cbo/households_ranked_by_inc_after_trans_tax_table_05_components_ibtt_1979_2021.csv')
cbo_estimates6 = read_csv('./resources/cbo/households_ranked_by_inc_after_trans_tax_table_06_components_means_tested_transfers_1979_2021.csv')
cbo_estimates7 = read_csv('./resources/cbo/households_ranked_by_inc_after_trans_tax_table_07_components_federal_taxes_1979_2021.csv')
cbo_obbba      = read_csv('./resources/cbo/obbba.csv')


# Combine CBO data with CEX
combined = cbo_estimates5 %>% 
  left_join(
    cbo_estimates3 %>% 
      select(household_type, income_group, year, inc_after_transfers_taxes), 
    by = c('household_type', 'income_group', 'year')) %>% 
  left_join(cbo_estimates6, by = c('household_type', 'income_group', 'year')) %>% 
  left_join(cbo_estimates7, by = c('household_type', 'income_group', 'year')) %>% 
  left_join(cbo_estimates1, by = c('household_type', 'income_group', 'year')) %>%  
  
  # Grab quintiles for 2019
  filter(
    year == 2019, 
    household_type == 'all_households', 
    str_sub(income_group, start = -9) == '_quintile', 
  ) %>% 
  mutate(
    quintile = case_when(
      income_group == 'lowest_quintile'  ~ 1, 
      income_group == 'second_quintile'  ~ 2, 
      income_group == 'middle_quintile'  ~ 3, 
      income_group == 'fourth_quintile'  ~ 4, 
      income_group == 'highest_quintile' ~ 5 
    )
  ) %>% 
  
  # Pare down to what we need
  mutate(social_security + ssi + snap) %>% 
  select(quintile, num_households, inc_after_transfers_taxes, social_security, ssi, snap) %>% 
  
  # Impute deciles
  expand_grid(row = 1:2) %>% 
  mutate(decile = 1:10) %>% 
  left_join(cbo_obbba, by = 'decile') %>% 
  group_by(quintile) %>% 
  mutate(
    
    # Split households in half
    num_households = num_households / 2,
    
    # Calculate within-quintile ratio of lower half to upper half
    ratio_atti = obbba.atti / mean(obbba.atti), 
    ratio_ss   = obbba.social_insurance / mean(obbba.social_insurance),
    ratio_snap = obbba.means_tested / mean(obbba.means_tested), 
    
    # Apply ratios
    inc_after_transfers_taxes = inc_after_transfers_taxes * ratio_atti, 
    social_security           = social_security * ratio_ss,
    ssi                       = social_security * ratio_ss,
    snap                      = snap * ratio_snap, 
    
    # Calculate total benefits with COLAs
    indexed_benefits = social_security + ssi + snap
  ) %>% 
  ungroup() %>% 
  select(decile, num_households, obbba.atti, obbba.pct_chg, obbba.avg_chg, inc_after_transfers_taxes, indexed_benefits) %>% 

  # Join CEX 
  left_join(
    cex_estimates %>% 
      mutate(cy_ratio.cex = Outlays / `After-tax income`) %>% 
      select(year, decile, c.cex = Outlays, cy_ratio.cex),
    by = 'decile'
  ) %>% 
  
  # Join tax offset
  left_join(tax_offset, by = 'decile') %>% 
  
  # Calculate implied consumption 
  mutate(
    share_c  = c.cex / sum(c.cex),
    c        = (pce_2019 * 1e9 * share_c) / (num_households * 1e6),
    cy_ratio = c / inc_after_transfers_taxes
  ) %>% 
  
  # Calculate implied tariff burden, in 2019 terms
  mutate(
    
    # Direct burden is higher prices
    direct_burden = -c * pce_effect$by_decile, 
    
    # Benefit offset from indexed cash programs
    benefit_offset = indexed_benefits * pce_effect$overall, 
    
    # Net burden
    net_burden = direct_burden + benefit_offset + tax_offset,
  ) %>% 
  
  # Express in relative terms
  mutate(
    across(
      .cols  = c(direct_burden, benefit_offset, tax_offset, net_burden), 
      .fns   = ~ . / inc_after_transfers_taxes, 
      .names = '{col}.pct_chg_atti'
    )
  ) 

# To get final dollar impacts, multiply percent changes by CBO ATTI 
results = combined %>% 
  select(decile, obbba.atti, obbba_avg = obbba.avg_chg, obbba_pctchg = obbba.pct_chg, tariffs_pctchg = net_burden.pct_chg_atti) %>% 
  mutate(tariffs_avg = obbba.atti * tariffs_pctchg)

#--------
# Output
#--------

# Write data table
data_table = results %>% 
  mutate(
    `Income Decile`          = decile,
    `Average income after transfers and taxes (2025 dollars)` = round(obbba.atti, -1),
    `OBBBA (via CBO)_pctchg` = round(obbba_pctchg, 3),
    Tariffs_pctchg           = round(tariffs_pctchg, 3),
    Total_pctchg             = round(tariffs_pctchg + obbba_pctchg, 3), 
    `OBBBA (via CBO)_avg`    = round(obbba_avg, -1),
    Tariffs_avg              = round(tariffs_avg, -1),
    Total_avg                = round(tariffs_avg + obbba_avg, -1)
  ) %>% 
  select(
    `Income Decile`,
    `Average income after transfers and taxes (2025 dollars)`,
    `OBBBA (via CBO)_pctchg`,
    Tariffs_pctchg,
    Total_pctchg,
    `OBBBA (via CBO)_avg`,
    Tariffs_avg,
    Total_avg
  )

dir.create('./output', showWarnings = F)

data_table %>% 
  write_csv('./output/table.csv')

# Produce contribution charts
data_table %>%
  select(`Income Decile`, ` OBBBA (via CBO)` = `OBBBA (via CBO)_pctchg`, `2025 Tariff Increases` = Tariffs_pctchg, Total_pctchg) %>%
  pivot_longer(-c(`Income Decile`, Total_pctchg)) %>% 
  ggplot(aes(x = `Income Decile`, y = value, fill = name)) +
  geom_col() +
  geom_point(aes(y = Total_pctchg), size = 5, show.legend = F) + 
  theme_minimal() + 
  geom_hline(yintercept = 0) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    legend.position = "top",
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    text = element_text(size = 12),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0) 
  ) + 
  guides(fill = guide_legend(ncol = 1)) + 
  labs(
    y = element_blank(), 
    fill = element_blank(),
    caption = "Source: The Budget Lab calculations"
  ) + 
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    labels = scales::percent_format(), 
    breaks = seq(-0.07, 0.03, 0.01)
  ) +
  scale_fill_brewer(palette = 'Set1') + 
  ggtitle(
    'Figure 1. Combined Effects of the House-Passed OBBBA and Tariffs', 
    subtitle = 'Average annual change in household resources as a percentage of current law income after transfers and taxes (2026–2034)'
  )

data_table %>%
  select(`Income Decile`, ` OBBBA (via CBO)` = `OBBBA (via CBO)_avg`, `2025 Tariff Increases` = Tariffs_avg, Total_avg) %>%
  pivot_longer(-c(`Income Decile`, Total_avg)) %>% 
  ggplot(aes(x = `Income Decile`, y = value, fill = name)) +
  geom_col() +
  geom_point(aes(y = Total_avg), size = 5, show.legend = F) + 
  theme_minimal() + 
  geom_hline(yintercept = 0) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    legend.position = "top",
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    text = element_text(size = 12),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0) 
  ) + 
  guides(fill = guide_legend(ncol = 1)) + 
  labs(
    y = element_blank(), 
    fill = element_blank(),
    caption = "Source: The Budget Lab calculations"
  ) + 
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    labels = scales::dollar_format(), 
    breaks = seq(-4000, 12000, 2000)
  ) +
  scale_fill_brewer(palette = 'Set1') + 
  ggtitle(
    'Figure 2. Combined Effects of the House-Passed OBBBA and Tariffs', 
    subtitle = 'Average annual change in household resources, 2025 dollars (2026–2034)'
  )

  