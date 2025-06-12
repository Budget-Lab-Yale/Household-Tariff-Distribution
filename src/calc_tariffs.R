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
    income_group = case_when(
      income_group == 'lowest_quintile'  ~ 1, 
      income_group == 'second_quintile'  ~ 2, 
      income_group == 'middle_quintile'  ~ 3, 
      income_group == 'fourth_quintile'  ~ 4, 
      income_group == 'highest_quintile' ~ 5 
    )
  ) %>% 
  
  # Join CEX 
  left_join(
    cex_estimates %>% 
      mutate(
        income_group = case_when(
          pctile == 0  ~ 1, 
          pctile == 20 ~ 2, 
          pctile == 40 ~ 3, 
          pctile == 60 ~ 4, 
          pctile == 80 ~ 5, 
        ), 
        cy_ratio.cex = Outlays / `After-tax income`
      ) %>% 
      select(year, income_group, c.cex = Outlays, cy_ratio.cex),
    by = c('income_group', 'year')
  ) %>% 
  
  # Join tax offset
  left_join(tax_offset, by = 'income_group') %>% 
  
  # Calculate implied consumption 
  mutate(
    share_c  = c.cex / sum(c.cex),
    c        = (pce_2019 * 1e9 * share_c) / (num_households * 1e6),
    cy_ratio = c / inc_after_transfers_taxes
  ) %>% 
  
  # Calculate implied tariff burden, in 2019 terms
  mutate(
    
    # Direct burden is higher prices
    direct_burden = -c * pce_effect$by_quintile, 
    
    # Benefit offset from indexed cash programs
    benefit_offset = (social_security + ssi + snap) * pce_effect$overall, 
    
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
  ) %>% 
  
  # Calculate excise tax shares for comparison
  mutate(
    share_excise = excise_taxes * num_households  / sum(excise_taxes * num_households)
  )

#--------
# Output
#--------

# Write data table
data_table = combined %>% 
  mutate(
    `Income Quintile` = factor(income_group, 
                               levels = 1:5, 
                               labels = c('Bottom quintile', 'Second quintile', 'Middle quintile', 'Fourth quintile', 'Top quintile')),
    `OBBBA (via CBO)` = round(cbo_obbba, 3),
    Tariffs           = round(direct_burden.pct_chg_atti, 3), 
    Total             = round(direct_burden.pct_chg_atti + cbo_obbba, 3)  
  ) %>% 
  select(contains(' '), Tariffs, Total) 

data_table %>% 
  write_csv('./output/table.csv')

# Produce contribution chart
data_table %>%
  rename(`New 2025 Tariffs as of June 1st` = Tariffs) %>% 
  pivot_longer(-c(`Income Quintile`, Total)) %>% 
  ggplot(aes(x = `Income Quintile`, y = value, fill = name)) +
  geom_col() +
  geom_point(aes(y = Total), size = 5, show.legend = F) + 
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
  scale_x_discrete(labels = c('Bottom quintile', 'Second quintile', 'Middle quintile', 'Fourth quintile', 'Top quintile')) +
  
  scale_y_continuous(
    labels = scales::percent_format(), 
    breaks = seq(-0.05, 0.03, 0.01)
  ) +
  scale_fill_brewer(palette = 'Set1') + 
  ggtitle(
    'Figure 1. Combined Effects of the House-Passed OBBBA and Tariffs, 2026', 
    subtitle = 'Percent Change in Income After Taxes and Transfers'
  )
  