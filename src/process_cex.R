#------------------------------------------------------------------------------
# process_cex.R
#
# Loads 2019 SCF and extracts consumption shares by quintiles of eqivalized
# income
#------------------------------------------------------------------------------


build_table = function(year) {
  
  #---------------------
  # open and clean data
  #---------------------
  
  # current quarter expenditures
  expcq = c('ETOTALC', 'TOTEXPCQ')
  # prior quarter expenditures
  exppq = c('ETOTALP', 'TOTEXPPQ')
  # income variables
  inc = c('FINCBTXM', 'FINATXEM', 'FSALARYM', 'FSMPFRXM')
  
  names = c('Before-tax income', 'After-tax income', 'Salary income', 'Self-employment income', 'Outlays', 'Expenditures')
  
  fmli = lapply(dir(path       = 'resources/cex',
                    pattern    = paste0('^fmli((', year%%100, ')[1-4]|(', year%%100+1, '1))[.]csv$'),
                    full.names = TRUE),
                fread,
                select = c('NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', 'FAM_SIZE', inc, expcq, exppq)) %>%
    bind_rows() %>%
    
    mutate(
      # equivalized after-tax income
      ati_equiv = FINATXEM/sqrt(FAM_SIZE)
    ) %>%
    
    rename_with(.fn   = ~gsub('(C|CQ)$', '_CQ', .x),
                .cols = all_of(expcq)) %>%
    rename_with(.fn   = ~gsub('(P|PQ)$', '_PQ', .x),
                .cols = all_of(exppq))
  
  
  #----------------------
  # define new variables
  #----------------------
  
  fmli = fmli %>%
    mutate(
      # quantiles by equivalized after-tax income
      pctile = cut(x              = ati_equiv,
                   breaks         = c(-Inf,
                                      wtd.quantile(x       = fmli$ati_equiv,
                                                   probs   = c(.2,.4,.6,.8),
                                                   weights = fmli$FINLWT21),
                                      Inf),
                   labels         = c(0,.2,.4,.6,.8)*100,
                   include.lowest = TRUE),
      pctile = as.numeric(levels(pctile))[pctile]
    ) %>%
    mutate(
      
      # population weights adjusted for scope as per
      # https://www.bls.gov/cex/pumd-getting-started-guide.htm#section6
      moscope = ifelse(QINTRVMO >3,
                       3,
                       ifelse(QINTRVYR == year,
                              QINTRVMO - 1,
                              4 - QINTRVMO)),
      popwt   = FINLWT21/4 * moscope/3,
      
      # expenditure variables adjusted for scope
      #   for q1, only current quarter expenditures
      #   for q2-4, sum of current quarter and last quarter expenditures
      #   for q5, only past quarter expenditures
      across(.cols = all_of(gsub('(C|CQ)$', '_CQ', expcq)),
             .fns  = ~ .x * (QINTRVYR == year) +
               get(gsub('CQ$', 'PQ', cur_column())) * (QINTRVMO > 3 |
                                                         QINTRVYR != year),
             .names = '{gsub("CQ$", "exp", .col)}'),
    )
  
  #---------------
  # compute means
  #---------------
  
  fmli %>%
    mutate(year = year) %>%
    group_by(year, pctile) %>%
    summarise(
      across(.cols = all_of(inc),
             .fns  = ~ weighted.mean(x = .x, w = FINLWT21)),
      across(.cols = ends_with('_exp'),
             .fns  = ~ (sum(.x * FINLWT21) / sum(popwt))),
      .groups = 'drop'
    ) %>%
    setnames(c('year', 'pctile', names)) %>% 
    return()
}


cex_estimates = build_table(2019)
