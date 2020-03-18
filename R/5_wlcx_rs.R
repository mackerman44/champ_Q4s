# compare means by dataset and habitat covariate
wlcx_rs_test = function(df_list = df_list, metrics = metrics) {
  
  # list of habitat covariates
  hab_list = unlist(unique(metrics[1:length(metrics)]))
  
  # initiate blank df to store the results
  tst_results = setNames(data.frame(matrix(ncol = 3, nrow = length(df_list) * length(hab_list))),
                         c('data', 'hab_cov', 'p_value'))
  
  # now loop over data frames and habitat covariates and perform 
  # Wilcoxon rank sum test to compare means of Q4 vs. 'rest'
  ctr = 1
  for(d in 1:length(df_list)) {
    df = df_list[[d]]
    
    for(h in 1:length(hab_list)) {
      hc = hab_list[h]
      
      q4_data = pull(df[df$qrtl == 'Q4', hc])
      rest_data = pull(df[df$qrtl == 'Rest', hc])
      if(sum(is.na(q4_data)) == length(q4_data) |
         sum(is.na(rest_data)) == length(rest_data)) {
        tst_results[ctr, 1] = names(df_list)[[d]]
        tst_results[ctr, 2] = hc
        ctr = ctr + 1
        next
      }
      tst = try(wilcox.test(q4_data, rest_data))
      
      tst_results[ctr, 1] = names(df_list)[[d]]
      tst_results[ctr, 2] = hc
      tst_results[ctr, 3] = if_else(class(tst) == 'try-error',
                                    as.numeric(NA),
                                    tst$p.value)
      ctr = ctr + 1
      
    } # end habitat covariate loop
  } # end data frame loop
  
  return(tst_results)
  
}