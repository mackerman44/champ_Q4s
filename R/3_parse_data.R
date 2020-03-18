# parse data by watershed, channel unit type, and/or tier 1, and remove any dataset too small (min_samp_size) for comparisons or plotting
parse_data = function(data = spc_ls_hab_df, spc, ls, min_samp_size = 20) {

  #------------------------------
  # parse data by watershed
  wtr = unique(spc_ls_hab_df$Watershed)
  for(w in wtr) {
    tmp = filter(spc_ls_hab_df, Watershed == as.character(w)) %>%
      mutate(qrtl = cut_number(log_fish_dens_m, n = 4, labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
      mutate(qrtl = recode(qrtl,
                           `Q1` = "Rest",
                           `Q2` = "Rest",
                           `Q3` = "Rest"))
    assign(paste(spc, ls, make_clean_names(w), sep = "_"), tmp) 
  }
  
  if(ls == "sum" | ls == "spw") {
    #------------------------------
    # parse data by channel_unit
    cht = unique(spc_ls_hab_df$Channel_Type)
    cht = cht[!is.na(cht)]
    for(c in cht) {
      tmp = filter(spc_ls_hab_df, Channel_Type == as.character(c)) %>%
        mutate(qrtl = cut_number(log_fish_dens_m, n = 4, labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
        mutate(qrtl = recode(qrtl,
                             `Q1` = "Rest",
                             `Q2` = "Rest",
                             `Q3` = "Rest"))
      assign(paste(spc, ls, make_clean_names(c), sep = "_"), tmp)  
    }
  }
  
  if(ls == "win")   {
    #------------------------------
    # parse data by tier 1
    tr1 = unique(spc_ls_hab_df$Tier1)
    for(t in tr1) {
      tmp = filter(spc_ls_hab_df, Tier1 == as.character(t)) %>%
        mutate(qrtl = cut_number(log_fish_dens_m, n = 4, labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
        mutate(qrtl = recode(qrtl,
                             `Q1` = "Rest",
                             `Q2` = "Rest",
                             `Q3` = "Rest"))
      assign(paste(spc, ls, make_clean_names(t), sep = "_"), tmp)
    }
  }
  
  #------------------------------
  # make a list of the parsed data frames
  df_list = ls(pattern = paste0("^",spc,"_",ls,"_"))
  df_list = do.call("list", mget(df_list))
  
  #------------------------------
  # find those dfs that are too small for comparisons or plotting
  big_dfs = names(which(sapply(df_list, nrow) > min_samp_size - 1, TRUE))
  df_list = df_list[names(df_list) %in% big_dfs]; rm(big_dfs)
  
  return(df_list)
    
}
