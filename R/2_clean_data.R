# clean, reduce dfs in prep for analysis
clean_data = function(data = ls_df, spc, ls) {
 
  # Chinook, summer parr
  if(spc == "chnk" & ls == "sum") {
    spc_ls_hab_df = ls_df %>%
      filter(Species == 'Chinook') %>%
      select(Watershed, Year, StreamName, Channel_Type, Lat, Lon,
             N, fish_dens_m = fish_dens,
             FishSiteLength, FishWettedArea, CUMDRAINAG, MeanU, Q,         # size
             WetWdth_Int, WetBraid, WetWdth_Avg, DpthThlwg_Avg, Area_Wet,
             DistPrin1, NatPrin1, NatPrin2,                                # PCA
             SlowWater_Pct, SlowWater_Freq, FstTurb_Pct, FstTurb_Freq,     # channel units
             FstNT_Pct, FstNT_Freq, CU_Freq,
             Grad, Sin, DetrendElev_SD, DpthThlwg_UF_CV, DpthWet_SD,       # complexity
             WetWdth_CV, WetWDRat_Avg, PoolResidDpth,  
             SC_Area_Pct, WetSC_Pct, SCSm_Freq,                            # side channel
             SubD16, SubD50, SubD84, SubEstGrvl, SubEstSandFines,          # substrate
             SubEstBldr, SubEstCbl, 
             Cond,                                                         # other
             RipCovBigTree, RipCovConif, RipCovNonWood, RipCovUstory,      # riparian cover
             RipCovWood, RipCovCanNone, RipCovUstoryNone, RipCovGrndNone, 
             LWVol_Wet, LWVol_WetSlow, LWVol_WetFstTurb, LWVol_WetFstNT,   # large wood
             LWFreq_Wet, 
             Ucut_Area,  UcutLgth_Pct, UcutArea_Pct,                       # undercuts
             FishCovLW, FishCovTVeg, FishCovArt, FishCovNone, FishCovAqVeg,# fish cover
             FishCovTotal) %>%
      filter(fish_dens_m > 0) %>%
      mutate(fish_dens_m2 = N / Area_Wet,
             log_fish_dens_m = log(fish_dens_m + 0.005),
             log_fish_dens_m2 = log(fish_dens_m2 + 0.005))
  }  
  
  # Chinook, winter presmolt
  if(spc == "chnk" & ls == "win") {
    spc_ls_hab_df = ls_df %>%
      filter(Species == 'Chinook') %>%
      select(Watershed, Year, Tier1_fish, Tier2_fish, Tier1, Tier2,
             N, fish_dens_m = fish_dens, AreaTotal, Area_Wet,
             Dpth_Max, DpthThlwgExit, DpthResid,                            # depth
             FishCovLW, FishCovTVeg, FishCovArt, FishCovAqVeg, FishCovNone, # fish cover
             FishCovAll,
             SubEstBdrk, SubEstBldr, SubEstCbl, SubEstGrvl, SubEstSandFines,# substrate 
             SubEstCandBldr, SubD50, 
             n_UC, Ucut_Length, Ucut_Area, UcutArea_Pct,                    # undercut
             Sin, CU_Freq, Discharge, Temp, LWCount) %>%                    # other
      filter(fish_dens_m > 0) %>%
      mutate(fish_dens_m2 = N / Area_Wet,
             log_fish_dens_m = log(fish_dens_m + 0.005),
             log_fish_dens_m2 = log(fish_dens_m2 + 0.005))
  }
  
  # Chinook, spawning
  if(spc == "chnk" & ls == "spw") {
    spc_ls_hab_df = ls_df %>%
      filter(Species == 'Chinook') %>%
      select(Watershed, Stream, Channel_Type, Year = maxYr,              # site
             # note fish_dens_m is redds/km
             fish_dens_m = maxReddsPerKm, LON_DD, LAT_DD, 
             CUMDRAINAG, MeanU, Q, Area_Wet, WetWdth_Int, WetBraid,      # size
             WetWdth_Avg, DpthThlwg_Avg, 
             NatPrin1, NatPrin2, DistPrin1,                              # PCA
             SlowWater_Freq, FstTurb_Freq, FstNT_Freq, FstTurb_Pct,      # channel units
             FstNT_Ct, FstNT_Pct, CU_Freq, 
             Grad, Sin, DetrendElev_SD, DpthThlwg_UF_CV, DpthWet_SD,     # complexity
             Sin_CL,
             WetWdth_CV, WetWDRat_CV, WetWDRat_Avg, PoolResidDpth, 
             WetSC_Pct, SCSm_Freq, SC_Area_Pct,                          # side channel
             SubEmbed_Avg, SubEmbed_SD, SubD16, SubD50, SubD84,          # substrate
             SubEstGrvl, SubEstSandFines, SubEstBldr, SubEstCbl, 
             Elev_M, Cond, avg_aug_temp,                                 # other
             RipCovBigTree, RipCovConif, RipCovGrnd, RipCovNonWood,      # riparian cover
             RipCovUstory, RipCovWood, RipCovCanNone, RipCovUstoryNone,
             RipCovGrndNone,
             LWVol_Wet, LWVol_WetSlow, LWVol_WetFstTurb, LWVol_WetFstNT, # large wood
             LWFreq_Wet, 
             Ucut_Area, UcutLgth_Pct, UcutArea_Pct,                       # undercuts
             FishCovLW, FishCovTVeg, FishCovArt, FishCovNone,             # fish cover
             FishCovAqVeg, FishCovTotal) %>%
      filter(fish_dens_m > 0) %>%
      filter(!Watershed %in% c('Tucannon'),
             !Channel_Type %in% c('Cascade', 'Plane-bed', 'Step-pool')) %>%# too little data
      mutate(N = NA) %>%
      mutate(fish_dens_m2 = N / Area_Wet,
             log_fish_dens_m = log(fish_dens_m + 0.005),
             log_fish_dens_m2 = log(fish_dens_m2 + 0.005))
  }
  
  # Steelhead, summer parr
  if(spc == "sthd" & ls == "sum") {
    spc_ls_hab_df = ls_df %>%
      filter(Species == 'Steelhead') %>%
      select(Watershed, Year, StreamName, Channel_Type, Lat, Lon,          # site
             N, fish_dens_m = fish_dens, 
             FishSiteLength, FishWettedArea, CUMDRAINAG, MeanU, Q,         # size
             WetWdth_Int, WetBraid, WetWdth_Avg, DpthThlwg_Avg, Area_Wet,
             DistPrin1, NatPrin1, NatPrin2,                                # PCA
             SlowWater_Pct, SlowWater_Freq, FstTurb_Pct, FstTurb_Freq,     # channel units
             FstNT_Pct, FstNT_Freq, CU_Freq,
             Grad, Sin, DetrendElev_SD, DpthThlwg_UF_CV, DpthWet_SD,       # complexity
             WetWdth_CV, WetWDRat_Avg, PoolResidDpth,  
             SC_Area_Pct, WetSC_Pct, SCSm_Freq,                            # side channel
             SubD16, SubD50, SubD84, SubEstGrvl, SubEstSandFines,          # substrate
             SubEstBldr, SubEstCbl, 
             Cond,                                                         # other
             RipCovBigTree, RipCovConif, RipCovNonWood, RipCovUstory,      # riparian cover
             RipCovWood, RipCovCanNone, RipCovUstoryNone, RipCovGrndNone, 
             LWVol_Wet, LWVol_WetSlow, LWVol_WetFstTurb, LWVol_WetFstNT,   # large wood
             LWFreq_Wet, 
             Ucut_Area,  UcutLgth_Pct, UcutArea_Pct,                       # undercuts
             FishCovLW, FishCovTVeg, FishCovArt, FishCovNone, FishCovAqVeg,# fish cover
             FishCovTotal) %>%
      filter(fish_dens_m > 0) %>%
      mutate(fish_dens_m2 = N / Area_Wet,
             log_fish_dens_m = log(fish_dens_m + 0.005),
             log_fish_dens_m2 = log(fish_dens_m2 + 0.005))
  }
  
  # Steelhead, winter presmolt
  if(spc == "sthd" & ls == "win") {
    spc_ls_hab_df = ls_df %>%
      filter(Species == 'Steelhead') %>%
      select(Watershed, Year, Tier1_fish, Tier2_fish, Tier1, Tier2,
             N, fish_dens_m = fish_dens, AreaTotal, Area_Wet,
             Dpth_Max, DpthThlwgExit, DpthResid,                            # depth
             FishCovLW, FishCovTVeg, FishCovArt, FishCovAqVeg, FishCovNone, # fish cover
             FishCovAll,
             SubEstBdrk, SubEstBldr, SubEstCbl, SubEstGrvl, SubEstSandFines,# substrate 
             SubEstCandBldr, SubD50, 
             n_UC, Ucut_Length, Ucut_Area, UcutArea_Pct,                    # undercut
             Sin, CU_Freq, Discharge, Temp, LWCount) %>%                    # other
      filter(fish_dens_m > 0) %>%
      mutate(fish_dens_m2 = N / Area_Wet,
             log_fish_dens_m = log(fish_dens_m + 0.005),
             log_fish_dens_m2 = log(fish_dens_m2 + 0.005))
  }
  
  # Steelhead, spawning
  if(spc == "sthd" & ls == "spw") {
    spc_ls_hab_df = ls_df %>%
      filter(Species == 'Steelhead') %>%
      select(Watershed, Stream, Channel_Type, Year = maxYr,              # site
             # note fish_dens_m is redds/km
             fish_dens_m = maxReddsPerKm, LON_DD, LAT_DD, 
             CUMDRAINAG, MeanU, Q, Area_Wet, WetWdth_Int, WetBraid,      # size
             WetWdth_Avg, DpthThlwg_Avg, 
             NatPrin1, NatPrin2, DistPrin1,                              # PCA
             SlowWater_Freq, FstTurb_Freq, FstNT_Freq, FstTurb_Pct,      # channel units
             FstNT_Ct, FstNT_Pct, CU_Freq, 
             Grad, Sin, DetrendElev_SD, DpthThlwg_UF_CV, DpthWet_SD,     # complexity
             Sin_CL,
             WetWdth_CV, WetWDRat_CV, WetWDRat_Avg, PoolResidDpth, 
             WetSC_Pct, SCSm_Freq, SC_Area_Pct,                          # side channel
             SubEmbed_Avg, SubEmbed_SD, SubD16, SubD50, SubD84,          # substrate
             SubEstGrvl, SubEstSandFines, SubEstBldr, SubEstCbl, 
             Elev_M, Cond, avg_aug_temp,                                 # other
             RipCovBigTree, RipCovConif, RipCovGrnd, RipCovNonWood,      # riparian cover
             RipCovUstory, RipCovWood, RipCovCanNone, RipCovUstoryNone,
             RipCovGrndNone,
             LWVol_Wet, LWVol_WetSlow, LWVol_WetFstTurb, LWVol_WetFstNT, # large wood
             LWFreq_Wet, 
             Ucut_Area, UcutLgth_Pct, UcutArea_Pct,                       # undercuts
             FishCovLW, FishCovTVeg, FishCovArt, FishCovNone,             # fish cover
             FishCovAqVeg, FishCovTotal) %>%
      filter(fish_dens_m > 0) %>%
      filter(!Watershed %in% c('John Day'),
             !Channel_Type %in% c('Confined', 'Meandering')) %>%
      mutate(N = NA) %>%
      mutate(fish_dens_m2 = N / Area_Wet,
             log_fish_dens_m = log(fish_dens_m + 0.005),
             log_fish_dens_m2 = log(fish_dens_m2 + 0.005))
  }

  return(spc_ls_hab_df)  
   
}