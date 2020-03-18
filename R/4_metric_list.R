# generate metric list for comparisons and plotting
metric_list = function(ls) {
  
  # Summer parr
  if(ls == "sum") {
    size = c("FishSiteLength", "FishWettedArea", "CUMDRAINAG", "MeanU", 
             "Q", "WetWdth_Int", "WetBraid", "WetWdth_Avg", "DpthThlwg_Avg")
    pca  = c("DistPrin1", "NatPrin1", "NatPrin2")
    channel_units = c("SlowWater_Pct", "SlowWater_Freq", "FstTurb_Pct", "FstTurb_Freq", "FstNT_Pct", "FstNT_Freq", "CU_Freq")
    complexity = c("Grad", "Sin", "DetrendElev_SD", "DpthThlwg_UF_CV", "DpthWet_SD",  
                   "WetWdth_CV", "WetWDRat_Avg", "PoolResidDpth")
    side_channel = c("SC_Area_Pct", "WetSC_Pct", "SCSm_Freq")
    substrate = c("SubD16", "SubD50", "SubD84", "SubEstGrvl", "SubEstSandFines", "SubEstBldr", "SubEstCbl")
    other = c("Cond")
    riparian_cover = c("RipCovBigTree", "RipCovConif", "RipCovNonWood", "RipCovUstory", 
                       "RipCovWood", "RipCovCanNone", "RipCovUstoryNone", "RipCovGrndNone")
    large_wood = c("LWVol_Wet", "LWVol_WetSlow", "LWVol_WetFstTurb", "LWVol_WetFstNT", "LWFreq_Wet")
    undercuts = c("Ucut_Area",  "UcutLgth_Pct", "UcutArea_Pct")
    fish_cover = c("FishCovLW", "FishCovTVeg", "FishCovArt", "FishCovNone", "FishCovAqVeg", "FishCovTotal")
    
    metric_list = list(size = size, 
                     pca = pca, 
                     channel_units = channel_units, 
                     complexity = complexity, 
                     side_channel = side_channel, 
                     substrate = substrate, 
                     other = other,
                     riparian_cover = riparian_cover, 
                     large_wood = large_wood, 
                     undercuts = undercuts, 
                     fish_cover = fish_cover)
  }
  
  # Winter presmolt
  if(ls == "win") {
    depth = c('Dpth_Max', 'DpthThlwgExit', 'DpthResid')
    fish_cover = c('FishCovLW', 'FishCovTVeg', 'FishCovArt', 
                   'FishCovAqVeg', 'FishCovNone', 'FishCovAll')
    substrate = c('SubEstBdrk', 'SubEstBldr', 'SubEstCbl', 
                  'SubEstGrvl', 'SubEstSandFines', 'SubEstCandBldr', 'SubD50')
    undercut = c('n_UC', 'Ucut_Length', 'Ucut_Area', 'UcutArea_Pct')
    other = c('Sin', 'CU_Freq', 'Discharge', 'Temp', 'LWCount', 'LWDens')
    
    metric_list = list(depth = depth,
                     fish_cover = fish_cover,
                     substrate = substrate,
                     undercut = undercut,
                     other = other)
  }
  
  # Spawning
  if(ls == "spw") {
    size = c('CUMDRAINAG', 'MeanU', 'Q', 'Area_Wet', 'WetWdth_Int', 'WetBraid',
             'WetWdth_Avg', 'DpthThlwg_Avg') 
    pca = c('NatPrin1', 'NatPrin2', 'DistPrin1')
    channel_units = c('SlowWater_Freq', 'FstTurb_Freq', 'FstNT_Freq',
                      'FstTurb_Pct', 'FstNT_Ct', 'FstNT_Pct', 'CU_Freq') 
    complexity = c('Grad', 'Sin', 'DetrendElev_SD', 'DpthThlwg_UF_CV', 'DpthWet_SD',
                   'Sin_CL', 'WetWdth_CV', 'WetWDRat_CV', 'WetWDRat_Avg', 'PoolResidDpth') 
    side_channel = c('WetSC_Pct', 'SCSm_Freq', 'SC_Area_Pct')
    substrate = c('SubEmbed_Avg', 'SubEmbed_SD', 'SubD16', 'SubD50', 'SubD84',
                  'SubEstGrvl', 'SubEstSandFines', 'SubEstBldr', 'SubEstCbl') 
    other = c('Elev_M', 'Cond', 'avg_aug_temp')                                 
    riparian_cover = c('RipCovBigTree', 'RipCovConif', 'RipCovGrnd', 'RipCovNonWood',
                       'RipCovUstory', 'RipCovWood', 'RipCovCanNone', 'RipCovUstoryNone',
                       'RipCovGrndNone')
    large_wood = c('LWVol_Wet', 'LWVol_WetSlow', 'LWVol_WetFstTurb', 'LWVol_WetFstNT',
                   'LWFreq_Wet')
    undercuts = c('Ucut_Area', 'UcutLgth_Pct', 'UcutArea_Pct')
    fish_cover = c('FishCovLW', 'FishCovTVeg', 'FishCovArt', 'FishCovNone',
                   'FishCovAqVeg', 'FishCovTotal')
    
    metric_list = list(size = size, 
                     pca = pca, 
                     channel_units = channel_units, 
                     complexity = complexity, 
                     side_channel = side_channel, 
                     substrate = substrate, 
                     other = other,
                     riparian_cover = riparian_cover, 
                     large_wood = large_wood, 
                     undercuts = undercuts, 
                     fish_cover = fish_cover)
  }
  
  return(metric_list)

}