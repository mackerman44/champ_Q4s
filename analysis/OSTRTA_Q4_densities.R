# Author: Mike Ackerman
#  - contributions from Kevin See within
# 
# The One Script To Rule Them All (OSTRTA), here I just want to combine all of the scenarios evaluation within
# the mackerman44/champ_Q4s Git repo. Those scenarios including:
#
# Species: Chinook and steelhead
# Life Stages: summer juveniles (parr), winter juveniles (presmolts), redds
#
# Here we want to mine the CHaMP sites that have the highest fish densities and then explore habitat covariates within those sites
# to see whether something about a given covariate might explain (be correlated with) high densities. i.e. Do sites with high juvenile
# fish densities tend to have more LWD? The hope is to look at distributions of habitat covariates in those sites to see if we can
# gleam information about target conditions.
#
# Created: 3/17/2020 in preparation for making a .Rmd for the MRA reports
#
# Notes:
##########################################################################
rm(list = ls())

#------------------------------
# load libraries
library(tidyverse)
library(ggplot2)
library(janitor)

#------------------------------
# set parameters
spc = "sthd"         # species: either "chnk" or "sthd"
ls  = "spw"          # life_stage: "sum" summer parr; "win" winter presmolt; "spw" spawning (redds)

#------------------------------
# load data
loadRData = function(fileName) {
  # loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

if(ls == "sum") {
  ls_df = loadRData("../QRFcapacity/data/fh_sum_champ_2017.rda") # summer juvenile parr paired fish/CHaMP habitat data through 2017
}
if(ls == "win") {
  ls_df = loadRData("../QRFcapacity/data/fh_win_champ_2017.rda") # winter juvenile presmolts paired fish/CHaMP habitat data through 2017
}
if(ls == "spw") {
  ls_df = loadRData("../QRFcapacity/data/fh_redds_champ_2017.rda") # paired redd/CHaMP habitat data through 2017
}

#------------------------------
# filter data by species
if(spc == "chnk") {
  spc_ls_df = ls_df %>%  filter(Species == 'Chinook')
}
if(spc == "sthd") {
  spc_ls_df = ls_df %>%  filter(Species == 'Steelhead')
}

#------------------------------
# examine fish abundance and density info
if(ls %in% c("sum", "win")) {
  spc_ls_n_df = spc_ls_df %>%
    select(Year, Watershed, N, fish_dens_m = fish_dens, Area_Wet)  
}
if(ls == "spw") {
  spc_ls_n_df = spc_ls_df %>%
    # note fish_dens_m is redds/km
    select(Year = maxYr, Watershed, fish_dens_m = maxReddsPerKm, Area_Wet) %>%
    mutate(N = NA)
}

# filter out where abundance/density = 0
spc_ls_n_df = spc_ls_n_df %>%
  filter(fish_dens_m > 0) %>%
  mutate(fish_dens_m2 = N / Area_Wet,
         log_fish_dens_m = log(fish_dens_m + 0.005),
         log_fish_dens_m2 = log(fish_dens_m2 + 0.005))

#------------------------------
# calculate quartiles of linear and areal fish densities
qrtls_m  = quantile(spc_ls_n_df$log_fish_dens_m,  probs = c(0.00, 0.25, 0.50, 0.75, 1.00))
#if(ls %in% c("sum", "win")) {
#  qrtls_m2 = quantile(spc_ls_n_df$log_fish_dens_m2, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = T)
#}

# plot log transformed linear densities with quartiles
spc_ls_p = spc_ls_n_df %>%
  ggplot() +
  geom_histogram(aes(x = log_fish_dens_m, fill = Watershed), bins = 50) +
  geom_vline(xintercept = qrtls_m, color = "blue") +
  labs(x = "Areal Density (per m)",
       y = "Frequency",
       title = paste(spc, ls)) +
  theme_bw()
spc_ls_p

#------------------------------
# reduce dfs in preparation for evaluation
# Note: This should eventually be consolidated.

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

#------------------------------
# data parsing

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
min_samp_size = 20
big_dfs = names(which(sapply(df_list, nrow) > min_samp_size - 1, TRUE))
df_list = df_list[names(df_list) %in% big_dfs]; rm(big_dfs)

#------------------------------
# generate lists of metrics categories
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
    
    plot_list = list(size = size, 
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
  
  plot_list = list(depth = depth,
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
  
  plot_list = list(size = size, 
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

#-----------------------------------------------------------------
# fish-habitat plotting
#-----------------------------------------------------------------

# generic function for plotting
fh_plot = function(data, metrics_list) {
  data %>%
    select(qrtl, one_of(metrics_list)) %>%
    gather(variable, value, -qrtl) %>%
    ggplot(aes(x = value,
               color = qrtl,
               fill = qrtl)) +
    geom_density(alpha = 0.3) +
    theme_bw() +
    labs(x = "Value",
         y = "Density",
         color = "Category",
         fill = "Category") +
    facet_wrap( ~ variable,
                scales = "free")
}

#-----------------------------------------------------------------
# begin comparing means by dataset and habitat covariate
#-----------------------------------------------------------------

# list of habitat covariates
hab_list = unlist(unique(plot_list[1:length(plot_list)]))

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

# let's look at the # of significant comparisons by habitat covariate
alpha = 0.1
sig_tst_results = tst_results %>%
  mutate(sig = ifelse(p_value <= alpha, 1, 0)) %>%
  group_by(hab_cov) %>%
  summarise(sig_dfs = sum(sig, na.rm = T)) %>%
  ungroup() %>%
  filter(sig_dfs > 0) %>%
  #arrange(desc(sig_dfs)) %>%
  ggplot(aes(x = reorder(hab_cov, sig_dfs),
             y = sig_dfs)) +
  geom_col(fill = "steelblue4") +
  theme_bw() +
  coord_flip() +
  labs(x = "Habitat Covariate",
       y = "# of Significant Comparisons",
       title = paste(spc, ls))
sig_tst_results
ggsave(paste0("figures/", spc, "_", ls, "_significant_comps.png"))

# 'raw' p-values by habitat covariate
# raw_wcx_p = tst_results %>%
#   ggplot(aes(x = p_value)) +
#   geom_histogram(bins = 10) +
#   theme_bw() +
#   facet_wrap(~ hab_cov)
# raw_wcx_p

# plot meadian and mean p-values
p_val_p = tst_results %>%
  group_by(hab_cov) %>%
  summarise(mn_p = mean(p_value, na.rm = T),
            md_p = median(p_value, na.rm = T)) %>%
  ungroup() %>%
  gather(key, value, mn_p:md_p) %>%
  ggplot(aes(x = reorder(hab_cov, -value),
             y = value)) +
  geom_col(fill = "steelblue4") +
  geom_hline(yintercept = alpha, color = "red") +
  theme_bw() +
  coord_flip() +
  labs(x = "Habitat Covariate",
       y = "p",
       title = paste(spc, ls)) +
  facet_wrap(~ key)
p_val_p
ggsave(paste0("figures/", spc, "_", ls, "_p_values.png"))
