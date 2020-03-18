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
spc = "chnk"         # species: either "chnk" or "sthd"
ls  = "win"          # life_stage: "sum" summer parr; "win" winter presmolt; "spw" spawning (redds)

#------------------------------
# load data
loadRData = function(fileName) {
  # loads and RData file, and returns it
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


