### 00a_CollectHumanImpactData.R
# This script will compile land use data and other human impacts
# and add it to the output from 00_SelectGagesForAnalysis.R

source(file.path("code", "paths+packages.R"))

## load output from 00_SelectGages...
gage_sample <- 
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv")) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID))

gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual_NoHumanImpacts.csv")) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID") %>% 
  # add some derived variables
  dplyr::mutate(p.pet_cy = p_mm_cy/pet_mm_cy,
                swe.p_cy = swe_mm_cy/p_mm_cy,
                p.pet_jfm = p_mm_jfm/pet_mm_jfm,
                swe.p_jfm = swe_mm_jfm/p_mm_jfm,
                p.pet_amj = p_mm_amj/pet_mm_amj,
                swe.p_amj = swe_mm_amj/p_mm_amj,
                p.pet_jas = p_mm_jas/pet_mm_jas,
                swe.p_jas = swe_mm_jas/p_mm_jas,
                p.pet_ond = p_mm_ond/pet_mm_ond,
                swe.p_ond = swe_mm_ond/p_mm_ond)

## Load shapefile that matches GAGES-II gage_IDs to NHDPlus COMIDs
sf_gageloc <- 
  file.path(dir_gis, "USGS_GageLoc", "NHD_index", "GageLoc.shp") %>% 
  sf::st_read() %>% 
  dplyr::mutate(gage_ID = as.numeric(SOURCE_FEA)) %>% 
  sf::st_drop_geometry() %>% 
  subset(gage_ID %in% gage_sample$gage_ID)

gage_sample <- 
  dplyr::left_join(gage_sample, sf_gageloc[,c("gage_ID", "FLComID")], by = "gage_ID")

# there are 5 gage_sample that don't get a COMID; manually assign those
gage_sample$FLComID[gage_sample$gage_ID == 2307359] <- 166743866
gage_sample$FLComID[gage_sample$gage_ID == 2264000] <- 21478144
gage_sample$FLComID[gage_sample$gage_ID == 2303350] <- 166743841
gage_sample$FLComID[gage_sample$gage_ID == 2309848] <- 16933100
gage_sample$FLComID[gage_sample$gage_ID == 2236900] <- 16636656

sum(is.na(gage_sample$FLComID))
####
#### load estimates of dam data from NID
####
# columns we want:  
#   - TOT_NORM_STORAGEYYYY = total normal dam storage (in acre-feet) in catchment
#   - TOT_NDAMSYYYY = total number of dams built on or before YYYY
#   - TOT_MAJORYYYY = total number of major dams
for (y in c(1980, 1990, 2000, 2010, 2013)){
  df_NID_y <- 
    file.path(dir_data, "NID_dams", paste0("NID_", y, "_CONUS.txt")) %>% 
    readr::read_csv() %>% 
    subset(COMID %in% gage_sample$FLComID) %>% 
    dplyr::select(COMID, starts_with("TOT_")) %>% 
    magrittr::set_colnames(c("COMID", "dams_n", "maxstorage_af", "normstorage_af", "majordams_n")) %>% 
    dplyr::mutate(Year = y)
    
    if (y == 1980){
      df_NID <- df_NID_y
    } else {
      df_NID <-
        dplyr::bind_rows(df_NID, df_NID_y)
    }
  
}

# data frame for year matching
df_nid_yearmatch <- 
  tibble::tibble(currentclimyear = seq(1980, 2018),
                 year_NID = c(signif(seq(1980, 2011), 3), rep(2013, 7)))

# join to annual gage data
gage_sample_annual_NID <- 
  dplyr::left_join(gage_sample_annual, df_nid_yearmatch, by = "currentclimyear") %>% 
  dplyr::left_join(gage_sample[,c("gage_ID", "FLComID")], by = "gage_ID") %>% 
  dplyr::left_join(df_NID, by = c("year_NID" = "Year", "FLComID" = "COMID")) %>% 
  dplyr::select(-FLComID, -year_NID)

####
#### USGS water use data
####
df_wuse <- 
  file.path(dir_gis, "USGS_GageLoc", "GAGES-II", "Dataset10_WaterUse", "WaterUse_1985-2010.txt") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(gage_ID = as.numeric(STAID)) %>% 
  subset(gage_ID %in% gage_sample$gage_ID) %>% 
  tidyr::pivot_longer(cols = starts_with("wu"),
                      names_to = "wuYYYY",
               values_to = "wu_MGPDKM2") %>% 
  dplyr::mutate(year = as.numeric(substr(wuYYYY, 3, 7))) %>% 
  dplyr::select(-STAID, -wuYYYY)


df_wuse_yearmatch <- tibble::tibble(currentclimyear = seq(1980, 2018))

for (g in gage_sample$gage_ID){
  i <- which(gage_sample$gage_ID == g)
  
  df_wuse_g <- 
    subset(df_wuse, gage_ID == g) %>% 
    dplyr::right_join(df_wuse_yearmatch, by = c("year" = "currentclimyear")) %>% 
    tidyr::replace_na(list("gage_ID" = g))
  
  df_wuse_g$wu_interp <- zoo::na.approx(df_wuse_g$wu_MGPDKM2, na.rm = F)
  df_wuse_g$wu_interp[df_wuse_g$year < 1985] <- df_wuse_g$wu_MGPDKM2[df_wuse_g$year == 1985]
  df_wuse_g$wu_interp[df_wuse_g$year > 2010] <- df_wuse_g$wu_MGPDKM2[df_wuse_g$year == 2010]
  
  # convert from (million gallons per km2) to (mm)
  df_wuse_g$wuse_mm <- 1000*(df_wuse_g$wu_interp*1000000*0.00378541)/(1000*1000)
  
  if (i == 1){
    df_wuse_all <- dplyr::select(df_wuse_g, gage_ID, year, wuse_mm)
  } else {
    df_wuse_all <- dplyr::bind_rows(df_wuse_all, dplyr::select(df_wuse_g, gage_ID, year, wuse_mm))
  }
  
  print(paste0(i, " complete"))
  
}

gage_sample_annual_NID_wuse <- 
  dplyr::left_join(gage_sample_annual_NID, df_wuse_all, by = c("gage_ID", "currentclimyear" = "year"))

####
#### USDA ag stats
####

# load raw data
df_ag <-
  file.path(dir_gis, "USGS_GageLoc", "GAGES-II", "Dataset2_CensusOfAgriculture", "CensusOfAgriculture_1950-2012.txt") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(gage_ID = as.numeric(STAID)) %>% 
  subset(gage_ID %in% gage_sample$gage_ID) %>% 
  dplyr::select(gage_ID, hcrop1982, hcrop1992, hcrop2002, hcrop2012,
                irrig1982, irrig1992, irrig2002, irrig2012) %>% 
  tidyr::pivot_longer(cols = -gage_ID) %>% 
  dplyr::mutate(metric = substr(name, 1, 5),
                year = as.numeric(substr(name, 6, 9))) %>% 
  dplyr::select(-name)

for (g in gage_sample$gage_ID){
  i <- which(gage_sample$gage_ID == g)
  
  df_irrig_g <- 
    subset(df_ag, metric == "irrig" & gage_ID == g) %>% 
    dplyr::right_join(df_wuse_yearmatch, by = c("year" = "currentclimyear")) %>% 
    tidyr::replace_na(list("gage_ID" = g))
  
  df_hcrop_g <- 
    subset(df_ag, metric == "hcrop" & gage_ID == g) %>% 
    dplyr::right_join(df_wuse_yearmatch, by = c("year" = "currentclimyear")) %>% 
    tidyr::replace_na(list("gage_ID" = g))

  df_irrig_g$irrig_prc <- zoo::na.approx(df_irrig_g$value, na.rm = F)
  df_irrig_g$irrig_prc[df_irrig_g$year < 1982] <- 
    df_irrig_g$irrig_prc[df_irrig_g$year == 1982]
  df_irrig_g$irrig_prc[df_irrig_g$year > 2012] <- 
    df_irrig_g$irrig_prc[df_irrig_g$year == 2012]
  
  df_hcrop_g$harvcrop_prc <- zoo::na.approx(df_hcrop_g$value, na.rm = F)
  df_hcrop_g$harvcrop_prc[df_hcrop_g$year < 1982] <- 
    df_hcrop_g$harvcrop_prc[df_hcrop_g$year == 1982]
  df_hcrop_g$harvcrop_prc[df_hcrop_g$year > 2012] <- 
    df_hcrop_g$harvcrop_prc[df_hcrop_g$year == 2012]
  
  # combine
  df_ag_g <- dplyr::left_join(df_irrig_g[,c("gage_ID", "year", "irrig_prc")],
                              df_hcrop_g[,c("year", "harvcrop_prc")], by = "year")
  
  if (i == 1){
    df_ag_all <- df_ag_g
  } else {
    df_ag_all <- dplyr::bind_rows(df_ag_all, df_ag_g)
  }
  
  print(paste0(i, " complete"))
  
}

gage_sample_annual_NID_wuse_ag <- 
  dplyr::left_join(gage_sample_annual_NID_wuse, df_ag_all, by = c("gage_ID", "currentclimyear" = "year"))

#### 
#### land use
####
# see script: LandCover_MessingAround.R for some comparisons between datasets, grouping, etc.
yr_start <- min(gage_sample_annual$currentclimyear)
yr_stop <- max(gage_sample_annual$currentclimyear)

## load LULC data
lulc_historic <-   
  readr::read_csv(file = file.path(dir_data, "LULC_historic.csv")) %>% 
  dplyr::select(-rownum) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID)) %>% 
  subset(year >= yr_start & year <= yr_stop & 
           gage_ID %in% gage_sample$gage_ID) %>% 
  dplyr::mutate(Source = "Historic")

lulc_hindcast <-   
  readr::read_csv(file = file.path(dir_data, "LULC_hindcast.csv")) %>% 
  dplyr::select(-rownum) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID)) %>% 
  subset(year >= yr_start & year <= yr_stop & 
           gage_ID %in% gage_sample$gage_ID) %>% 
  dplyr::mutate(Source = "Hindcast")

lulc_all <- 
  dplyr::bind_rows(lulc_hindcast, lulc_historic) %>% 
  tidyr::replace_na(list(lu3 = 0, lu4 = 0, lu5 = 0))

## lump to get percent cover in each class (see lulc_key.xlsx)
# lu categories and broad class are: 
# 1	water
# 2	developed
# 3 mechanically disturbed national forests
# 4 mechanically disturbed other public lands
# 5 mechanically disturbed private
# 6	mining
# 7	barren
# 8	deciduous forest
# 9	evergreen forest
# 10	mixed forest
# 11	grassland
# 12	shrubland
# 13	cropland
# 14	hay/pasture land
# 15	herbaceous wetland
# 16	woody wetland
# 17	perennial ice/snow

# sum by area
lulc_all$water_area <- lulc_all$lu1 + lulc_all$lu17
lulc_all$developed_area <- lulc_all$lu2
lulc_all$forest_area <- lulc_all$lu3 + lulc_all$lu4 + lulc_all$lu5 + 
  lulc_all$lu8 + lulc_all$lu9 + lulc_all$lu10
lulc_all$barren_area <- lulc_all$lu6 + lulc_all$lu7
lulc_all$grass_area <- lulc_all$lu11 + lulc_all$lu12 + lulc_all$lu14
lulc_all$agriculture_area <- lulc_all$lu13
lulc_all$wetland_area <- lulc_all$lu15 + lulc_all$lu16
lulc_all$total_area <- 
  lulc_all$water_area + lulc_all$developed_area + 
  lulc_all$forest_area + lulc_all$barren_area + lulc_all$grass_area + 
  lulc_all$agriculture_area + lulc_all$wetland_area

# calculate percentage
lulc_all$lulc_water_prc <- lulc_all$water_area/lulc_all$total_area
lulc_all$lulc_dev_prc <- lulc_all$developed_area/lulc_all$total_area
lulc_all$lulc_forest_prc <- lulc_all$forest_area/lulc_all$total_area
lulc_all$lulc_barren_prc <- lulc_all$barren_area/lulc_all$total_area
lulc_all$lulc_grass_prc <- lulc_all$grass_area/lulc_all$total_area
lulc_all$lulc_ag_prc <- lulc_all$agriculture_area/lulc_all$total_area
lulc_all$lulc_wetland_prc <- lulc_all$wetland_area/lulc_all$total_area

# since Hindcast and Historic are the same, drop one of them (hindcast)
lulc_out <- 
  lulc_all %>% 
  subset(!(year == 1992 & Source == "Hindcast")) %>% 
  dplyr::select(gage_ID, year, starts_with("lulc_")) %>% 
  dplyr::arrange(gage_ID, year)

gage_sample_annual_NID_wuse_ag_lulc <-
  dplyr::left_join(gage_sample_annual_NID_wuse_ag, lulc_out, by = c("currentclimyear" = "year", "gage_ID"))

# compare this lulc and interpolated ag census harvest cropland
ggplot(gage_sample_annual_NID_wuse_ag_lulc, aes(x=harvcrop_prc, y=lulc_ag_prc)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

####
#### save results
####
gage_sample_annual_NID_wuse_ag_lulc %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv"))
