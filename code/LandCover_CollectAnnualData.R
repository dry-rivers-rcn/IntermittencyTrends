## LandCover_InterpolateToAnnual.R

source(file.path("code", "paths+packages.R"))

#### 
#### land use
####
# see script: LandCover_MessingAround.R for some comparisons between datasets, grouping, etc.
yr_start <- 1980
yr_stop <- 2017

## load LULC data
lulc_historic <-   
  readr::read_csv(file = "C:/Users/Zippe/OneDrive - The University of Kansas/Research/DryRiversRCN/data/LULC_historic.csv") %>% 
  dplyr::select(-rownum) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID)) %>% 
  subset(year >= yr_start & year <= yr_stop) %>% 
  dplyr::mutate(Source = "Historic")

lulc_hindcast <-   
  readr::read_csv(file = "C:/Users/Zippe/OneDrive - The University of Kansas/Research/DryRiversRCN/data/LULC_hindcast.csv") %>% 
  dplyr::select(-rownum) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID)) %>% 
  subset(year >= yr_start & year <= yr_stop) %>% 
  dplyr::mutate(Source = "Hindcast")

lulc_nlcd <-   
  readr::read_csv(file = "C:/Users/Zippe/OneDrive - The University of Kansas/Research/DryRiversRCN/data/LULC_nlcd.csv") %>% 
  dplyr::select(-rownum) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID)) %>% 
  subset(year >= yr_start & year <= yr_stop) %>% 
  dplyr::mutate(Source = "NLCD")

lulc_past <- 
  dplyr::bind_rows(lulc_hindcast, lulc_historic) %>% 
  tidyr::replace_na(list(lu3 = 0, lu4 = 0, lu5 = 0))

## lump to get percent cover in each class (see lulc_key.xlsx)
# historic and hindcast - sum by area then calculate percentage
lulc_past$water_area <- lulc_past$lu1 + lulc_past$lu17
lulc_past$developed_area <- lulc_past$lu2
lulc_past$forest_area <- lulc_past$lu3 + lulc_past$lu4 + lulc_past$lu5 + 
  lulc_past$lu8 + lulc_past$lu9 + lulc_past$lu10
lulc_past$barren_area <- lulc_past$lu6 + lulc_past$lu7
lulc_past$grass_area <- lulc_past$lu11 + lulc_past$lu12 + lulc_past$lu14
lulc_past$agriculture_area <- lulc_past$lu13
lulc_past$wetland_area <- lulc_past$lu15 + lulc_past$lu16
lulc_past$total_area <- 
  lulc_past$water_area + lulc_past$developed_area + 
  lulc_past$forest_area + lulc_past$barren_area + lulc_past$grass_area + 
  lulc_past$agriculture_area + lulc_past$wetland_area

lulc_past$lulc_water_prc <- lulc_past$water_area/lulc_past$total_area
lulc_past$lulc_dev_prc <- lulc_past$developed_area/lulc_past$total_area
lulc_past$lulc_forest_prc <- lulc_past$forest_area/lulc_past$total_area
lulc_past$lulc_barren_prc <- lulc_past$barren_area/lulc_past$total_area
lulc_past$lulc_grass_prc <- lulc_past$grass_area/lulc_past$total_area
lulc_past$lulc_ag_prc <- lulc_past$agriculture_area/lulc_past$total_area
lulc_past$lulc_wetland_prc <- lulc_past$wetland_area/lulc_past$total_area

# NLCD - sum by area then calculate percentage
lulc_nlcd$water_area <- lulc_nlcd$lu11 + lulc_nlcd$lu12
lulc_nlcd$developed_area <- lulc_nlcd$lu21 + lulc_nlcd$lu22 + lulc_nlcd$lu23 + lulc_nlcd$lu24
lulc_nlcd$forest_area <- lulc_nlcd$lu41 + lulc_nlcd$lu42 + lulc_nlcd$lu43
lulc_nlcd$barren_area <- lulc_nlcd$lu31
lulc_nlcd$grass_area <- lulc_nlcd$lu52 + lulc_nlcd$lu71 + lulc_nlcd$lu81
lulc_nlcd$agriculture_area <- lulc_nlcd$lu82
lulc_nlcd$wetland_area <- lulc_nlcd$lu90 + lulc_nlcd$lu95
lulc_nlcd$total_area <- 
  lulc_nlcd$water_area + lulc_nlcd$developed_area + 
  lulc_nlcd$forest_area + lulc_nlcd$barren_area + lulc_nlcd$grass_area + 
  lulc_nlcd$agriculture_area + lulc_nlcd$wetland_area

lulc_nlcd$lulc_water_prc <- lulc_nlcd$water_area/lulc_nlcd$total_area
lulc_nlcd$lulc_dev_prc <- lulc_nlcd$developed_area/lulc_nlcd$total_area
lulc_nlcd$lulc_forest_prc <- lulc_nlcd$forest_area/lulc_nlcd$total_area
lulc_nlcd$lulc_barren_prc <- lulc_nlcd$barren_area/lulc_nlcd$total_area
lulc_nlcd$lulc_grass_prc <- lulc_nlcd$grass_area/lulc_nlcd$total_area
lulc_nlcd$lulc_ag_prc <- lulc_nlcd$agriculture_area/lulc_nlcd$total_area
lulc_nlcd$lulc_wetland_prc <- lulc_nlcd$wetland_area/lulc_nlcd$total_area

# combine; since Hindcast and Historic are the same, drop one of them (hindcast)
lulc_all <- 
  dplyr::bind_rows(dplyr::select(lulc_past, gage_ID, year, ends_with("prc"), Source), 
                   dplyr::select(lulc_nlcd, gage_ID, year, ends_with("prc"), Source)) %>% 
  subset(!(year == 1992 & Source == "Hindcast")) %>% 
  dplyr::arrange(gage_ID, year) %>% 
  dplyr::rename(lulc_source = Source)

# need to fill in missing years (2006, 2007, 2009, 2010, 2012, 2014, 2015, 2017, 2018)
gages_all <- unique(lulc_nlcd$gage_ID)
df_wuse_yearmatch <- tibble::tibble(currentclimyear = seq(1980, 2018))
for (g in gages_all){
  df_lulc_g <- 
    subset(lulc_all, gage_ID == g) %>% 
    dplyr::right_join(df_wuse_yearmatch, by = c("year" = "currentclimyear")) %>% 
    tidyr::replace_na(list("gage_ID" = g)) %>% 
    dplyr::arrange(year)
  
  df_lulc_g$lulc_water_prc <- zoo::na.approx(df_lulc_g$lulc_water_prc, na.rm = F)
  df_lulc_g$lulc_water_prc[df_lulc_g$year > 2016] <- 
    df_lulc_g$lulc_water_prc[df_lulc_g$year == 2016]
  
  df_lulc_g$lulc_dev_prc <- zoo::na.approx(df_lulc_g$lulc_dev_prc, na.rm = F)
  df_lulc_g$lulc_dev_prc[df_lulc_g$year > 2016] <- 
    df_lulc_g$lulc_dev_prc[df_lulc_g$year == 2016]
  
  df_lulc_g$lulc_forest_prc <- zoo::na.approx(df_lulc_g$lulc_forest_prc, na.rm = F)
  df_lulc_g$lulc_forest_prc[df_lulc_g$year > 2016] <- 
    df_lulc_g$lulc_forest_prc[df_lulc_g$year == 2016]
  
  df_lulc_g$lulc_barren_prc <- zoo::na.approx(df_lulc_g$lulc_barren_prc, na.rm = F)
  df_lulc_g$lulc_barren_prc[df_lulc_g$year > 2016] <- 
    df_lulc_g$lulc_barren_prc[df_lulc_g$year == 2016]
  
  df_lulc_g$lulc_grass_prc <- zoo::na.approx(df_lulc_g$lulc_grass_prc, na.rm = F)
  df_lulc_g$lulc_grass_prc[df_lulc_g$year > 2016] <- 
    df_lulc_g$lulc_grass_prc[df_lulc_g$year == 2016]
  
  df_lulc_g$lulc_ag_prc <- zoo::na.approx(df_lulc_g$lulc_ag_prc, na.rm = F)
  df_lulc_g$lulc_ag_prc[df_lulc_g$year > 2016] <- 
    df_lulc_g$lulc_ag_prc[df_lulc_g$year == 2016]
  
  df_lulc_g$lulc_wetland_prc <- zoo::na.approx(df_lulc_g$lulc_wetland_prc, na.rm = F)
  df_lulc_g$lulc_wetland_prc[df_lulc_g$year > 2016] <- 
    df_lulc_g$lulc_wetland_prc[df_lulc_g$year == 2016]
  
  df_lulc_g$lulc_source[is.na(df_lulc_g$lulc_source)] <- "Gap-Filled"
  
  if (g == gages_all[1]){
    lulc_out <- df_lulc_g
  } else {
    lulc_out <- dplyr::bind_rows(lulc_out, df_lulc_g)
  }
  
  print(paste0(which(gages_all==g), " complete"))
  
}
