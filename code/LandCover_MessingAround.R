## LandCover_MessingAround.R
# Messing around with land cover data received from Nate

source(file.path("code", "paths+packages.R"))

## load data - gage mean properties and annual values
gage_sample <- 
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv")) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID))

gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv"))

yr_start <- min(gage_sample_annual$currentclimyear)
yr_stop <- max(gage_sample_annual$currentclimyear)

## load LULC data
lulc_historic <-   
  readr::read_csv(file = file.path(dir_data, "LULC_historic.csv")) %>% 
  dplyr::select(-rownum) %>% 
  subset(year >= yr_start & year <= yr_stop & 
           gage_ID %in% gage_sample$gage_ID) %>% 
  dplyr::mutate(Source = "Historic")

lulc_hindcast <-   
  readr::read_csv(file = file.path(dir_data, "LULC_hindcast.csv")) %>% 
  dplyr::select(-rownum) %>% 
  subset(year >= yr_start & year <= yr_stop & 
           gage_ID %in% gage_sample$gage_ID) %>% 
  dplyr::mutate(Source = "Hindcast")


lulc_nlcd <-   
  readr::read_csv(file = file.path(dir_data, "LULC_nlcd.csv")) %>% 
  dplyr::select(-rownum) %>% 
  subset(year >= yr_start & year <= yr_stop & 
           gage_ID %in% gage_sample$gage_ID) %>% 
  dplyr::mutate(Source = "NLCD")

lulc_all <- 
  dplyr::bind_rows(lulc_hindcast, lulc_historic) %>% 
  tidyr::replace_na(list(lu3 = 0, lu4 = 0, lu5 = 0))

# spit out missing NLCD gages
gage_sample$NLCD <- gage_sample$gage_ID %in% as.numeric(lulc_nlcd$gage_ID)
gage_sample %>% 
  dplyr::select(gage_ID, NLCD) %>% 
  readr::write_csv(path = file.path("results", "LandCover_NLCDstatus.csv"))

max(lulc_hindcast$year)
min(lulc_historic$year)

# 1992 is included in both datasets - see how it matches
lulc1992 <- 
  lulc_all %>% 
  subset(year == 1992)
lulc1992$gage_ID[which(lulc1992$lu3 > 0)]

inspect1992 <- 
  lulc1992 %>% 
  subset(gage_ID == "12395000") %>% 
  dplyr::select(-gage_ID,
                 -year) %>% 
  pivot_longer(cols = starts_with("lu"),
               values_to = "area") %>% 
  pivot_wider(names_from = Source,
              values_from = area) %>% 
  dplyr::mutate(area_diff = Historic - Hindcast)

inspect1992
## some gage comparisons
# gage 01109403: 
#  - the historic data has values for lu5 ("mechanically disturbed private")
#  - for hindcast, these are split into lu8 (decid forest, ~2/3) and lu10 (mixed forest, ~1/3)
# gage 08285500, 09169500:
#  - the historic data has values for lu3, lu4, and lu5
#  - for hindcast, these are mostly in lu9 (evergreen forest) and lu10 (mixed forest)
# gage 09128500:
#  - historic has lu5 and lu3, for hindcast it is in lu9
# CONCLUSION: values in historic from lu3, lu4, and lu5 should be forest.
#   this is consitent with the dataset design, because the historic dataset is designed 
#   for forest assessment so they more finely differentiated the forest class

ggplot(inspect1992, aes(x = Historic, y = Hindcast, color = name)) +
  geom_point()

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
lulc_all$prc_water <- lulc_all$water_area/lulc_all$total_area
lulc_all$prc_dev <- lulc_all$developed_area/lulc_all$total_area
lulc_all$prc_forest <- lulc_all$forest_area/lulc_all$total_area
lulc_all$prc_barren <- lulc_all$barren_area/lulc_all$total_area
lulc_all$prc_grass <- lulc_all$grass_area/lulc_all$total_area
lulc_all$prc_ag <- lulc_all$agriculture_area/lulc_all$total_area
lulc_all$prc_wetland <- lulc_all$wetland_area/lulc_all$total_area

summary(lulc_all)

# check percentages to see if Hindcast and Historic are the same
inspect1992prc <- 
  lulc_all %>% 
  subset(year == 1992) %>% 
  dplyr::select(gage_ID, prc_water, prc_dev, prc_forest, prc_barren, 
                prc_grass, prc_ag, prc_wetland, Source) %>% 
  pivot_longer(cols = starts_with("prc_"),
               values_to = "prc") %>% 
  pivot_wider(id_cols = c(gage_ID, name),
              names_from = Source,
              values_from = prc) %>% 
  dplyr::mutate(prc_diff = Historic - Hindcast)

ggplot(inspect1992prc, aes(x = Historic, y = Hindcast, color = name)) +
  geom_point()

# since Hindcast and Historic are the same, drop one of them (hindcast)
lulc_out <- 
  lulc_all %>% 
  dplyr::select(gage_ID, year, Source, starts_with("prc_")) %>% 
  subset(!(year == 1992 & Source == "Hindcast")) %>% 
  dplyr::arrange(gage_ID, year)

ggplot(lulc_out, aes(x = year, y = prc_forest, group = gage_ID)) +
  geom_line(alpha = 0.25)
