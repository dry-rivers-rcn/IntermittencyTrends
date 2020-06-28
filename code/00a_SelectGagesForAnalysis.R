## 00a_SelectGagesForAnalysis.R
# This script will select and aggregate data for analysis.

source(file.path("code", "paths+packages.R"))

## load data from John - mean annual data
# load gage summary of gages for analysis
gage_sample_new <-
  file.path(dir_data, "USGS_non_perenial_flow_climate_and_watershed_properties_060520.csv") %>% 
  readr::read_csv() %>% 
  dplyr::rename(gage_ID = staid, CLASS = class, region = aggregated_region) %>% 
  # eliminate a bunch of mean flow and climate values because we will calculate those based on annual data
  dplyr::select(-annualfractionnoflow, -p2z, -zeroflowfirst, -pet_mm_climy, -t_max_c_climy,
                -t_min_c_climy, -t_mean_c_climy, -swe_mm_climy, -pdsi_climy,
                -swe.p, -p.pet, -tmean, -p_mm_climy)  

# get some additional watershed characteristics that don't change
gage_sample_old <-
  file.path(dir_data, "mean_annual_no_flow_and_climate_metrics_110419.csv") %>% 
  readr::read_csv()  %>% 
  subset(gage_ID %in% gage_sample_new$gage_ID) %>% 
  dplyr::select(all_of(c("gage_ID", "dec_lat_va", "dec_long_va", 
                         "FRESHW_WITHDRAWAL", 
                         "PCT_IRRIG_AG", "POWER_NUM_PTS", "DEVNLCD06", 
                         "CLAYAVE", "SILTAVE", "SANDAVE")))
names(gage_sample_old)[4:10] <- stringr::str_to_lower(names(gage_sample_old)[4:10])

# combine characteristics into a single data frame
gage_sample_prelim <- dplyr::left_join(gage_sample_new, gage_sample_old, by = "gage_ID")

sum(gage_sample_prelim$CLASS == "Ref")
sum(gage_sample_prelim$CLASS == "Non-ref")

# one gage is missing lat/long; fill it in
gage_sample_prelim$dec_lat_va[gage_sample_prelim$gage_ID==208111310] <- 36.04778
gage_sample_prelim$dec_long_va[gage_sample_prelim$gage_ID==208111310] <- -76.98417

## load annual stats for each gage
# load and calculate peak2zero length from raw data
p2z_all <- 
  file.path(dir_data, 
            "p2z_by_event_final_060320.csv") %>% 
  readr::read_csv() %>% 
  dplyr::rename(gage_ID = gage)

p2z_mean_climyear <-
  p2z_all %>% 
  dplyr::group_by(gage_ID, dry_climyear) %>% 
  dplyr::summarize(peak2z_length = mean(peak2zero),
                   peak2z_count = n())

# annual stats for each gage - climate and flow metrics
gages_annual_summary <- 
  file.path(dir_data, 
            "annual_no_flow_and_climate_metrics_climatic_year_050820.csv") %>% 
  readr::read_csv() %>% 
  dplyr::rename(gage_ID = sitewith0) %>% 
  dplyr::left_join(p2z_mean_climyear, by = c("gage_ID", "currentclimyear" = "dry_climyear")) %>% 
  subset(gage_ID %in% gage_sample_prelim$gage_ID)

# variables to calculate trends in and save
annual_vars <- 
  c("gage_ID", "currentclimyear",
    "annualfractionnoflow", "zeroflowfirst", "peak2z_length", "p_mm_wy", "p_mm_amj", 
    "p_mm_jas", "p_mm_ond", "p_mm_jfm", "pet_mm_wy", "pet_mm_amj", "pet_mm_jas", 
    "pet_mm_ond", "pet_mm_jfm", "T_max_c_wy", "T_max_c_amj", "T_max_c_jas", 
    "T_max_c_ond", "T_max_c_jfm", "T_min_c_wy", "T_min_c_amj", "T_min_c_jas", 
    "T_min_c_ond", "T_min_c_jfm", "pcumdist10days", "pcumdist50days", 
    "pcumdist90days", "swe_mm_wy", "swe_mm_amj", "swe_mm_jas", "swe_mm_ond", 
    "swe_mm_jfm", "srad_wm2_wy", "srad_wm2_amj", "srad_wm2_jas", 
    "srad_wm2_ond", "srad_wm2_jfm", "pdsi_wy", "pdsi_amj", "pdsi_jas", 
    "pdsi_ond", "pdsi_jfm")

gages_annual_summary <- 
  gages_annual_summary %>% 
  dplyr::select(all_of(annual_vars)) %>% 
  dplyr::rename(p_mm_cy = p_mm_wy,
                pet_mm_cy = pet_mm_wy,
                T_max_c_cy = T_max_c_wy,
                T_min_c_cy = T_min_c_wy,
                swe_mm_cy = swe_mm_wy,
                srad_wm2_cy = srad_wm2_wy,
                pdsi_cy = pdsi_wy)

## calculate mean for each gage
gages_mean <-
  gages_annual_summary %>% 
  dplyr::group_by(gage_ID) %>% 
  dplyr::summarize_all(mean, na.rm = T) %>% 
  dplyr::select(-currentclimyear, -pdsi_cy, -pdsi_jfm, -pdsi_amj, -pdsi_jas, -pdsi_ond)

gages_n_noflow <-
  gages_annual_summary %>% 
  dplyr::group_by(gage_ID) %>% 
  dplyr::summarize(yrs_noflow = sum(annualfractionnoflow > 0, na.rm = T),
                   yrs_data = sum(is.finite(annualfractionnoflow))) %>% 
  dplyr::ungroup()

## trim to only gages with at least 5 years of no-flow values
noflow_prc_threshold <- 0.0
gage_sample <-
  dplyr::left_join(gage_sample_prelim, gages_mean, by = "gage_ID") %>% 
  dplyr::left_join(gages_n_noflow, by = "gage_ID") %>% 
  subset(yrs_noflow/yrs_data > noflow_prc_threshold)

gages_annual_summary <-
  subset(gages_annual_summary, gage_ID %in% gage_sample$gage_ID)

## calculate trends - this is modified from John's script, CalculateTrends_020720.R
fulllengthwyears <- tibble::tibble(currentclimyear = c(min(gages_annual_summary$currentclimyear):max(gages_annual_summary$currentclimyear)))
sites <- unique(gages_annual_summary$gage_ID)

for (i in seq_along(sites)){
  current <- subset(gages_annual_summary, gage_ID == sites[i])
  current[current==-Inf] <- NA
  current[current==Inf] <- NA
  
  # which columns to calculate trends?
  #cols_trend <- which(!names(gages_annual_summary) %in% c("gage_ID", "currentclimyear"))
  cols_trend <- which(names(gages_annual_summary) %in% c("annualfractionnoflow", "zeroflowfirst", "peak2z_length"))
  
  results <- tibble::tibble(metric = colnames(gages_annual_summary[cols_trend]),
                            tau = as.numeric(rep(NA, length = length(cols_trend))), 
                            pval = as.numeric(rep(NA, length = length(cols_trend))), 
                            slope = as.numeric(rep(NA, length = length(cols_trend))),
                            n_years = as.numeric(rep(NA, length = length(cols_trend))))
  
  site_start <- T
  for(col in cols_trend){
    currentcolumnname <- colnames(current)[col]
    currentcolumn <- tibble::tibble(variable = dplyr::pull(current, col))
    currentcolumn$currentclimyear <- current$currentclimyear
    currentcolumn <- dplyr::right_join(currentcolumn, fulllengthwyears, by = "currentclimyear")
    years_data <-  sum(is.finite(currentcolumn$variable))
    
    # only calculate trend if at least 30 years of data
    if (years_data >= 30){
      manken <- rkt::rkt(currentcolumn$currentclimyear, currentcolumn$variable)

      trend <- tibble::tibble(metric = currentcolumnname,
                              tau = manken$tau,
                              pval = manken$sl[1],
                              slope = manken$B,
                              n_years = years_data)
    } else {
      trend <- tibble::tibble(metric = currentcolumnname,
                              tau = NA,
                              pval = NA,
                              slope = NA,
                              n_years = years_data)
    }
    if (site_start){
      results <- trend
      site_start <- F
    } else {
      results <- dplyr::bind_rows(results, trend)
    }
  }
  
  results$gage_ID <- sites[i]
  
  if (i == 1){
    gage_trends <- results
  } else {
    gage_trends <- dplyr::bind_rows(gage_trends, results)
  }
  
  # status update
  print(paste0("Site ", i, " complete"))
}

# subset to final sample: sites that have data for all records
gage_sample_out <- 
  gage_sample %>% 
  dplyr::select(-epa_level_1_ecoregion_name) %>% 
  subset(is.finite(annualfractionnoflow) & is.finite(zeroflowfirst) & is.finite(peak2z_length))  # screen out any with missing data

table(gage_sample_out$region, gage_sample_out$CLASS)
table(gage_sample_out$CLASS)

ggplot(gage_sample_out, aes(x=dec_long_va, y = dec_lat_va, color = region)) + geom_point()

## divide gage sample into train (80%), test (20%)
# choose fraction of gages to use as validation
n_folds <- 5
frac_test <- 1/n_folds

# set up k-fold cross-validation - want to use same sample for 
# all regions and metrics so need to take folds from each region
# and mix of ref/nonref
set.seed(1)
test1 <- 
  gage_sample %>% 
  dplyr::group_by(region, CLASS) %>% 
  dplyr::sample_frac(frac_test) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(gage_ID) %>% 
  dplyr::mutate(Sample = "Test1")
test2 <- 
  gage_sample %>% 
  subset(!(gage_ID %in% test1$gage_ID)) %>% 
  dplyr::group_by(region, CLASS) %>% 
  dplyr::sample_frac(1/(n_folds-1)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(gage_ID) %>% 
  dplyr::mutate(Sample = "Test2")
test3 <- 
  gage_sample %>% 
  subset(!(gage_ID %in% test1$gage_ID) &
           !(gage_ID %in% test2$gage_ID)) %>% 
  dplyr::group_by(region, CLASS) %>% 
  dplyr::sample_frac(1/(n_folds-2)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(gage_ID) %>% 
  dplyr::mutate(Sample = "Test3")
test4 <- 
  gage_sample %>% 
  subset(!(gage_ID %in% test1$gage_ID) &
           !(gage_ID %in% test2$gage_ID) &
           !(gage_ID %in% test3$gage_ID)) %>% 
  dplyr::group_by(region, CLASS) %>% 
  dplyr::sample_frac(1/(n_folds-3)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(gage_ID) %>% 
  dplyr::mutate(Sample = "Test4")
test5 <- 
  gage_sample %>% 
  subset(!(gage_ID %in% test1$gage_ID) &
           !(gage_ID %in% test2$gage_ID) &
           !(gage_ID %in% test3$gage_ID) &
           !(gage_ID %in% test4$gage_ID)) %>% 
  dplyr::select(gage_ID) %>% 
  dplyr::mutate(Sample = "Test5")

gage_val_sample <-
  dplyr::bind_rows(test1, test2, test3, test4, test5)

# add to gage_sample; everything that is not selected will be Train (if ref) or non-ref
gage_sample_out <- dplyr::left_join(gage_sample_out, gage_val_sample, by = "gage_ID")

# check count in each sample
gage_sample_out %>% 
  dplyr::select(region, Sample) %>% 
  table()

gage_sample_out %>% 
  dplyr::select(CLASS, Sample) %>% 
  table()

gage_sample_out %>% 
  dplyr::select(region, CLASS, Sample) %>% 
  table()

## there are a few instances that have swe > p; this should be impossible
i_swe_cy <- which(gages_annual_summary$swe_mm_cy > gages_annual_summary$p_mm_cy)
i_swe_amj <- which(gages_annual_summary$swe_mm_amj > gages_annual_summary$p_mm_amj)
i_swe_jas <- which(gages_annual_summary$swe_mm_jas > gages_annual_summary$p_mm_jas)
i_swe_ond <- which(gages_annual_summary$swe_mm_ond > gages_annual_summary$p_mm_ond)
i_swe_jfm <- which(gages_annual_summary$swe_mm_jfm > gages_annual_summary$p_mm_jfm)

gages_annual_summary$swe_mm_cy[i_swe_cy] <- gages_annual_summary$p_mm_cy[i_swe_cy]
gages_annual_summary$swe_mm_amj[i_swe_amj] <- gages_annual_summary$p_mm_amj[i_swe_amj]
gages_annual_summary$swe_mm_jas[i_swe_jas] <- gages_annual_summary$p_mm_jas[i_swe_jas]
gages_annual_summary$swe_mm_ond[i_swe_ond] <- gages_annual_summary$p_mm_ond[i_swe_ond]
gages_annual_summary$swe_mm_jfm[i_swe_jfm] <- gages_annual_summary$p_mm_jfm[i_swe_jfm]

## save data to repository
gage_sample_out %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_sample %>% 
  dplyr::select(gage_ID, epa_level_1_ecoregion_name, region) %>% 
  dplyr::rename(EPA_Ecoregion_Name = epa_level_1_ecoregion_name) %>% 
  subset(gage_ID %in% gage_sample_out$gage_ID) %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gages_annual_summary %>% 
  subset(gage_ID %in% gage_sample_out$gage_ID) %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual_NoHumanImpacts.csv"))

gage_trends %>% 
  subset(gage_ID %in% gage_sample_out$gage_ID) %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv"))

## plot
# load state map
states <- map_data("state")

ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = col.gray) +
  geom_point(data = gage_sample, aes(x = dec_long_va, y = dec_lat_va, color = region, shape = CLASS)) +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  scale_color_discrete(name = "Region") +
  scale_shape_discrete(name = "Class") +
  labs(title = "Gages Selected for Analysis", subtitle = "Tentative grouping by regions from Econame") +
  coord_map() +
  ggsave(file.path("results", "00_SelectGagesForAnalysis_Map.png"),
         width = 190, height = 90, units= "mm")