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
  subset(gage_ID %in% gage_sample_prelim$gage_ID) %>% 
  dplyr::mutate(annualnoflowdays = as.integer(round(annualfractionnoflow*365)))

# variables to calculate trends in and save
annual_vars <- 
  c("gage_ID", "currentclimyear",
    "annualfractionnoflow", "annualnoflowdays", "zeroflowfirst", "peak2z_length", "p_mm_wy", "p_mm_amj", 
    "p_mm_jas", "p_mm_ond", "p_mm_jfm", "pet_mm_wy", "pet_mm_amj", "pet_mm_jas", 
    "pet_mm_ond", "pet_mm_jfm", "T_max_c_wy", "T_max_c_amj", "T_max_c_jas", 
    "T_max_c_ond", "T_max_c_jfm", "T_min_c_wy", "T_min_c_amj", "T_min_c_jas", 
    "T_min_c_ond", "T_min_c_jfm", "pcumdist10days", "pcumdist50days", 
    "pcumdist90days", "swe_mm_wy", "swe_mm_amj", "swe_mm_jas", "swe_mm_ond", 
    "swe_mm_jfm", "srad_wm2_wy", "srad_wm2_amj", "srad_wm2_jas", 
    "srad_wm2_ond", "srad_wm2_jfm")

gages_annual_summary <- 
  gages_annual_summary %>% 
  dplyr::select(all_of(annual_vars)) %>% 
  dplyr::rename(p_mm_cy = p_mm_wy,
                pet_mm_cy = pet_mm_wy,
                T_max_c_cy = T_max_c_wy,
                T_min_c_cy = T_min_c_wy,
                swe_mm_cy = swe_mm_wy,
                srad_wm2_cy = srad_wm2_wy)

## calculate mean for each gage
gages_mean <-
  gages_annual_summary %>% 
  dplyr::group_by(gage_ID) %>% 
  dplyr::summarize_all(mean, na.rm = T) %>% 
  dplyr::select(-currentclimyear)

gages_n_noflow <-
  gages_annual_summary %>% 
  dplyr::group_by(gage_ID) %>% 
  dplyr::summarize(yrs_noflow = sum(annualfractionnoflow > 0, na.rm = T),
                   yrs_data = sum(is.finite(annualfractionnoflow))) %>% 
  dplyr::ungroup()

## trim to only gages with at least 5 years of no-flow values - currently not used
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

# which columns to calculate trends?
cols_trend <- which(!names(gages_annual_summary) %in% c("gage_ID", "currentclimyear"))
nvar <- length(cols_trend)

# year to split for mann-whitney? (this will be included in the first set)
mw_yr_split <- 1998 # group 1 1980-1998, group 2 1999-2017

for (i in seq_along(sites)){
  current <- subset(gages_annual_summary, gage_ID == sites[i])
  current[current==-Inf] <- NA
  current[current==Inf] <- NA
  
  results <- tibble::tibble(metric = colnames(gages_annual_summary[cols_trend]),
                            mk_tau = as.numeric(rep(NA, length = nvar)), 
                            mk_p = as.numeric(rep(NA, length = nvar)), 
                            sen_slope = as.numeric(rep(NA, length = nvar)),
                            lin_slope = as.numeric(rep(NA, length = nvar)),
                            lin_r2 = as.numeric(rep(NA, length = nvar)),
                            lin_p = as.numeric(rep(NA, length = nvar)),
                            pois_slope = as.numeric(rep(NA, length = nvar)),
                            pois_r2 = as.numeric(rep(NA, length = nvar)),
                            pois_p = as.numeric(rep(NA, length = nvar)),
                            mw_p = as.numeric(rep(NA, length = nvar)),
                            mw_meanGroup1 = as.numeric(rep(NA, length = nvar)),
                            mw_meanGroup2 = as.numeric(rep(NA, length = nvar)),
                            mw_medianGroup1 = as.numeric(rep(NA, length = nvar)),
                            mw_medianGroup2 = as.numeric(rep(NA, length = nvar)),
                            n_yrGroup1 = as.numeric(rep(NA, length = nvar)),
                            n_yrGroup2 = as.numeric(rep(NA, length = nvar)))
  
  site_start <- T
  for(col in cols_trend){
    currentcolumnname <- colnames(current)[col]
    currentcolumn <- tibble::tibble(variable = dplyr::pull(current, col))
    currentcolumn$currentclimyear <- current$currentclimyear
    currentcolumn <- dplyr::right_join(currentcolumn, fulllengthwyears, by = "currentclimyear")
    years_data <-  sum(is.finite(currentcolumn$variable))
    
    # mann-whitney
    group1 <- currentcolumn$variable[currentcolumn$currentclimyear <= mw_yr_split]
    group2 <- currentcolumn$variable[currentcolumn$currentclimyear > mw_yr_split]
    if (sum(is.finite(group1)) > 5 & sum(is.finite(group2)) > 5){
      mw_test <- wilcox.test(group1, group2)
      mw_p <- mw_test$p.value
    } else {
      mw_p <- NA
    }
    
    # only calculate trends, differences if at least 10 years of data
    if (years_data >= 10){
      i_finite <- which(is.finite(currentcolumn$variable))
      manken <- rkt::rkt(currentcolumn$currentclimyear, currentcolumn$variable)
      linfit <- lm(variable ~ currentclimyear, data = currentcolumn)
      
      # poisson slope only possible for count data (integer)
      if (currentcolumnname %in% c("annualnoflowdays", "zeroflowfirst",
                                   "pcumdist10days", "pcumdist50days", "pcumdist90days")){
        pois <- glm(variable ~ currentclimyear, family=poisson(link = "log"), data = currentcolumn)
        p_slope <- coef(pois)[2]
        p_r2 <- R2(predict(pois, currentcolumn[i_finite, ]), currentcolumn$variable[i_finite])
        p_p <- summary(pois)$coef[2,4]
      } else {
        p_slope <- NA
        p_r2 <- NA
        p_p <- NA
      }
      
      trend <- tibble::tibble(metric = currentcolumnname,
                              mk_tau = manken$tau,
                              mk_p = manken$sl[1],
                              sen_slope = manken$B,
                              lin_slope = coef(linfit)[2],
                              lin_r2 = summary(linfit)$r.squared,
                              lin_p = lmp(linfit),
                              pois_slope = p_slope,
                              pois_r2 = p_r2,
                              pois_p = p_p,
                              mw_p = mw_p,
                              mw_meanGroup1 = mean(group1, na.rm = T),
                              mw_meanGroup2 = mean(group2, na.rm = T),
                              mw_medianGroup1 = median(group1, na.rm = T),
                              mw_medianGroup2 = median(group2, na.rm = T),
                              n_yrGroup1 = sum(is.finite(group1)),
                              n_yrGroup2 = sum(is.finite(group1)))
    } else {
      trend <- tibble::tibble(metric = currentcolumnname,
                              mk_tau = NA,
                              mk_p = NA,
                              sen_slope = NA,
                              lin_slope = NA,
                              lin_r2 = NA,
                              lin_p = NA,
                              pois_slope = NA,
                              pois_r2 = NA,
                              pois_p = NA,
                              mw_p = mw_test$p.value,
                              mw_meanGroup1 = mean(group1, na.rm = T),
                              mw_meanGroup2 = mean(group2, na.rm = T),
                              mw_medianGroup1 = median(group1, na.rm = T),
                              mw_medianGroup2 = median(group2, na.rm = T),
                              n_yrGroup1 = sum(is.finite(group1)),
                              n_yrGroup2 = sum(is.finite(group1)))
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
  subset(is.finite(annualnoflowdays) & is.finite(zeroflowfirst) & is.finite(peak2z_length))  # screen out any with missing data

table(gage_sample_out$region, gage_sample_out$CLASS)
table(gage_sample_out$CLASS)

ggplot(gage_sample_out, aes(x=dec_long_va, y = dec_lat_va, color = region)) + geom_point()

## divide gage sample into train (80%), test (20%)
# choose fraction of gages to use as validation
frac_test <- 0.2

# set up k-fold cross-validation - want to use same sample for 
# all regions and metrics so need to take folds from each region
# and mix of ref/nonref
set.seed(1)
test <- 
  gage_sample_out %>% 
  dplyr::group_by(region, CLASS) %>% 
  dplyr::sample_frac(frac_test) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(gage_ID) %>% 
  dplyr::mutate(Sample = "Test")
train <- 
  gage_sample_out %>% 
  subset(!(gage_ID %in% test$gage_ID)) %>% 
  dplyr::select(gage_ID) %>% 
  dplyr::mutate(Sample = "Train")

gage_val_sample <-
  dplyr::bind_rows(test, train)

# add to gage_sample
gage_sample_out <- dplyr::left_join(gage_sample_out, gage_val_sample, by = "gage_ID")

# check count in each sample
gage_sample_out %>% 
  dplyr::select(Sample) %>% 
  table()

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
