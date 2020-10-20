## InspectRawGageData.R
# This is a script to look at the raw data for a gage for troubleshooting purposes.

# gage to inspect
ID <- "01109070"

# mean flow metrics from john
gage_sample_new <-
  file.path(dir_data, "USGS_non_perenial_flow_climate_and_watershed_properties_060520.csv") %>% 
  readr::read_csv() %>% 
  dplyr::rename(gage_ID = staid, CLASS = class, region = aggregated_region) %>% 
  subset(gage_ID == as.numeric(ID))

gage_sample_old <-
  file.path(dir_data, "mean_annual_no_flow_and_climate_metrics_110419.csv") %>% 
  readr::read_csv()  %>% 
  subset(gage_ID %in% gage_sample_new$gage_ID) %>% 
  dplyr::select(all_of(c("gage_ID", "dec_lat_va", "dec_long_va", 
                         "FRESHW_WITHDRAWAL", 
                         "PCT_IRRIG_AG", "POWER_NUM_PTS", "DEVNLCD06", 
                         "CLAYAVE", "SILTAVE", "SANDAVE"))) %>% 
  subset(gage_ID == as.numeric(ID))
names(gage_sample_old)[4:10] <- stringr::str_to_lower(names(gage_sample_old)[4:10])

# event peak2zero metrics
p2z_all <- 
  file.path(dir_data, 
            "p2z_by_event_final_060320.csv") %>% 
  readr::read_csv() %>% 
  dplyr::rename(gage_ID = gage) %>% 
  subset(gage_ID == as.numeric(ID))

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
  subset(gage_ID == as.numeric(ID)) %>% 
  dplyr::left_join(p2z_mean_climyear, by = c("gage_ID", "currentclimyear" = "dry_climyear"))

gages_annual_ID <- 
  gages_annual_summary %>% 
  dplyr::select(gage_ID, currentclimyear, annualfractionnoflow, peak2z_length, zeroflowfirst)

# download raw USGS data
daily_data <- 
  dataRetrieval::readNWISdv(ID, parameterCd = "00060", 
                            startDate = "1980-04-01", endDate = "2018-03-31")

