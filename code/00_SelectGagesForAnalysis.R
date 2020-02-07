## 00_SelectGagesForAnalysis.R
# This script will select gages for analysis based on the following characteristics:
#  -At least 1 no-flow day in at least 10 of the 30 years

source(file.path("code", "paths+packages.R"))

## load John's annual summary statistics
gages_annual_summary <- 
  file.path(dir_data, 
            "annual_no_flow_and_climate_metrics_020720_trends.csv") %>% 
  readr::read_csv()

# summarize mean no-flow days/yr
gages_summary <-
  gages_annual_summary %>% 
  dplyr::group_by(gage_ID) %>% 
  dplyr::summarize(noflowfraction_mean = mean(annualfractionnoflow),
                   n_years = n())

# subset to gages meeting threshold
noflowfraction_min_threshold <- 15/365   # Eng et al used 15 days
noflowfraction_max_threshold <- 350/365  # also at least 15 no-flow days/yr
year_threshold <- 30

# sites to sample
gage_sample <- gages_summary[gages_summary$noflowfraction_mean > noflowfraction_min_threshold &
                               gages_summary$noflowfraction_mean < noflowfraction_max_threshold &
                               gages_summary$n_years > year_threshold, ]

sum(gage_sample$CLASS == "Ref")
sum(gage_sample$CLASS == "Non-ref")

# subset annual stats for sampled gages
gage_sample_annual <- 
  gages_annual_summary %>% 
  subset(site %in% gage_sample$site)

vars_keep <- 
  c("site", "wyear", "p/pet", 
    "p_mm_wy", "p_mm_jja", "p_mm_son", "p_mm_djf", "p_mm_mam", "pet_mm_wy", 
    "pet_mm_jja", "pet_mm_son", "pet_mm_djf", "pet_mm_mam", "T_max_c_wy", 
    "T_max_c_jja", "T_max_c_son", "T_max_c_djf", "T_max_c_mam", "T_min_c_wy", 
    "T_min_c_jja", "T_min_c_son", "T_min_c_djf", "T_min_c_mam", "pcumdist10days", 
    "pcumdist50days", "pcumdist90days", "swe_mm_wy", "swe_mm_jja", 
    "swe_mm_son", "swe_mm_djf", "swe_mm_mam", "srad_wm2_wy", "srad_wm2_jja", 
    "srad_wm2_son", "srad_wm2_djf", "srad_wm2_mam", "pdsi_wy", "pdsi_jja", 
    "pdsi_son", "pdsi_djf", "pdsi_mam", 
    "annualfractionnoflow", 
    "zeroflowcentroiddate", 
    "totalnoflowperiods")

## save sample to repository
gage_sample %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSample.csv"))

gage_sample_annual[, vars_keep] %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv"))
