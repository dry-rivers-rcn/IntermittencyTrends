## Trends_SummaryStatistics.R
#' This script load the trend results and calculates some simple 
#' summary statistics for the trends.
#' 

source(file.path("code", "paths+packages.R"))

## metrics we care about
metrics <- c("annualfractionnoflow", "zeroflowfirst", "peak2z_length")

## load data
gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_trends <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv")) %>% 
  subset(metric %in% metrics) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID")

## summarize
p_thres <- 0.05
gage_trends %>% 
  dplyr::group_by(metric) %>% 
  dplyr::summarize(n_finite = sum(is.finite(slope)),
                   n_sig = sum(pval < p_thres, na.rm = T),
                   n_sig_pos = sum(slope > 0 & pval < p_thres, na.rm = T),
                   n_sig_neg = sum(slope <= 0 & pval < p_thres, na.rm = T))

207/512
34/150
23/123
