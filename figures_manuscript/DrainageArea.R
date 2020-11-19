## Figure_DrainageArea.R

source(file.path("code", "paths+packages.R"))

## load data
gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_sample <- 
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv")) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID)) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID")

ggplot(gage_mean, aes(x = drain_sqkm, fill = region)) + 
  geom_histogram(binwidth = 2000) +
  scale_x_continuous(name = "Drainage Area [km\u00b2]") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Region", values = pal_regions) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1,1)) +
  ggsave(file.path("figures_manuscript", "DrainageArea_Histogram.png"),
         width = 120, height = 95, units = "mm")

min(gage_mean$drain_sqkm, 0.95)
max(gage_mean$drain_sqkm, 0.95)
quantile(gage_mean$drain_sqkm, 0.95)

# convert 0.05 cfs to [mm]
threshold_mm_per_sec <- (0.05*(0.3048^3))/(gage_sample$drain_sqkm*(1000^3))
threshold_mm_per_day <- 86400*(0.05*(0.3048^3))/(gage_sample$drain_sqkm*(1000^3))
min(threshold_mm_per_day)
max(threshold_mm_per_day)
