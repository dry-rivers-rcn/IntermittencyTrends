## Redundancy_CompareTrends.R
# This script is intended to compare the trends including and excluding redundant gages.

source(file.path("code", "paths+packages.R"))

## metrics we care about
metrics <- c("annualnoflowdays", "zeroflowfirst", "peak2z_length")

## load data
redundant_gages <- readr::read_csv(file.path("results", "RedundancyAnalysis_RedundantGages.csv"))

gage_mean <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_trends <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv")) %>% 
  subset(metric %in% metrics) %>% 
  dplyr::left_join(gage_mean[,c("gage_ID", "region", "dec_lat_va", "dec_long_va")], by = "gage_ID") %>% 
  dplyr::left_join(redundant_gages, by = "gage_ID")

table(gage_trends$region[gage_trends$redundant & gage_trends$metric == "annualnoflowdays"])

# load state map
states <- map_data("state")

# p-value threshold for significance
p_thres <- 0.05

## map of gages
sf_gages <- 
  gage_trends %>%
  dplyr::select(dec_lat_va, dec_long_va, redundant) %>% 
  unique() %>% 
  sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326) 

ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = col.gray) +
  geom_sf(data = sf_gages, aes(color = is.na(redundant))) +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  scale_color_manual(name = NULL,
                     values = c("TRUE" = "black", "FALSE" = col.cat.red),
                     labels = c("TRUE" = "Not Redundant", "FALSE" = "Redundant")) +
  coord_sf() +
  theme(panel.border = element_blank(),
        legend.position = "bottom") +
  ggsave(file.path("figures_manuscript", "Redundancy_MapOfGages.png"),
         width = 190, height = 95, units = "mm")

## gages removed by trend significance
gage_trends %>% 
  subset(redundant & is.finite(mk_p)) %>% 
  ggplot(aes(x = metric, fill = mk_p < p_thres)) +
  geom_bar() +
  scale_x_discrete(name = "Intermittency Signature") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Kendall Significance", 
                    values = c("FALSE" = col.cat.yel, "TRUE" = col.cat.red), 
                    labels = c("FALSE" = "p > 0.05", "TRUE" = "p < 0.05")) +
  labs(title = "Trend Significance of Redundant Gages") +
  theme(legend.position = "bottom")

## compare median trend by region with and without redundant gages
gage_trends_regionRemoveRedundant <- 
  gage_trends %>% 
  subset(is.na(redundant)) %>% 
  dplyr::group_by(region, metric) %>% 
  dplyr::summarize(mk_tau_median = median(mk_tau, na.rm = T))
gage_trends_region <- 
  gage_trends %>% 
  dplyr::group_by(region, metric) %>% 
  dplyr::summarize(mk_tau_median = median(mk_tau, na.rm = T)) %>% 
  dplyr::left_join(gage_trends_regionRemoveRedundant, by = c("region", "metric"), suffix = c(".all", ".trim"))

ggplot(gage_trends_region, aes(x = mk_tau_median.all, y = mk_tau_median.trim, color = region, shape = metric)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point() +
  scale_x_continuous(name = "Median Kendall \u03c4 (all gages)") +
  scale_y_continuous(name = "Median Kendall \u03c4 (trimmed for redundancy)") +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_shape_discrete(name = "Intermittency Signature",
                       labels = c("annualnoflowdays" = "Annual No-Flow Days",
                                  "peak2z_length" = "Days from Peak to No-Flow",
                                  "zeroflowfirst" = "First No-Flow Day")) +
  ggsave(file.path("figures_manuscript", "Redundancy_CompareMedianTrend.png"),
         width = 190, height = 120, units = "mm")
