## Trends_ExploreResults.R
#' This script load the trend results and calculates some simple 
#' summary statistics for the trends.
#' 

source(file.path("code", "paths+packages.R"))

## metrics we care about
metrics <- c("annualnoflowdays", "zeroflowfirst", "peak2z_length")

## load data
gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_mean <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_trends <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv")) %>% 
  subset(metric %in% metrics) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID") %>% 
  dplyr::left_join(gage_mean[,c("gage_ID", "dec_lat_va", "dec_long_va")], by = "gage_ID")

# threshold for significant
p_thres <- 0.05

## plot: mann-kendall results
# load state map
states <- map_data("state")

df_mk <- 
  gage_trends %>% 
  dplyr::select(metric, gage_ID, region, dec_lat_va, dec_long_va, mk_tau, mk_p) %>% 
  subset(complete.cases(.))

ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "gray75", color = "white") +
  geom_point(data = df_mk,  aes(x = dec_long_va, y = dec_lat_va,
                                color = mk_tau, shape = mk_p < p_thres)) +
  facet_wrap(~metric, labeller = 
               as_labeller(c("annualnoflowdays" = "(a) Annual Days with Flow",
                             "peak2z_length" = "(b) Days from Peak to No-Flow",
                             "zeroflowfirst" = "(c) First No-Flow Day"))) +
  scale_x_continuous(name = "Longitude [\u00B0E]") +
  scale_y_continuous(name = "Latitude [\u00B0N]",
                     breaks = seq(30, 50, 10)) +
  scale_color_gradient2(name = "Kendall \u03c4",
                        high = "#4575b4", mid = "#ffffbf", low = "#d73027") +
  scale_shape_manual(name = "Significance", values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom") +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5),
         shape = guide_legend(order = 2, direction = "vertical"))

## plot: mann-whitney results
df_mw <- 
  gage_trends %>% 
  dplyr::mutate(mw_diff_mean = mw_meanGroup2 - mw_meanGroup1,
                mw_diff_median = mw_medianGroup2 - mw_medianGroup1) %>% 
  dplyr::select(metric, gage_ID, region, dec_lat_va, dec_long_va, mw_p, mw_diff_mean, mw_diff_median) %>% 
  subset(complete.cases(.))

# histograms
ggplot() +
  geom_histogram(data = df_mw, aes(x = mw_diff_mean, fill = mw_p < p_thres),
                 binwidth = 10) +
  geom_vline(xintercept = 0, color = "black") +
  facet_wrap(~metric, scales = "free", labeller = 
               as_labeller(c("annualnoflowdays" = "(a) Annual Days with Flow",
                             "peak2z_length" = "(b) Days from Peak to No-Flow",
                             "zeroflowfirst" = "(c) First No-Flow Day"))) +
  scale_x_continuous(name = "Change in Annual Value [1999 to 2017 Mean - 1980 to 1998 Mean]") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", values = c("TRUE" = col.cat.blu, "FALSE" = col.gray),
                    labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom") +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5),
         shape = guide_legend(order = 2, direction = "vertical"))

# maps
ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "gray75", color = "white") +
  geom_point(data = df_mw,  aes(x = dec_long_va, y = dec_lat_va,
                                color = mw_diff, shape = mw_p < p_thres)) +
  facet_wrap(~metric, labeller = 
               as_labeller(c("annualnoflowdays" = "(a) Annual Days with Flow",
                             "peak2z_length" = "(b) Days from Peak to No-Flow",
                             "zeroflowfirst" = "(c) First No-Flow Day"))) +
  scale_x_continuous(name = "Longitude [\u00B0E]") +
  scale_y_continuous(name = "Latitude [\u00B0N]",
                     breaks = seq(30, 50, 10)) +
  scale_color_gradient2(name = "Change in Annual Mean Value",
                        high = "#4575b4", mid = "#ffffbf", low = "#d73027") +
  scale_shape_manual(name = "Significance", values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom") +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5),
         shape = guide_legend(order = 2, direction = "vertical"))