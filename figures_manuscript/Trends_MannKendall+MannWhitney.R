## Trends_MannKendall+MannWhitney.R
#' This script plots the results of the Mann-Kendall trend test
#' and the Mann-Whitney difference test.

source(file.path("code", "paths+packages.R"))

## metrics we care about
metrics <- c("annualflowdays", "zeroflowfirst", "peak2z_length")

## load data
gage_mean <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_trends <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv")) %>% 
  subset(metric %in% metrics) %>% 
  dplyr::left_join(gage_mean[,c("gage_ID", "region", "dec_lat_va", "dec_long_va")], by = "gage_ID")

# load state map
states <- map_data("state")

# p-value threshold for significance
p_thres <- 0.05

####
#### mann-kendall
####

df_mk <- 
  gage_trends %>% 
  dplyr::select(metric, gage_ID, region, dec_lat_va, dec_long_va, mk_tau, mk_p) %>% 
  subset(complete.cases(.))

# multiple panels using patchwork, which allows placement of legend in blank panel
tau_min <- min(df_mk$mk_tau)
tau_max <- max(df_mk$mk_tau)

p_afd <- 
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), 
               fill = "gray75", color = "white") +
  geom_point(data = subset(df_mk, metric == "annualflowdays"),
             aes(x = dec_long_va, y = dec_lat_va, 
                 color = mk_tau, shape = mk_p < p_thres)) +
  scale_x_continuous(name = NULL, breaks = seq(-120, -80, 20),
                     labels = c("120\u00b0W", "100\u00b0W", "80\u00b0W")) +
  scale_y_continuous(name = NULL, breaks = seq(30, 50, 10),
                     labels = c("30\u00b0N", "40\u00b0N", "50\u00b0N")) +
  scale_color_gradient2(name = "Kendall \u03c4", limits = c(tau_min, tau_max),
                        high = "#4575b4", mid = "#ffffbf", low = "#d73027") +
  scale_shape_manual(name = "Significance", values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "bottom") +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5),
         shape = guide_legend(order = 2, direction = "vertical"))

p_p2z <- 
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), 
               fill = "gray75", color = "white") +
  geom_point(data = subset(df_mk, metric == "peak2z_length"),
             aes(x = dec_long_va, y = dec_lat_va, 
                 color = mk_tau, shape = mk_p < p_thres)) +
  scale_x_continuous(name = NULL, breaks = seq(-120, -80, 20),
                     labels = c("120\u00b0W", "100\u00b0W", "80\u00b0W")) +
  scale_y_continuous(name = NULL, breaks = seq(30, 50, 10),
                     labels = c("30\u00b0N", "40\u00b0N", "50\u00b0N")) +
  scale_color_gradient2(name = "Kendall \u03c4", limits = c(tau_min, tau_max),
                        high = "#4575b4", mid = "#ffffbf", low = "#d73027") +
  scale_shape_manual(name = "Significance", values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "bottom") +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5),
         shape = guide_legend(order = 2, direction = "vertical"))

p_zff <- 
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), 
               fill = "gray75", color = "white") +
  geom_point(data = subset(df_mk, metric == "zeroflowfirst"),
             aes(x = dec_long_va, y = dec_lat_va, 
                 color = mk_tau, shape = mk_p < p_thres)) +
  scale_x_continuous(name = NULL, breaks = seq(-120, -80, 20),
                     labels = c("120\u00b0W", "100\u00b0W", "80\u00b0W")) +
  scale_y_continuous(name = NULL, breaks = seq(30, 50, 10),
                     labels = c("30\u00b0N", "40\u00b0N", "50\u00b0N")) +
  scale_color_gradient2(name = "Kendall \u03c4", limits = c(tau_min, tau_max),
                        high = "#4575b4", mid = "#ffffbf", low = "#d73027") +
  scale_shape_manual(name = "Significance", values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "bottom") +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5),
         shape = guide_legend(order = 2, direction = "vertical"))

p_combo <- 
  ((p_afd + ggtitle("(a) Annual Days with Flow")) + 
    (p_p2z + ggtitle("(b) Days from Peak to No-Flow")) + 
    (p_zff + ggtitle("(c) First No-Flow Day")) + 
    guide_area()) +
  plot_layout(ncol = 2,
              guides = "collect")

ggsave(file.path("figures_manuscript", "Trends_MannKendall-Maps.png"),
       p_combo, width = 190, height = 140, units = "mm")


# ## single map using facet_wrap
# p_map <- 
#   ggplot() +
#   geom_polygon(data = states, aes(x = long, y = lat, group = group), 
#                fill = "gray75", color = "white") +
#   geom_point(data = df_mk,  aes(x = dec_long_va, y = dec_lat_va, 
#                                 color = mk_tau, shape = mk_p < p_thres)) +
#   facet_wrap(~metric, ncol = 1, labeller = as_labeller(
#     c("annualflowdays" = "(a) Annual Days with Flow",
#       "peak2z_length" = "(b) Days from Peak to No-Flow",
#       "zeroflowfirst" = "(c) First No-Flow Day"))) +
#   scale_x_continuous(name = "Longitude [\u00B0E]") +
#   scale_y_continuous(name = "Latitude [\u00B0N]",
#                      breaks = seq(30, 50, 10)) +
#   scale_color_gradient2(name = "Kendall \u03c4", 
#                         high = "#4575b4", mid = "#ffffbf", low = "#d73027") +
#   scale_shape_manual(name = "Significance", values = c("TRUE" = 16, "FALSE" = 1),
#                      labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
#   theme(panel.border = element_blank(),
#         strip.text = element_text(face = "bold"),
#         legend.position = "bottom") +
#   guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5),
#          shape = guide_legend(order = 2, direction = "vertical"))
# 
# ggsave(file.path("figures_manuscript", "Trends_MannKendall-Maps.png"),
#        p_map, width = 95, height = 200, units = "mm")

####
#### mann-whitney
####


## plot: mann-whitney results
df_mw <- 
  gage_trends %>% 
  dplyr::mutate(mw_diff_mean = mw_meanGroup2 - mw_meanGroup1,
                mw_diff_median = mw_medianGroup2 - mw_medianGroup1) %>% 
  dplyr::select(metric, gage_ID, region, dec_lat_va, dec_long_va, mw_p, mw_diff_mean, mw_diff_median) %>% 
  subset(complete.cases(.))

# histograms
p_mw_hist <-
  ggplot() +
  geom_histogram(data = df_mw, aes(x = mw_diff_mean, fill = mw_p < p_thres),
                 binwidth = 10) +
  geom_vline(xintercept = 0, color = "black") +
  facet_wrap(~metric, ncol = 1, scales = "free", labeller = 
               as_labeller(c("annualflowdays" = "(a) Annual Days with Flow",
                             "peak2z_length" = "(c) Days from Peak to No-Flow",
                             "zeroflowfirst" = "(e) First No-Flow Day"))) +
  scale_x_continuous(name = "Change in Annual Mean\n[(1999 to 2017) - (1980 to 1998)]") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", values = c("TRUE" = col.cat.grn, "FALSE" = col.gray),
                    labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom") +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = 0.5))

# maps
p_mw_map <- 
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "gray75", color = "white") +
  geom_point(data = subset(df_mw, mw_p < p_thres),  aes(x = dec_long_va, y = dec_lat_va,
                                color = mw_diff_mean)) +
  facet_wrap(~metric, ncol = 1, labeller = 
               as_labeller(c("annualflowdays" = "(b) Annual Days with Flow",
                             "peak2z_length" = "(d) Days from Peak to No-Flow",
                             "zeroflowfirst" = "(f) First No-Flow Day"))) +
  scale_x_continuous(name = "Longitude [\u00B0E]") +
  scale_y_continuous(name = "Latitude [\u00B0N]",
                     breaks = seq(30, 50, 10)) +
  scale_color_gradient2(name = "Change in Annual Mean\n[(1999 to 2017) - (1980 to 1998)]",
                        high = "#4575b4", mid = "#ffffbf", low = "#d73027") +
  #scale_shape_manual(name = "Significance", values = c("TRUE" = 16, "FALSE" = 1),
  #                   labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom") +
  guides(color = guide_colorbar(order = 1, barwidth = 10,
                                title.position = "top", title.hjust = 0.5),
         shape = guide_legend(order = 2, direction = "vertical"))

p_mw_combo <- 
  (p_mw_hist + p_mw_map) +
  plot_layout(ncol = 2, widths = c(1, 2))

ggsave(file.path("figures_Manuscript", "Trends_MannWhitney-Hist+Maps.png"),
       p_mw_combo, width = 190, height = 220, units = "mm")  
