## Trends_MannKendall+MannWhitney.R
#' This script plots the results of the Mann-Kendall trend test
#' and the Mann-Whitney difference test.

source(file.path("code", "paths+packages.R"))

## metrics we care about
metrics <- c("annualnoflowdays", "zeroflowfirst", "peak2z_length")

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
  geom_point(data = subset(df_mk, metric == "annualnoflowdays"),
             aes(x = dec_long_va, y = dec_lat_va, 
                 color = mk_tau, shape = mk_p < p_thres)) +
  scale_x_continuous(name = NULL, breaks = seq(-120, -80, 20),
                     labels = c("120\u00b0W", "100\u00b0W", "80\u00b0W")) +
  scale_y_continuous(name = NULL, breaks = seq(30, 50, 10),
                     labels = c("30\u00b0N", "40\u00b0N", "50\u00b0N")) +
  scale_color_gradient2(name = "Kendall \u03c4", limits = c(tau_min, tau_max),
                        low = "#4575b4", mid = "#ffffbf", high = "#d73027") +
  scale_shape_manual(name = NULL, values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  coord_map() +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "right") +
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
  scale_shape_manual(name = NULL, values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  coord_map() +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "right") +
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
  scale_shape_manual(name = NULL, values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p > 0.05"))  +
  coord_map() +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "right") +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5),
         shape = guide_legend(order = 2, direction = "vertical"))

p_combo <- 
  ((p_afd + ggtitle("(a) Annual No-Flow Days")) + 
    (p_p2z + ggtitle("(b) Days from Peak to No-Flow")) + 
    (p_zff + ggtitle("(c) First No-Flow Day"))) +
  plot_layout(ncol = 1)

ggsave(file.path("figures_manuscript", "Trends_MannKendall-Maps.png"),
       p_combo, width = 190, height = 240, units = "mm")


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

# make a column for Mann-Whitney about significance and directio of change
df_mw$mw_sig[df_mw$mw_p > p_thres] <- "NotSig"
df_mw$mw_sig[df_mw$mw_p < p_thres & 
               df_mw$mw_diff_mean < 0 & 
               df_mw$metric %in% c("zeroflowfirst", "peak2z_length")] <- "SigDry"
df_mw$mw_sig[df_mw$mw_p < p_thres & 
               df_mw$mw_diff_mean < 0 & 
               df_mw$metric %in% c("annualnoflowdays")] <- "SigWet"
df_mw$mw_sig[df_mw$mw_p < p_thres & 
               df_mw$mw_diff_mean > 0 & 
               df_mw$metric %in% c("zeroflowfirst", "peak2z_length")] <- "SigWet"
df_mw$mw_sig[df_mw$mw_p < p_thres & 
               df_mw$mw_diff_mean > 0 & 
               df_mw$metric %in% c("annualnoflowdays")] <- "SigDry"

# histograms
p_mw_hist <-
  ggplot() +
  geom_histogram(data = df_mw, aes(x = mw_diff_mean, fill = mw_sig),
                 binwidth = 10) +
  geom_vline(xintercept = 0, color = "black") +
  facet_wrap(~metric, ncol = 3, scales = "free", labeller = 
               as_labeller(c("annualnoflowdays" = "(a) Annual No-Flow Days",
                             "peak2z_length" = "(b) Days from Peak to No-Flow",
                             "zeroflowfirst" = "(c) First No-Flow Day"))) +
  scale_x_continuous(name = "Change in Annual Mean [(1999 to 2017) - (1980 to 1998)]") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", 
                    values = c("SigDry" = col.cat.red, "SigWet" = col.cat.blu, "NotSig" = col.gray),
                    labels = c("SigDry" = "Drier", "SigWet" = "Wetter", "NotSig" = "No Change"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom") +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = 0.5)) +
  ggsave(file.path("figures_manuscript", "Trends_MannWhitney-Hist.png"),
         width = 190, height = 95, units = "mm")


# individual maps
p_mw_afd_map <- 
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "gray75", color = "white") +
  geom_point(data = subset(df_mw, mw_p < p_thres & metric == "annualnoflowdays"),  
             aes(x = dec_long_va, y = dec_lat_va, color = mw_diff_mean)) +
  labs(title = "(d) Annual No-Flow Days") +
  scale_x_continuous(name = "Longitude [\u00B0E]") +
  scale_y_continuous(name = "Latitude [\u00B0N]",
                     breaks = seq(30, 50, 10)) +
  scale_color_gradient2(name = "Change", 
                        low = "#4575b4", mid = "#ffffbf", high = "#d73027") +
  coord_map() +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5, barwidth = 10))

p_mw_p2z_map <- 
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "gray75", color = "white") +
  geom_point(data = subset(df_mw, mw_p < p_thres & metric == "peak2z_length"),  
             aes(x = dec_long_va, y = dec_lat_va, color = mw_diff_mean)) +
  labs(title = "(e) Days from Peak to No-Flow") +
  scale_x_continuous(name = "Longitude [\u00B0E]") +
  scale_y_continuous(name = "Latitude [\u00B0N]",
                     breaks = seq(30, 50, 10)) +
  scale_color_gradient2(name = "Change",
                        high = "#4575b4", mid = "#ffffbf", low = "#d73027") +
  coord_map() +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5, barwidth = 10))

p_mw_zff_map <- 
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "gray75", color = "white") +
  geom_point(data = subset(df_mw, mw_p < p_thres & metric == "zeroflowfirst"),  
             aes(x = dec_long_va, y = dec_lat_va, color = mw_diff_mean)) +
  labs(title = "(f) First No-Flow Day") + 
  scale_x_continuous(name = "Longitude [\u00B0E]") +
  scale_y_continuous(name = "Latitude [\u00B0N]",
                     breaks = seq(30, 50, 10)) +
  scale_color_gradient2(name = "Change",
                        high = "#4575b4", mid = "#ffffbf", low = "#d73027") +
  coord_map() +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5, barwidth = 10))

p_mw_map_combo <- 
  (p_mw_afd_map +
     p_mw_p2z_map +
     p_mw_zff_map) + 
  plot_layout(ncol = 3)

ggsave(file.path("figures_Manuscript", "Trends_MannWhitney-Maps.png"),
       p_mw_map_combo, width = 190, height = 90, units = "mm")  





# individual histograms
p_mw_afd_hist <-
  ggplot() +
  geom_histogram(data = subset(df_mw, metric == "annualnoflowdays"), 
                 aes(x = mw_diff_mean, fill = mw_sig),
                 binwidth = 5) +
  geom_vline(xintercept = 0, color = "black") +
  labs(title = "(a) Annual No-Flow Days") +
  scale_x_continuous(name = "Change") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", guide = NULL,
                    values = c("SigDry" = col.cat.red, "SigWet" = col.cat.blu, "NotSig" = col.gray),
                    labels = c("SigDry" = "Drier", "SigWet" = "Wetter", "NotSig" = "No Change"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")

p_mw_p2z_hist <-
  ggplot() +
  geom_histogram(data = subset(df_mw, metric == "peak2z_length"), 
                 aes(x = mw_diff_mean, fill = mw_sig),
                 binwidth = 5) +
  geom_vline(xintercept = 0, color = "black") +
  labs(title = "(c) Days from Peak to No-Flow") +
  scale_x_continuous(name = "Change") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", guide = NULL,
                    values = c("SigDry" = col.cat.red, "SigWet" = col.cat.blu, "NotSig" = col.gray),
                    labels = c("SigDry" = "Drier", "SigWet" = "Wetter", "NotSig" = "No Change"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")

p_mw_zff_hist <-
  ggplot() +
  geom_histogram(data = subset(df_mw, metric == "zeroflowfirst"), 
                 aes(x = mw_diff_mean, fill = mw_sig),
                 binwidth = 5) +
  geom_vline(xintercept = 0, color = "black") +
  labs(title = "(e) First No-Flow Day") +
  scale_x_continuous(name = "Change") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", guide = NULL,
                    values = c("SigDry" = col.cat.red, "SigWet" = col.cat.blu, "NotSig" = col.gray),
                    labels = c("SigDry" = "Drier", "SigWet" = "Wetter", "NotSig" = "No Change"))  +
  theme(panel.border = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")