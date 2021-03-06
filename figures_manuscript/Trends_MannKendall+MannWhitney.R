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

gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv")) %>% 
  dplyr::select(gage_ID, region)

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
         shape = "none")

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
         shape = "none")

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
         shape = "none")

p_combo <- 
  ((p_afd + ggtitle("(a) Annual No-Flow Days")) + 
     (p_p2z + ggtitle("(b) Days from Peak to No-Flow")) + 
     (p_zff + ggtitle("(c) First No-Flow Day"))) +
  plot_layout(ncol = 1) & 
  theme(plot.title = element_text(face = "plain"))

ggsave(file.path("figures_manuscript", "Trends_MannKendall-Maps.png"),
       p_combo, width = 190, height = 200, units = "mm")

ggsave(file.path("figures_manuscript", "Figure2.pdf"),
       p_combo, width = 140, height = 200, units = "mm", device = cairo_pdf)

## comparison among trends
df_mk_wide <-
  df_mk %>% 
  dplyr::select(metric, gage_ID, mk_tau, mk_p) %>% 
  pivot_wider(names_from = metric, values_from = c(mk_tau, mk_p)) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID")

p_mk_anf.p2z <- 
  ggplot(df_mk_wide, aes(x = mk_tau_annualnoflowdays, y = mk_tau_peak2z_length)) +
  geom_hline(yintercept = 0, color = col.gray) + geom_vline(xintercept = 0, color = col.gray) +
  geom_point(shape = 1) +
  scale_x_continuous(name = "Kendall \u03c4, Annual No-Flow Days", 
                     limits = c(tau_min, tau_max), expand = c(0,0.01)) +
  scale_y_continuous(name = "Kendall \u03c4, Peak to No-Flow", 
                     limits = c(tau_min, tau_max), expand = c(0,0.01)) +
  stat_smooth(method = "lm", color = col.cat.blu)

p_mk_anf.zff <-
  ggplot(df_mk_wide, aes(x = mk_tau_annualnoflowdays, y = mk_tau_zeroflowfirst)) +
  geom_hline(yintercept = 0, color = col.gray) + geom_vline(xintercept = 0, color = col.gray) +
  geom_point(shape = 1) +
  scale_x_continuous(name = "Kendall \u03c4, Annual No-Flow Days", 
                     limits = c(tau_min, tau_max), expand = c(0,0.01)) +
  scale_y_continuous(name = "Kendall \u03c4, First No-Flow Day", 
                     limits = c(tau_min, tau_max), expand = c(0,0.01)) +
  stat_smooth(method = "lm", color = col.cat.blu)

p_mk_p2z.zff <-
  ggplot(df_mk_wide, aes(y = mk_tau_peak2z_length, x = mk_tau_zeroflowfirst)) +
  geom_hline(yintercept = 0, color = col.gray) + geom_vline(xintercept = 0, color = col.gray) +
  geom_point(shape = 1) +
  scale_y_continuous(name = "Kendall \u03c4, Peak to No-Flow", 
                     limits = c(tau_min, tau_max), expand = c(0,0.01)) +
  scale_x_continuous(name = "Kendall \u03c4, First No-Flow Day", 
                     limits = c(tau_min, tau_max), expand = c(0,0.01)) +
  stat_smooth(method = "lm", color = col.cat.blu)

((p_mk_anf.p2z + p_mk_anf.zff + p_mk_p2z.zff) +
    plot_layout(ncol = 1) + 
    plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
    theme(plot.title = element_text(face = "plain"),
          plot.tag.position = c(0.07, 1))) +
  ggsave(file.path("figures_manuscript", "Trends_MannKendall-MetricComparison.png"),
         width = 95, height = 210, units = "mm")

## mann-kendall stats
# percent of gages with significant, positive, negative trends
n_anf_mk <- dim(subset(df_mk, metric == "annualnoflowdays"))[1] # sufficient data
df_mk %>% subset(metric == "annualnoflowdays" & mk_p < p_thres) %>% dim()/n_anf_mk
df_mk %>% subset(metric == "annualnoflowdays" & mk_tau > 0 & mk_p < p_thres) %>% dim()/n_anf_mk # drying
df_mk %>% subset(metric == "annualnoflowdays" & mk_tau < 0 & mk_p < p_thres) %>% dim()/n_anf_mk # wetting

n_p2z_mk <- dim(subset(df_mk, metric == "peak2z_length"))[1] # sufficient data
df_mk %>% subset(metric == "peak2z_length" & mk_p < p_thres) %>% dim()/n_p2z_mk
df_mk %>% subset(metric == "peak2z_length" & mk_tau < 0 & mk_p < p_thres) %>% dim()/n_p2z_mk # faster/drying
df_mk %>% subset(metric == "peak2z_length" & mk_tau > 0 & mk_p < p_thres) %>% dim()/n_p2z_mk # slower/wetting

n_zff_mk <- dim(subset(df_mk, metric == "zeroflowfirst"))[1] # sufficient data
df_mk %>% subset(metric == "zeroflowfirst" & mk_p < p_thres) %>% dim()/n_zff_mk
df_mk %>% subset(metric == "zeroflowfirst" & mk_tau < 0 & mk_p < p_thres) %>% dim()/n_zff_mk # earlier/drying
df_mk %>% subset(metric == "zeroflowfirst" & mk_tau > 0 & mk_p < p_thres) %>% dim()/n_zff_mk # later/wetting

# significant trend in any of the three metrics
df_mk %>% subset(metric %in% metrics & mk_p < p_thres) %>% dplyr::select(gage_ID) %>% unique() %>% dim()


# correlation between trends
cor(x = df_mk_wide$mk_tau_annualnoflowdays, y = df_mk_wide$mk_tau_peak2z_length, use = "complete.obs")
cor(x = df_mk_wide$mk_tau_annualnoflowdays, y = df_mk_wide$mk_tau_zeroflowfirst, use = "complete.obs")
cor(x = df_mk_wide$mk_tau_zeroflowfirst, y = df_mk_wide$mk_tau_peak2z_length, use = "complete.obs")

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
  geom_vline(xintercept = 0, color = "#ffffbf") +
  facet_wrap(~metric, ncol = 3, scales = "free", labeller = 
               as_labeller(c("annualnoflowdays" = "(a) Annual No-Flow Days",
                             "peak2z_length" = "(b) Days from Peak to No-Flow",
                             "zeroflowfirst" = "(c) First No-Flow Day"))) +
  scale_x_continuous(name = "Change in Annual Mean, (1999 to 2017) - (1980 to 1998) [days]") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", 
                    values = c("SigDry" = col.cat.red, "SigWet" = col.cat.blu, "NotSig" = col.gray),
                    labels = c("SigDry" = "Drier", "SigWet" = "Wetter", "NotSig" = "No Change"))  +
  theme(panel.border = element_blank(),
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
  scale_color_gradient2(name = "Change [days]", 
                        low = "#4575b4", mid = "#ffffbf", high = "#d73027",
                        limits = c(-100, 100), oob = scales::squish,
                        breaks = seq(-100, 100, 50),
                        labels = c("<-100", "-50", "0", "50", ">100")) +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5, barwidth = 9)) +
  coord_map() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

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
  scale_color_gradient2(name = "Change [days]", 
                        low = "#d73027", mid = "#ffffbf", high = "#4575b4", 
                        limits = c(-25, 25), oob = scales::squish,
                        breaks = c(-25, -12, 0, 12, 25),
                        labels = c("<-25", "-12", "0", "12", ">25")) +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5, barwidth = 9)) +
  coord_map() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

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
  scale_color_gradient2(name = "Change [days]", 
                        low = "#d73027", mid = "#ffffbf", high = "#4575b4", 
                        limits = c(-60, 60), oob = scales::squish,
                        labels = c("<-60", "-30", "0", "30", ">60")) +
  guides(color = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5, barwidth = 9)) +
  coord_map() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

p_mw_map_combo <- 
  (p_mw_afd_map +
     p_mw_p2z_map +
     p_mw_zff_map) + 
  plot_layout(ncol = 3)

((p_mw_hist / p_mw_map_combo) +
    plot_layout(heights = c(0.4, 0.6))) +
  ggsave(file.path("figures_Manuscript", "Trends_MannWhitney-Hist+Maps.png"),
         width = 190, height = 150, units = "mm") 

((p_mw_hist / p_mw_map_combo) +
    plot_layout(heights = c(0.4, 0.6))) +
  ggsave(file.path("figures_Manuscript", "Figure4.pdf"),
         width = 190, height = 130, units = "mm", device = cairo_pdf) 

## mann-whitney stats
# percent of gages with significant, positive, negative trends
n_anf_mw <- dim(subset(df_mw, metric == "annualnoflowdays"))[1]
df_mw %>% subset(metric == "annualnoflowdays" & mw_p < p_thres) %>% dim()/n_anf_mw
df_mw %>% subset(metric == "annualnoflowdays" & mw_diff_mean > 0 & mw_p < p_thres) %>% dim()/n_anf_mw # drying
df_mw %>% subset(metric == "annualnoflowdays" & mw_diff_mean < 0 & mw_p < p_thres) %>% dim()/n_anf_mw # wetting

n_p2z_mw <- dim(subset(df_mw, metric == "peak2z_length"))[1]
df_mw %>% subset(metric == "peak2z_length" & mw_p < p_thres) %>% dim()/n_p2z_mw
df_mw %>% subset(metric == "peak2z_length" & mw_diff_mean < 0 & mw_p < p_thres) %>% dim()/n_p2z_mw # faster/drying
df_mw %>% subset(metric == "peak2z_length" & mw_diff_mean > 0 & mw_p < p_thres) %>% dim()/n_p2z_mw # slower/wetting

n_zff_mw <- dim(subset(df_mw, metric == "zeroflowfirst"))[1]
df_mw %>% subset(metric == "zeroflowfirst" & mw_p < p_thres) %>% dim()/n_zff_mw
df_mw %>% subset(metric == "zeroflowfirst" & mw_diff_mean < 0 & mw_p < p_thres) %>% dim()/n_zff_mw # earlier/drying
df_mw %>% subset(metric == "zeroflowfirst" & mw_diff_mean > 0 & mw_p < p_thres) %>% dim()/n_zff_mw # later/wetting

# significant change in any of the three metrics
df_mw %>% subset(metric %in% metrics & mw_p < p_thres) %>% dplyr::select(gage_ID) %>% unique() %>% dim()

# range of significant change
df_mw %>% subset(metric == "annualnoflowdays" & mw_p < p_thres) %>% dplyr::select(mw_diff_mean) %>% range()
df_mw %>% subset(metric == "peak2z_length" & mw_p < p_thres) %>% dplyr::select(mw_diff_mean) %>% range()
df_mw %>% subset(metric == "zeroflowfirst" & mw_p < p_thres) %>% dplyr::select(mw_diff_mean) %>% range()

# big changes
df_mw %>% subset(metric == "annualnoflowdays" & mw_diff_mean > 100 & mw_p < p_thres) %>% dim()
df_mw %>% subset(metric == "annualnoflowdays" & mw_diff_mean < -100 & mw_p < p_thres) %>% dim()
