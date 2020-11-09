## Trends_CompareTrendsToDrivers.R

source(file.path("code", "paths+packages.R"))

## load data
gage_mean <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_trends <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv"))

## reorganize trends so that you have a row for each gage and a column for each metric
# metrics to keep
metrics <- c("annualnoflowdays", "peak2z_length", "zeroflowfirst", "p.pet_cy")

gage_trends_wide <-
  gage_trends %>% 
  dplyr::select(gage_ID, mk_tau, metric) %>% 
  subset(metric %in% metrics) %>% 
  tidyr::pivot_wider(names_from = metric, values_from = mk_tau, names_prefix = "tau_") %>% 
  ## add region information and mean characteristics
  dplyr::left_join(gage_mean, by = "gage_ID")

## plot each no-flow metric against p/pet trend
# figure out tau limits
tau_min <- min(subset(gage_trends, metric %in% metrics)$mk_tau, na.rm=T)
tau_max <- max(subset(gage_trends, metric %in% metrics)$mk_tau, na.rm=T)
tau_abs <- max(c(tau_min, tau_max))

p_anfd.aridity <- 
  ggplot(gage_trends_wide, aes(y = tau_annualnoflowdays, x = tau_p.pet_cy)) +
  geom_hline(yintercept = 0, color = col.gray) + 
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(aes(color = region)) +
  scale_x_continuous(name = "P/PET, Kendall \u03c4", expand = c(0, 0.01)) +
  scale_y_continuous(name = "Annual No-Flow Days,\nKendall \u03c4", 
                     limits = c(-tau_abs, tau_abs), expand = c(0, 0.01)) +
  scale_color_manual(name = "Region", values = pal_regions,
                     labels = lab_regions_skinny1line) +
  stat_smooth(method = "lm", color = "black") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5, 
                              ncol = 2))

p_p2z.aridity <-
  ggplot(gage_trends_wide, aes(y = tau_peak2z_length, x = tau_p.pet_cy)) +
  geom_hline(yintercept = 0, color = col.gray) + 
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(aes(color = region)) +
  scale_x_continuous(name = "P/PET, Kendall \u03c4", expand = c(0, 0.01)) +
  scale_y_continuous(name = "Peak to No-Flow,\nKendall \u03c4", 
                     limits = c(-tau_abs, tau_abs), expand = c(0, 0.01)) +
  scale_color_manual(name = "Region", values = pal_regions,
                     labels = lab_regions_skinny1line) +
  stat_smooth(method = "lm", color = "black") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5, 
                              ncol = 2))

p_zff.aridity <- 
  ggplot(gage_trends_wide, aes(y = tau_zeroflowfirst, x = tau_p.pet_cy)) +
  geom_hline(yintercept = 0, color = col.gray) + 
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(aes(color = region)) +
  scale_x_continuous(name = "P/PET, Kendall \u03c4", expand = c(0, 0.01)) +
  scale_y_continuous(name = "First No-Flow Day,\nKendall \u03c4", 
                     limits = c(-tau_abs, tau_abs), expand = c(0, 0.01)) +
  scale_color_manual(name = "Region", values = pal_regions,
                     labels = lab_regions_skinny1line) +
  stat_smooth(method = "lm", color = "black") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5, 
                              ncol = 2))

# combine using patchwork
(((p_anfd.aridity + theme(axis.title.x = element_blank())) + 
      (p_p2z.aridity + theme(axis.title.x = element_blank())) + 
      (p_zff.aridity)) +
  plot_layout(ncol = 1, guides = "collect") + 
    plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.05, 1))) +
  ggsave(file.path("figures_manuscript", "Trends_CompareTrendsToDrivers-MetricsVsAridity.png"),
         width = 95, height = 210, units = "mm")

# get correlation between trends
cor(x = gage_trends_wide$tau_p.pet_cy, y = gage_trends_wide$tau_annualnoflowdays, use = "complete.obs")
cor(x = gage_trends_wide$tau_p.pet_cy, y = gage_trends_wide$tau_peak2z_length, use = "complete.obs")
cor(x = gage_trends_wide$tau_p.pet_cy, y = gage_trends_wide$tau_zeroflowfirst, use = "complete.obs")
