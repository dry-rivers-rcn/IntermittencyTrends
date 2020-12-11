## Trends_Violins.R
#' This script load the trend results and makes regional boxplots.
#' It is modified from Kendra's code in 'code/TrendPatterns.R'
#' 

source(file.path("code", "paths+packages.R"))

## metrics we want to plot, in the order they should be plotted
metrics <- c("annualnoflowdays", "peak2z_length", "zeroflowfirst")

## load data
gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_mean <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_trends <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv")) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID") %>% 
  dplyr::left_join(gage_mean[,c("gage_ID", "dec_lat_va", "dec_long_va")], by = "gage_ID")

## prep some formatting details
# just hydro metrics of interest
gage_hydro_trends <- 
  subset(gage_trends, metric %in% metrics)

# set metric as factor to preserve order for plotting
gage_hydro_trends$metric <- factor(gage_hydro_trends$metric, levels = metrics)

# figure out tau axis limits
tau_min <- min(gage_hydro_trends$mk_tau, na.rm = T)
tau_max <- max(gage_hydro_trends$mk_tau, na.rm = T)
tau_abs <- max(abs(c(tau_min, tau_max)))

# facet labels
labs_metrics <- c("annualnoflowdays" = "Annual No-Flow Days", 
                  "peak2z_length" = "Days from Peak to No-Flow", 
                  "zeroflowfirst" = "First No-Flow Day")

## boxplots by region for the three metrics, plus precip, temp, PET
p_region <- 
  gage_hydro_trends %>% 
  ggplot(aes(x = region, y = mk_tau, fill = region, color = region)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_violin(draw_quantiles = 0.5) +
  facet_wrap(~metric, ncol = 1, dir = "v",
             scales = "free_y", labeller = as_labeller(labs_metrics)) +
  scale_x_discrete(name = "Region", labels = lab_regions_skinny) +
  scale_y_continuous(name = "Kendall \u03c4", limits = c(-tau_abs, tau_abs),
                     breaks = seq(-0.6, 0.6, 0.3), expand = c(0, 0.01)) +
  scale_fill_manual(values = pal_regions, guide = NULL) +
  scale_color_manual(values = pal_regions_dk, guide = NULL) +
  ggsave(file.path("figures_manuscript", "Trends_Violins-Region.png"),
         width = 95, height = 240, units = "mm")

## boxplot by latitude
gage_hydro_trends$lat_bin <- cut(gage_hydro_trends$dec_lat_va, 
                                 breaks = seq(26, 50, 3),
                                 labels = seq(27.5, 50, 3))

p_lat <- 
  ggplot(gage_hydro_trends, aes(x = lat_bin, y = mk_tau)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_violin(draw_quantiles = 0.5) +
  facet_wrap(~metric, ncol = 1, dir = "v",
             scales = "free_y", labeller = as_labeller(labs_metrics)) +
  coord_flip() +
  scale_y_continuous(name = "Kendall \u03c4", limits = c(-tau_abs, tau_abs),
                     breaks = seq(-0.6, 0.6, 0.3), expand = c(0, 0.01)) +
    scale_x_discrete(name = "Latitude [\u00b0N]")


# combine    
(p_region + p_lat + plot_layout(widths = c(0.6, 0.4))) + 
  ggsave(file.path("figures_manuscript", "Trends_Violins-Region+Lat_NoText.pdf"),
         width = 190, height = 140, units = "mm", device = cairo_pdf)

# number by region
gage_hydro_trends %>% 
  subset(is.finite(mk_tau)) %>% 
  dplyr::group_by(metric, region) %>% 
  dplyr::summarize(n_gages = n())
