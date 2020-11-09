## Trends_Boxplots.R
#' This script load the trend results and makes regional boxplots.
#' It is modified from Kendra's code in 'code/TrendPatterns.R'
#' 

source(file.path("code", "paths+packages.R"))

## metrics we want to plot, in the order they should be plotted
metrics <- c("annualnoflowdays", "peak2z_length", "zeroflowfirst", 
             "p_mm_cy", "pet_mm_cy")

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

## prep some formatting details
# set metric as factor to preserve order for plotting
gage_trends$metric <- factor(gage_trends$metric, levels = metrics)

# figure out tau axis limits
tau_min <- min(gage_trends$mk_tau, na.rm = T)
tau_max <- max(gage_trends$mk_tau, na.rm = T)
tau_abs <- max(abs(c(tau_min, tau_max)))

# facet labels
labs_metrics <- c("annualnoflowdays" = "(a) Annual No-Flow Days", 
                  "peak2z_length" = "(b) Days from Peak to No-Flow", 
                  "zeroflowfirst" = "(c) First No-Flow Day", 
                  "p_mm_cy" = "(d) Precipitation", 
                  "pet_mm_cy" = "(e) Potential ET")

## boxplots by region for the three metrics, plus precip, temp, PET
gage_trends %>% 
  ggplot(aes(x = region, y = mk_tau, fill = region, color = region)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_boxplot() +
  facet_wrap(~metric, ncol = 1, dir = "v",
             scales = "free_y", labeller = as_labeller(labs_metrics)) +
  scale_x_discrete(name = "Region", labels = lab_regions_skinny) +
  scale_y_continuous(name = "Kendall \u03c4", limits = c(-tau_abs, tau_abs),
                     breaks = seq(-0.6, 0.6, 0.3), expand = c(0, 0.01)) +
  scale_fill_manual(values = pal_regions, guide = NULL) +
  scale_color_manual(values = pal_regions_dk, guide = NULL) +
  ggsave(file.path("figures_manuscript", "Trends_Boxplots-Region.png"),
         width = 95, height = 240, units = "mm")
