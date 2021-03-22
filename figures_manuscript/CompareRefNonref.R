## CompareRefNonref.R
# This script is intended to compare intermittence metrics for the reference and non-reference gages.

source(file.path("code", "paths+packages.R"))

## load data
gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_sample <- 
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv")) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID)) %>% 
  dplyr::select(gage_ID, CLASS) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID")

gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv")) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID") %>% 
  dplyr::left_join(gage_sample, by = "gage_ID") %>% 
  # add some derived variables
  dplyr::mutate(p.pet_cy = p_mm_cy/pet_mm_cy,
                swe.p_cy = swe_mm_cy/p_mm_cy,
                p.pet_jfm = p_mm_jfm/pet_mm_jfm,
                swe.p_jfm = swe_mm_jfm/p_mm_jfm,
                p.pet_amj = p_mm_amj/pet_mm_amj,
                swe.p_amj = swe_mm_amj/p_mm_amj,
                p.pet_jas = p_mm_jas/pet_mm_jas,
                swe.p_jas = swe_mm_jas/p_mm_jas,
                p.pet_ond = p_mm_ond/pet_mm_ond,
                swe.p_ond = swe_mm_ond/p_mm_ond)

gage_trends <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv")) %>% 
  dplyr::left_join(gage_sample[,c("gage_ID", "CLASS", "region")], by = "gage_ID")

## collect data for plots
metrics <- c("annualnoflowdays", "zeroflowfirst", "peak2z_length")

df_annual <- 
  gage_sample_annual %>% 
  dplyr::select(gage_ID, CLASS, currentclimyear, region, all_of(metrics)) %>% 
  tidyr::pivot_longer(cols = all_of(metrics),
                      names_to = "metric")

df_mean <- 
  df_annual %>% 
  dplyr::group_by(gage_ID, CLASS, region, metric) %>% 
  dplyr::summarize(value_mean = mean(value, na.rm = T))

df_trends <- 
  gage_trends %>% 
  subset(metric %in% metrics) 

## compare mean values
# boxplots - mean, all gages
ggplot(df_mean, aes(x = metric, y = value_mean, fill = CLASS)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_fill_manual(name = "Gage Class", labels = c("Non-Reference", "Reference"),
                    values = c(col.cat.org, col.cat.grn)) +
  scale_x_discrete(name = "Intermittency Signature", 
                   labels = c("No-Flow\nDays", "Peak to\nNo-Flow", "First No-Flow\nDay")) +
  scale_y_continuous(name = "Gage Average [days]", limits = c(0, 366), expand = c(0,0)) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1)) +
  ggsave(file.path("figures_manuscript", "CompareRefNonref_Violin-MeanAllGage.png"),
         width = 95, height = 95, units = "mm")

## compare trends
# boxplots - trend, all gages
ggplot(df_trends, aes(x = metric, y = mk_tau, fill = CLASS)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_violin(draw_quantiles = 0.5) +
  scale_fill_manual(name = "Gage Class", labels = c("Non-Reference", "Reference"),
                      values = c(col.cat.org, col.cat.grn)) +
  scale_x_discrete(name = "Intermittency Signature", 
                   labels = c("No-Flow\nDays", "Peak to\nNo-Flow", "First No-Flow\nDay")) +
  scale_y_continuous(name = "Kendall \u03c4") +
  ggsave(file.path("figures_manuscript", "CompareRefNonref_Violin-TrendAllGage.png"),
         width = 120, height = 95, units = "mm")
