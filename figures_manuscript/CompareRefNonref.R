## CompareRefNonref.R
# This script is intended to compare intermittence metrics for the reference and non-reference gages.

source(file.path("code", "paths+packages.R"))

## load data
gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_sample <- 
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv")) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID)) %>% 
  dplyr::select(gage_ID, CLASS)

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
#  dplyr::group_by(gage_ID, CLASS, region, metric) %>% 
#  dplyr::summarize(anf_mean = mean(annualnoflowdays, na.rm = T),
#                   p2z_mean = mean(peak2z_length, na.rm = T),
#                   zff_mean = mean(zeroflowfirst, na.rm = T))

## boxplots - mean, all gages
ggplot(df_mean, aes(x = metric, y = value_mean, fill = CLASS)) +
  geom_boxplot(outlier.shape = 1) +
  scale_fill_discrete(name = "Gage Class", labels = c("Non-Reference", "Reference")) +
  scale_x_discrete(name = "Intermittency Signature", 
                   labels = c("No-Flow\nDays", "Peak to\nNo-Flow", "First No-Flow\nDay")) +
  scale_y_continuous(name = "Gage Average [days]", limits = c(0, 366), expand = c(0,0)) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1)) +
  ggsave(file.path("figures_manuscript", "CompareRefNonref_Boxplots-AllGageMean.png"),
         width = 95, height = 95, units = "mm")

## boxplots - mean, by region
ggplot(df_mean, aes(x = region, y = value_mean, fill = CLASS)) +
  geom_boxplot() +
  facet_wrap(~metric)

## boxplots - annual, all gages
ggplot(df_annual, aes(x = metric, y = value, fill = CLASS)) +
  geom_boxplot()
