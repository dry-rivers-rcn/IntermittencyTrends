## MannWhitney_SplitYearSensitivity.R
# This script will test the sensitivity of the Mann-Whitney results to the year chosen for the split.

source(file.path("code", "paths+packages.R"))

## load data
gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv")) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID")

## variables  to test?
vars_all <- c("annualnoflowdays", "zeroflowfirst", "peak2z_length")

## years to split for mann-whitney? (this will be included in the first set)
mw_yr_all <- seq(1994, by = 1, length.out = 9)

## loop through gages
sites <- unique(gage_sample_annual$gage_ID)
start_flag <- T
for (s in sites){
  for (var in vars_all){
    for (mw_yr_split in mw_yr_all){
      
      # mann-whitney groups
      group1 <- 
        gage_sample_annual %>% 
        subset(gage_ID == s & currentclimyear <= mw_yr_split) %>% 
        dplyr::pull(var)
      group2 <- 
        gage_sample_annual %>% 
        subset(gage_ID == s & currentclimyear > mw_yr_split) %>% 
        dplyr::pull(var)
      
      if (sum(is.finite(group1)) > 5 & sum(is.finite(group2)) > 5){
        mw_test <- wilcox.test(group1, group2)
        mw_p <- mw_test$p.value
        
        mw_out <- tibble::tibble(gage_ID = s,
                                 metric = var,
                                 mw_yr = mw_yr_split,
                                 mw_p = mw_p,
                                 mw_meanGroup1 = mean(group1, na.rm = T),
                                 mw_meanGroup2 = mean(group2, na.rm = T),
                                 mw_medianGroup1 = median(group1, na.rm = T),
                                 mw_medianGroup2 = median(group2, na.rm = T),
                                 n_yrGroup1 = sum(is.finite(group1)),
                                 n_yrGroup2 = sum(is.finite(group1)))
        
      } else {
        
        mw_out <- tibble::tibble(gage_ID = s,
                                 metric = var,
                                 mw_yr = mw_yr_split,
                                 mw_p = NA,
                                 mw_meanGroup1 = mean(group1, na.rm = T),
                                 mw_meanGroup2 = mean(group2, na.rm = T),
                                 mw_medianGroup1 = median(group1, na.rm = T),
                                 mw_medianGroup2 = median(group2, na.rm = T),
                                 n_yrGroup1 = sum(is.finite(group1)),
                                 n_yrGroup2 = sum(is.finite(group1)))
      }
      
      if (start_flag){
        mw_all <- mw_out
        start_flag <- F
      } else {
        mw_all <- dplyr::bind_rows(mw_all, mw_out)
      }
    }
  }
  
  print(paste0(s, " complete"))
}

## plot
df_mw <- 
  mw_all %>% 
  dplyr::mutate(mw_diff_mean = mw_meanGroup2 - mw_meanGroup1,
                mw_diff_median = mw_medianGroup2 - mw_medianGroup1) %>% 
  subset(complete.cases(.))

# make a column for Mann-Whitney about significance and directio of change
p_thres <- 0.05
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
p_mw_hist_afnf <-
  ggplot() +
  geom_histogram(data = subset(df_mw, metric == "annualnoflowdays"), 
                 aes(x = mw_diff_mean, fill = mw_sig), binwidth = 10) +
  geom_vline(xintercept = 0, color = "#ffffbf") +
  facet_wrap(~mw_yr, ncol = 3) +
  scale_x_continuous(name = "Change in Annual No-Flow Days, (Split+1 to 2017) - (1980 to Split) [days]") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", 
                    values = c("SigDry" = col.cat.red, "SigWet" = col.cat.blu, "NotSig" = col.gray),
                    labels = c("SigDry" = "Drier", "SigWet" = "Wetter", "NotSig" = "No Change"))  +
  theme(panel.border = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = 0.5)) +
  ggsave(file.path("figures_manuscript", "MannWhitney_SplitYearSensitivity-annualnoflowdays.png"),
         width = 190, height = 220, units = "mm")

p_mw_hist_zff <-
  ggplot() +
  geom_histogram(data = subset(df_mw, metric == "zeroflowfirst"), 
                 aes(x = mw_diff_mean, fill = mw_sig), binwidth = 10) +
  geom_vline(xintercept = 0, color = "#ffffbf") +
  facet_wrap(~mw_yr, ncol = 3) +
  scale_x_continuous(name = "Change in First No-Flow Day, (Split+1 to 2017) - (1980 to Split) [days]") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", 
                    values = c("SigDry" = col.cat.red, "SigWet" = col.cat.blu, "NotSig" = col.gray),
                    labels = c("SigDry" = "Drier", "SigWet" = "Wetter", "NotSig" = "No Change"))  +
  theme(panel.border = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = 0.5)) +
  ggsave(file.path("figures_manuscript", "MannWhitney_SplitYearSensitivity-zeroflowfirst.png"),
         width = 190, height = 220, units = "mm")

p_mw_hist_p2z <-
  ggplot() +
  geom_histogram(data = subset(df_mw, metric == "peak2z_length"), 
                 aes(x = mw_diff_mean, fill = mw_sig), binwidth = 10) +
  geom_vline(xintercept = 0, color = "#ffffbf") +
  facet_wrap(~mw_yr, ncol = 3) +
  scale_x_continuous(name = "Change in Days from Peak to No-Flow, (Split+1 to 2017) - (1980 to Split) [days]") +
  scale_y_continuous(name = "Number of Gages") +
  scale_fill_manual(name = "Mann-Whitney Significance", 
                    values = c("SigDry" = col.cat.red, "SigWet" = col.cat.blu, "NotSig" = col.gray),
                    labels = c("SigDry" = "Drier", "SigWet" = "Wetter", "NotSig" = "No Change"))  +
  theme(panel.border = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = 0.5)) +
  ggsave(file.path("figures_manuscript", "MannWhitney_SplitYearSensitivity-peak2z.png"),
         width = 190, height = 220, units = "mm")

## organize data into a table:
# Metric, Split Year, # Sig Wet, # Sig Dry, # Not Sig, Mean Change Sig Wet, Mean Change Sig Dry
mw_summary_table <- 
  df_mw %>% 
  dplyr::group_by(metric, mw_yr) %>% 
  dplyr::summarize(n_SigDry = sum(mw_sig == "SigDry"),
                   n_SigWet = sum(mw_sig == "SigWet"),
                   n_NotSig = sum(mw_sig == "NotSig"),
                   n_tested = n_SigDry + n_SigWet + n_NotSig,
                   prc_SigDry = n_SigDry/540,
                   prc_SigWet = n_SigWet/540,
                   prc_NotSig = n_NotSig/540) %>% 
  dplyr::mutate(SigDryText = paste0("n = ", n_SigDry, " (", round(prc_SigDry*100, 1), "%)"),
                SigWetText = paste0("n = ", n_SigWet, " (", round(prc_SigWet*100, 1), "%)")) %>% 
  dplyr::select(metric, mw_yr, SigDryText, SigWetText)
readr::write_csv(mw_summary_table, file.path("figures_manuscript", "MannWhitney_SplitYearSensitivity.csv"))
