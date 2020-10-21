## CompareRoundingMetricsTrends.R
# This script is intended to compare no-flow metrics with and without rounding.

source(file.path("code", "paths+packages.R"))

## save output
df_all <- readr::read_csv(file = file.path("results", "000_CalculateMetrics_AnnualHydroMetrics.csv"))
df_all_rounded <- readr::read_csv(file = file.path("results", "000_CalculateMetrics_AnnualHydroMetrics_Rounded.csv"))
df_trends_all <- readr::read_csv(file = file.path("results", "000_CalculateMetrics_HydroMetricsTrends.csv"))

## join annual metrics for plotting
df_metrics <- dplyr::left_join(df_all, df_all_rounded, by = c("currentclimyear", "gage_ID"), suffix = c("", "_rounded"))

p_afnf <- 
  ggplot(df_metrics, aes(x = annualfractionnoflow, y = annualfractionnoflow_rounded)) +
  geom_point(shape = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red")

p_zff <-
  ggplot(df_metrics, aes(x = zeroflowfirst, y = zeroflowfirst_rounded)) +
  geom_point(shape = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red")

p_afnf_trend <-
  ggplot(df_trends_all, aes(x = tau_annualfractionnoflow, y = tau_annualfractionnoflow_rounded)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(shape = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red")

p_zff_trend <-
  ggplot(df_trends_all, aes(x = tau_zeroflowfirst, y = tau_zeroflowfirst_rounded)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(shape = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red")

(p_afnf + p_zff + 
    p_afnf_trend + p_zff_trend) +
  ggsave(file.path("results", "CompareRoundingMetricsTrends.png"),
         width = 8, height = 8, units = "in")

## calculate how many gages meet criteria (average >5 days and < 360 days no-flow)
df_bygage <- 
  df_metrics %>% 
  dplyr::group_by(gage_ID) %>% 
  dplyr::summarize(afnf = mean(annualfractionnoflow),
                   afnf_rounded = mean(annualfractionnoflow_rounded),
                   n_yrs = sum(is.finite(annualfractionnoflow)))

thres_min <- 5/365
thres_max <- 360/365
thres_yrs <- 30

sum(df_bygage$afnf >= thres_min & df_bygage$afnf <= thres_max & df_bygage$n_yrs >= thres_yrs, na.rm = T)
sum(df_bygage$afnf_rounded >= thres_min & df_bygage$afnf_rounded <= thres_max & df_bygage$n_yrs >= thres_yrs, na.rm = T)
