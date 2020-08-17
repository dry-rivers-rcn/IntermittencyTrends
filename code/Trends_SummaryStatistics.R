## Trends_SummaryStatistics.R
#' This script load the trend results and calculates some simple 
#' summary statistics for the trends.
#' 

source(file.path("code", "paths+packages.R"))

## metrics we care about
metrics <- c("annualflowdays", "zeroflowfirst", "peak2z_length")

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

## plot: mann-kendall results
# load state map
states <- map_data("state")

df_mk <- 
  gage_trends %>% 
  dplyr::select(metric, gage_ID, region, dec_lat_va, dec_long_va, mk_tau, mk_p) %>% 
  subset(complete.cases(.))

ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = col.gray) +
  geom_point(data = df_mk, aes(x = dec_long_va, y = dec_lat_va, color = mk_tau)) +
  facet_wrap(~metric, ncol = 1) +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  scale_color_gradient2()


## summarize
p_thres <- 0.05
gage_trends %>% 
  dplyr::group_by(metric) %>% 
  dplyr::summarize(n_finite = sum(is.finite(slope)),
                   n_sig = sum(pval < p_thres, na.rm = T),
                   n_sig_pos = sum(slope > 0 & pval < p_thres, na.rm = T),
                   n_sig_neg = sum(slope <= 0 & pval < p_thres, na.rm = T))

207/512
34/150
23/123



## reproducible example - exploring mann-kendal and sen's slope
# sample data
df <- 
  tibble::tibble(year = seq(1980, 2017),
                 value = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0.071232877, 0.082191781, 0.008219178, 
                           0, 0.008219178, 0, 0.273972603, 0.270491803, 0.01369863, 0, 0, 
                           0.092896175, 0.090410959, 0, 0, 0, 0, 0.024657534))


# make sample data with linear increase
year <- seq(1, 51)
value <- seq(0, 500, 10)

# replace >50% of values with 0s
value[seq(1,51,2)] <- 0

# plot
plot(year, value)

# calculate mann-kendall tau and theil-sen slope
manken <- rkt::rkt(year, value)
manken$tau  # Kendall tau = 0.235
manken$sl   # p-value = 0.009
manken$B    # Theil-Sen estimator = 0

# test linear slope
linfit <- lm(value ~ year)
summary(linfit)

## now, set >50% of values to zero
df$value_with_0s <- df$value
df$value_with_0s[seq(1,51,2)] <- 0

# plot
plot(df$year, df$value_with_0s)

# calculate mann-kendall tau and theil-sen slope
manken_with_0s <- rkt::rkt(df$year, df$value_with_0s)
manken_with_0s$tau  # Kendall tau = 0.23
manken_with_0s$sl   # p-value = 0.009 (significant)
manken_with_0s$B    # Theil-Sen slope = 0 becuase > 50% of points are the same
