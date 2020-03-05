## TrendPatterns.R

# This script does a general analysis of the trends based on gage descriptors. Mean annual climate, hydrological, physiographic, land use, water use, ref/nonref, etc.

## Metrics of interest: precip, annual fraction no flow; zero flow centroid date; total no flow periods

# Seems like one valuable figure would be a boxplot of tau of these metrics by region

source(file.path("code", "paths+packages.R"))

## --- load trends data
gages_trends <- 
  file.path(dir_data, 
            "results/00_SelectGagesForAnalysis_GageSampleTrends.csv") %>% 
  readr::read_csv()

## --- use dplyr to organize and filter data, alternative is to transform to make the metrics their own columns
summary_trends <- gages_trends %>% group_by(metric,region) %>% filter(pval < 0.05) %>% summarise(avgTau = mean(tau), avgP = mean(pval))

## -- This significantly decreases the number of values, but might be valuable in assessing relationships, if we use this then need to ID tau's that have sig P values in both metrics of interest (e.g. 70 sig precip trends, and associated values of no flow metric)
sig_trends <- gages_trends %>% filter(pval < 0.05) 

# --- Annual Fraction of No Flow v Precip --- #

reg1 <- lm(gages_trends$tau[gages_trends$metric == "annualfractionnoflow"] ~gages_trends$tau[gages_trends$metric == "p_mm_wy"])
summary(reg1)

plot(gages_trends$tau[gages_trends$metric == "p_mm_wy"], gages_trends$tau[gages_trends$metric == "annualfractionnoflow"], pch = 20, xlab='Precipitation Trend Over Time', ylab= 'Annual Fraction of No Flow Trend')
abline(reg1) #p-value is significant, but r-squared is only 0.19 - should add this value to the figure or to a summary table?


# --- Zero Flow Centroid Dates v Precip --- #
reg2 <- lm(gages_trends$tau[gages_trends$metric == "zeroflowcentroiddate"] ~gages_trends$tau[gages_trends$metric == "p_mm_wy"])
summary(reg2)

plot(gages_trends$tau[gages_trends$metric == "p_mm_wy"], gages_trends$tau[gages_trends$metric == "zeroflowcentroiddate"], pch = 20, xlab='Precipitation Trend Over Time', ylab= 'Zero Flow Centroid Date')
abline(reg2) 

# --- Total No Flow Periods v Precip --- #
reg3 <- lm(gages_trends$tau[gages_trends$metric == "totalnoflowperiods"] ~ gages_trends$tau[gages_trends$metric == "p_mm_wy"])
summary(reg3)

plot(gages_trends$tau[gages_trends$metric == "p_mm_wy"], gages_trends$tau[gages_trends$metric == "totalnoflowperiods"], pch = 20, xlab='Precipitation Trend Over Time', ylab= 'Total No Flow Periods')
abline(reg3) 

# --- Boxplots of Tau by Region and Metric of interest --- #




