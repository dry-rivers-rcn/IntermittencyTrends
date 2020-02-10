## TrendPatterns.R

# This script does a general analysis of the trends based on gage descriptors. 
# Mean annual climate, hydrological, physiographic, land use, water use, ref/nonref, etc.

source(file.path("code", "paths+packages.R"))

## load trends data
gages_trends <- 
  file.path(dir_data, 
            "results/00_SelectGagesForAnalysis_GageSampleTrends.csv") %>% 
  readr::read_csv()

## Metrics of interest: precip, annual fraction no flow; zero flow centroid date; total no flow periods
#this ggplot doesn't work yet ...
#ggplot(gages_trends, aes(x = tau[gages_trends$metric == "p_mm_wy"], y = tau[gages_trends$metric == "annualfractionnoflow"]))

plot(gages_trends$tau[gages_trends$metric == "p_mm_wy"], gages_trends$tau[gages_trends$metric == "annualfractionnoflow"])
