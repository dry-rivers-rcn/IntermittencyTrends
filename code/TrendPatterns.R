## TrendPatterns.R

# This script does a general analysis of the trends based on gage descriptors. Mean annual climate, hydrological, physiographic, land use, water use, ref/nonref, etc.

## Metrics of interest: precip, annual fraction no flow; zero flow centroid date; total no flow periods


source(file.path("code", "paths+packages.R"))
library(gclus)

# --- load trends data ----
gage_trends <- 
  file.path(dir_data, 
            "results/00_SelectGagesForAnalysis_GageSampleTrends.csv") %>% 
  readr::read_csv()

gage_charc <- 
  file.path(dir_data, 
            "results/00_SelectGagesForAnalysis_GageSampleMean.csv") %>% 
  readr::read_csv()

# --- join data tables ----

gages <- left_join(gage_trends, gage_charc, by = 'gage_ID')
gages_sig <- gages  %>% filter(pval < 0.05)

# --- use dplyr to organize and filter data, alternative is to transform to make the metrics their own columns ----
summary_trends <- gage_trends %>% group_by(metric,region) %>% filter(pval < 0.05) %>% summarise(avgTau = mean(tau), avgP = mean(pval))

# --- Subset to significant trends ----
sig_trends <- gage_trends %>% filter(pval < 0.05) 


# ---- Initial Data exploration, prob not useful ----
# --- Annual Fraction of No Flow Tau v Precip Tau --- #
# I don't think this actually means anything re: not actually sure what tau is ...
reg1 <- lm(gage_trends$tau[gage_trends$metric == "annualfractionnoflow"] ~gage_trends$tau[gage_trends$metric == "p_mm_wy"])
summary(reg1)

plot(gage_trends$tau[gage_trends$metric == "p_mm_wy"], gage_trends$tau[gage_trends$metric == "annualfractionnoflow"], pch = 20, xlab='Precipitation Trend Over Time', ylab= 'Annual Fraction of No Flow Trend')
abline(reg1) #p-value is significant, but r-squared is only 0.19 - should add this value to the figure or to a summary table?

# --- Zero Flow Centroid Dates v Precip --- not significant#

#--- Total No Flow Periods Trend v Precip Trend --- #
reg3 <- lm(gage_trends$tau[gage_trends$metric == "totalnoflowperiods"] ~ gage_trends$tau[gage_trends$metric == "p_mm_wy"])
summary(reg3)

plot(gage_trends$tau[gage_trends$metric == "p_mm_wy"], gage_trends$tau[gage_trends$metric == "totalnoflowperiods"], pch = 20, xlab='Precipitation Trend Over Time', ylab= 'Total No Flow Periods')
abline(reg3) #significant and r2 = .13

# --- Boxplots of Tau by Region and Metric of interest --- #
# might not be useful by regiona since there doesnt not appear to be a regional grouping, but perhaps by other gage characteristics
 




# --- subset data for plotting and regressions ---- 
#%>% filter(CLASS =='Ref')
annfracnoflow <- gages_sig %>% filter(metric == "annualfractionnoflow") %>% select(slope, p.pet, tmean_c, swe.p, pdsi_wy, DRAIN_SQKM, FORESTNLCD06, PLANTNLCD06, IMPNLCD06, ELEV_MEAN_M_BASIN, SLOPE_PCT, PRECIP_SEAS_IND, depth_bedrock_m, porosity, storage_m, T_max_c_wy, PCT_IRRIG_AG, SNOW_PCT_PRECIP)
zeroflowcentroid  <- gages_sig %>% filter(metric == "zeroflowcentroiddate") %>% select(slope, p.pet, tmean_c, swe.p, pdsi_wy, DRAIN_SQKM, FORESTNLCD06, PLANTNLCD06, IMPNLCD06, ELEV_MEAN_M_BASIN, SLOPE_PCT, PRECIP_SEAS_IND, depth_bedrock_m, porosity, storage_m, T_max_c_wy, PCT_IRRIG_AG, SNOW_PCT_PRECIP)
totnoflowper <- gages_sig %>% filter(metric == "totalnoflowperiods") %>% select(slope, p.pet, tmean_c, swe.p, pdsi_wy, DRAIN_SQKM, FORESTNLCD06, PLANTNLCD06, IMPNLCD06, ELEV_MEAN_M_BASIN, SLOPE_PCT, PRECIP_SEAS_IND, depth_bedrock_m, porosity, storage_m, T_max_c_wy, PCT_IRRIG_AG, SNOW_PCT_PRECIP)

#--- Pearson r linear correlations between trend in flow metrics versus average watershed characteristics ----

source("code/panelutils.R") 

annfract_cor <- cor(annfracnoflow)#, method = "kendall") 
annfrac_o <- order.single(annfract_cor)
quartz(title ='', 12, 12)
op <- par(mfrow=c(1,1), pty ='s')
pairs(annfracnoflow[,annfrac_o], lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main= "Linear correlation btw Annual Fraction No Flow Trends and Gage Characteristics")
par(op)

quartz.save("results/AnnFrac_cor.pdf", type="pdf")

# total no flow periods
totnoflowper_cor <- cor(totnoflowper) 
totnoflowper_o <- order.single(totnoflowper_cor)
quartz(title ='', 12, 12)
op <- par(mfrow=c(1,1), pty ='s')
pairs(totnoflowper[,totnoflowper_o], lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main= "Linear correlation btw Total No Flow Period Trends and Gage Characteristics")
par(op)

quartz.save("results/totnoflowper_cor.pdf", type="pdf")

# centroid date
zeroflowcentroid_cor <- cor(zeroflowcentroid) 
zeroflowcentroid_o <- order.single(zeroflowcentroid_cor)
quartz(title ='', 12, 12)
op <- par(mfrow=c(1,1), pty ='s')
pairs(zeroflowcentroid[,annfrac_o], lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main= "Linear correlation btw Zero Flow Centroid Date Trends and Gage Characteristics")
par(op)

quartz.save("results/ZeroFlowCentroid_cor.pdf", type="pdf")


# --- combine correlations into one datatable for export ----
cors<- data.frame(annfracnoflow= annfract_cor[,1], totnoflowper= totnoflowper_cor[,1], zerocentroid= zeroflowcentroid_cor[,1])

write.csv(cors, "results/Metric Trend Correlation of All Gages to Static Variables.csv")
