## TrendPatterns.R

# This script does a general analysis of the trends based on gage descriptors. Mean annual climate, hydrological, physiographic, land use, water use, ref/nonref, etc.

## Metrics of interest: precip, annualfractionnoflow, firstnoflowcaly, and peak2z_length

source(file.path("code", "paths+packages.R"))
library(gclus)
library(ggplot2)
library(gridExtra)

# --- load trends data ----
gage_trends <- 
  file.path(dir_data, 
            "results/00_SelectGagesForAnalysis_GageSampleTrends.csv") %>% 
  readr::read_csv()

gage_charc <- 
  file.path(dir_data, 
            "results/00_SelectGagesForAnalysis_GageSampleMean.csv") %>% 
  readr::read_csv()

gage_region <- 
  file.path(dir_data, 
            "results/00_SelectGagesForAnalysis_GageRegions.csv") %>% 
  readr::read_csv()

# --- join data tables ----

gagesc <- left_join(gage_region, gage_charc, by = 'gage_ID')
gages <- left_join(gage_trends, gagesc, by = 'gage_ID')
gages_sig <- gages  %>% filter(pval < 0.05)

# --- use dplyr to organize and filter data, alternative is to transform to make the metrics their own columns ----
summary_trends <- gages_sig %>% group_by(metric, region.x) %>% filter(pval < 0.05) %>% summarise(avgTau = mean(tau), avgP = mean(pval))

# --- Boxplots of Slope by Region and Metric of interest --- #
# --- Seems like there should be a cleaner way to code this - would love input #

regions<-unique(gages$region.x)

gages<-as.data.frame(gages)
afnf_sig<-gages_sig[gages_sig$metric == "annualfractionnoflow",] 
afnf_sig$region.x<- factor(afnf_sig$region.x, levels=regions)

fnf_sig<-gages_sig[gages_sig$metric == "firstnoflowcaly",] 
fnf_sig$region.x<- factor(fnf_sig$region.x, levels=regions)

p2l_sig<-gages_sig[gages_sig$metric == "peak2z_length",] 
p2l_sig$region.x<- factor(p2l_sig$region.x, levels=regions)

p_sig<-gages_sig[gages_sig$metric == "p_mm_wy",] 
p_sig$region.x<- factor(p_sig$region.x, levels=regions)

T_max_sig<-gages_sig[gages_sig$metric == "T_max_c_wy",] 
T_max_sig$region.x<- factor(T_max_sig$region.x, levels=regions)

pet_sig<-gages_sig[gages_sig$metric == "pet_mm_wy",]
pet_sig$region.x<- factor(pet_sig$region.x, levels=regions)

af<- ggplot(afnf_sig, aes(x=fct_reorder(region.x, slope), y= slope, fill =region.x)) +
  geom_boxplot() + 
  geom_hline(yintercept=0) +
  geom_jitter(width=0.05,alpha=0.2)+
  theme_bw()+
  xlab('')+
  ylab('Annual Frac No Flow (n=237)')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

fnf<- ggplot(fnf_sig, aes(x=fct_reorder(region.x, slope), y= slope, fill =region.x)) +
  geom_boxplot() + 
  geom_hline(yintercept=0) +
  geom_jitter(width=0.05,alpha=0.2)+
  theme_bw()+
  xlab('')+
  ylab('First No Flow (n=21)')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

peak<- ggplot(p2l_sig, aes(x=fct_reorder(region.x, slope), y= slope, fill =region.x)) +
  geom_boxplot() + 
  geom_hline(yintercept=0) +
  geom_jitter(width=0.05,alpha=0.2)+
  theme_bw()+
  xlab('')+
  ylab('Peak to (n=33)')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

p<- ggplot(p_sig, aes(x=region.x, y= slope, fill =region.x)) +
  geom_boxplot() + 
  theme_bw()+
  geom_hline(yintercept=0) +
  geom_jitter(width=0.05,alpha=0.2)+
  xlab('')+
  ylab('Annual Precip (n=107)')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

t<- ggplot(T_max_sig, aes(x=region.x, y= slope, fill =region.x)) +
  geom_boxplot() + 
  theme_bw()+
  geom_hline(yintercept=0) +
  geom_jitter(width=0.05,alpha=0.2)+
  xlab('')+
  ylab('Annual Temp (n=138)')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

pet<- ggplot(pet_sig, aes(x=region.x, y= slope, fill =region.x)) + 
  geom_boxplot() + 
  theme_bw()+
  geom_jitter(width=0.05,alpha=0.2)+
  xlab('')+
  ylab('PET (n= 310)')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

  grid.arrange(af, p, fnf, t, peak, pet, ncol=2)


# --- subset data for plotting and regressions ---- 

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
