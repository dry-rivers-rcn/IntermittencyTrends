## TrendPatterns.R

# This script does a general analysis of the trends based on gage descriptors. Mean annual climate, hydrological, physiographic, land use, water use, ref/nonref, etc.

## Metrics of interest: precip, annual fraction no flow; zero flow centroid date; total no flow periods


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

# --- Boxplots of Slope by Region and Metric of interest --- #

gages<-as.data.frame(gages)
afnf_sig<-gages_sig[gages_sig$metric == "annualfractionnoflow",] #this is 174 observations
afnf_sig$region.x<- factor(afnf_sig$region.x, levels=c("Eastern Temperate", "North Great Plains", "South Great Plains",  "Western Mountains", "Western Desert", "Mediterranean California"))

p_sig<-gages_sig[gages_sig$metric == "p_mm_wy",] # this only results in 71 locations
p_sig$region.x<- factor(p_sig$region.x, levels=c("Eastern Temperate", "North Great Plains", "South Great Plains",  "Western Mountains", "Western Desert", "Mediterranean California"))

T_max_sig<-gages_sig[gages_sig$metric == "T_max_c_wy",] # this only results in 97 locations
T_max_sig$region.x<- factor(T_max_sig$region.x, levels=c("Eastern Temperate", "North Great Plains", "South Great Plains",  "Western Mountains", "Western Desert", "Mediterranean California"))

pet_sig<-gages_sig[gages_sig$metric == "pet_mm_wy",]
pet_sig$region.x<- factor(pet_sig$region.x, levels=c("Eastern Temperate", "North Great Plains", "South Great Plains",  "Western Mountains", "Western Desert", "Mediterranean California"))

af<- ggplot(afnf_sig, aes(x=fct_reorder(region.x, slope), y= slope, fill =region.x)) +
  geom_boxplot() + 
  geom_hline(yintercept=0) +
  geom_jitter(width=0.05,alpha=0.2)+
  theme_bw()+
  xlab('')+
  ylab('Slopes of Annual Frac No Flow (n =174)')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

p<- ggplot(p_sig, aes(x=region.x, y= slope, fill =region.x)) +
  geom_boxplot() + 
  theme_bw()+
  geom_hline(yintercept=0) +
  geom_jitter(width=0.05,alpha=0.2)+
  xlab('')+
  ylab('Slopes of Annual Precip (n=71)')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

t<- ggplot(T_max_sig, aes(x=region.x, y= slope, fill =region.x)) +
  geom_boxplot() + 
  theme_bw()+
  geom_hline(yintercept=0) +
  geom_jitter(width=0.05,alpha=0.2)+
  xlab('')+
  ylab('Slopes of Annual Temperature (n=97)')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

pet<- ggplot(pet_sig, aes(x=region.x, y= slope, fill =region.x)) + 
  geom_boxplot() + 
  theme_bw()+
  geom_jitter(width=0.05,alpha=0.2)+
  xlab('Region')+
  ylab('Slopes of PET (n= 212) ')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))
  
  #theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30)) +
  #guides(fill = guide_legend(nrow = 3), byrow=TRUE)

  grid.arrange(af, p, t, pet, ncol=1)




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
