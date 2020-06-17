## TrendPatterns.R
# Kendra Kaiser Sprin 2020
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

afnf<-gages[gages$metric == "annualfractionnoflow",] 
afnf$region.x<- factor(afnf$region.x, levels=regions)
afnf_sig<-gages_sig[gages_sig$metric == "annualfractionnoflow",] 
afnf_sig$region.x<- factor(afnf_sig$region.x, levels=regions)

fnf<-gages[gages$metric == "zeroflowfirst",] 
fnf$region.x<- factor(fnf$region.x, levels=regions)
fnf_sig<-gages_sig[gages_sig$metric == "zeroflowfirst",] 
fnf_sig$region.x<- factor(fnf_sig$region.x, levels=regions)

p2l<-gages[gages$metric == "peak2z_length",] 
p2l$region.x<- factor(p2l$region.x, levels=regions)
p2l_sig<-gages_sig[gages_sig$metric == "peak2z_length",] 
p2l_sig$region.x<- factor(p2l_sig$region.x, levels=regions)

p<-gages[gages$metric == "p_mm_cy",]
p$region.x<- factor(p$region.x, levels=regions)
p_sig<-gages_sig[gages_sig$metric == "p_mm_cy",] 
p_sig$region.x<- factor(p_sig$region.x, levels=regions)

T_max<-gages[gages$metric == "T_max_c_cy",] 
T_max$region.x<- factor(T_max$region.x, levels=regions)
T_max_sig<-gages_sig[gages_sig$metric == "T_max_c_cy",] 
T_max_sig$region.x<- factor(T_max_sig$region.x, levels=regions)

pet<-gages[gages$metric == "pet_mm_cy",]
pet$region.x<- factor(pet$region.x, levels=regions)
pet_sig<-gages_sig[gages_sig$metric == "pet_mm_cy",]
pet_sig$region.x<- factor(pet_sig$region.x, levels=regions)

af<- ggplot(afnf, aes(x=fct_reorder(region.x, slope), y= slope, fill = region.x, color =region.x)) +
  geom_boxplot() + 
  geom_hline(yintercept=0) +
  scale_fill_manual(values = pal_regions) +
  scale_color_manual(values = pal_regions_dk) +
  #geom_jitter(width=0.05,alpha=0.2)+
  theme_bw()+
  xlab('')+
  ylab('Annual Frac No Flow')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

fn<- ggplot(fnf, aes(x=fct_reorder(region.x, slope), y= slope, fill = region.x, color =region.x)) + 
  geom_boxplot() + 
  scale_fill_manual(values = pal_regions) +
  scale_color_manual(values = pal_regions_dk) +
  geom_hline(yintercept=0) +
  #geom_jitter(width=0.05,alpha=0.2)+
  theme_bw()+
  xlab('')+
  ylab('First No Flow')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

peak<- ggplot(p2l, aes(x=fct_reorder(region.x, slope), y= slope, fill = region.x, color =region.x)) + 
  geom_boxplot() + 
  scale_fill_manual(values = pal_regions) +
  scale_color_manual(values = pal_regions_dk) +
  geom_hline(yintercept=0) +
  #geom_jitter(width=0.05,alpha=0.2)+
  theme_bw()+
  xlab('')+
  ylab('Peak to Zero (rate)')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

p<- ggplot(p, aes(x=region.x, y= slope, fill = region.x, color =region.x)) +
  geom_boxplot() +
  scale_fill_manual(values = pal_regions) +
  scale_color_manual(values = pal_regions_dk) +
  theme_bw()+
  geom_hline(yintercept=0) +
  geom_jitter(width=0.05,alpha=0.2)+
  xlab('')+
  ylab('Annual Precip')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

t<- ggplot(T_max, aes(x=region.x, y= slope, fill = region.x, color =region.x)) +
  geom_boxplot() +
  scale_fill_manual(values = pal_regions) +
  scale_color_manual(values = pal_regions_dk) +
  theme_bw()+
  geom_hline(yintercept=0) +
  geom_jitter(width=0.05,alpha=0.2)+
  xlab('')+
  ylab('Annual Temp')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

pet<- ggplot(pet, aes(x=region.x, y= slope, fill = region.x, color =region.x)) + 
  geom_boxplot() + 
  scale_fill_manual(values = pal_regions) +
  scale_color_manual(values = pal_regions_dk) +
  theme_bw()+
  geom_jitter(width=0.05,alpha=0.2)+
  xlab('')+
  ylab('PET')+
  theme(legend.position='none', axis.text.x = element_text(angle = 30))

  grid.arrange(af, p, fn, t, peak, pet, ncol=2)
  grid.arrange(af, fn, peak, ncol=1)
  grid.arrange(p, t, pet, ncol=1)

