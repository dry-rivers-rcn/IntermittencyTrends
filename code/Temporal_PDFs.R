## Temporal_PDFs.R

## This script is meant to create a visualization of how the distribution of different no flow metrics
## change through time

## Metrics of interest: annual fraction no flow; date of 1st no flow;
## frequency; Peak flow to zero flow

lapply(c("plyr","dplyr","ggplot2","cowplot","ggridges",
         "lubridate","tidyverse", "viridis","data.table",
         "hrbrthemes","gclus"), require, character.only=T)

## Import Data
setwd("../results")
gsm <- fread("00_SelectGagesForAnalysis_GageSampleMean.csv")
gsa <- fread("00_SelectGagesForAnalysis_GageSampleAnnual.csv")


# Plot pdfs by current water year by region
temp_pdf_region <- function(region){
  reg <- gsa[which(gsa$region == region),] 
  
  ggplot(reg, aes(x = annualfractionnoflow, y = as.factor(currentwyear),fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height=0.01) +
  scale_fill_viridis(name = "Frac No Flow", option = "C") +
  labs(x="Annual Fraction of No Flow", y="Current Water Year")+
  ggtitle(region)+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
}

levels(factor(gsa$region)) ## Region options
temp_pdf_region("North Great Plains")
temp_pdf_region("Mediterranean California")


# Plot histograms by current water year by region
temp_hist_region <- function(region){
  ggplot(gsa[which(gsa$region == region),],
         aes(x = annualfractionnoflow,
             y = as.factor(gsa[which(gsa$region == region),]$currentwyear),
             fill = ..x..)) +
    stat_binline(bins = 30, draw_baseline = FALSE) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_viridis(name = "Frac No Flow", option = "C") +
    coord_cartesian(clip = "off") +
    labs(x="Annual Fraction of No Flow", y="Current Water Year")+
    ggtitle(region)+
    theme_ridges() +
    theme(legend.position = 'none')
}

levels(factor(gsa$region)) ## Region options
temp_hist_region("North Great Plains") ## More difficult to visualize




