## K-means clustering of time series

## This script is an attempt to cluster time series
## Following Savoy et al. 2019 L&O, there are two potential approaches:
## 1) Dynamic time warping and hierarchical partitional clustering
## 2) K-means partitional clustering on Euclidean distance matrix among time series

## I am going to start by attempting #2
## This approach has also been detailed in Olden et al. 2012 Ecohydrology

## Depending on the variable, some need to be z-normalized and others don't (e.g. if between 0 to 1, they don't)
## Next, compute a Euclidean distance matrix (package 'TSdist' - Dissim distance)
## Then, define the number of clusters
## Compute cluster validity indices (CVIs) (package 'NbClust')

lapply(c("plyr","dplyr","ggplot2","cowplot","ggridges",
         "lubridate","tidyverse", "viridis","data.table",
         "NbClust", "TSdist", "TSclust","proxy", "reshape2"), require, character.only=T)


## Import Data
setwd("../results")
gsm <- fread("00_SelectGagesForAnalysis_GageSampleMean.csv")
gsa <- fread("00_SelectGagesForAnalysis_GageSampleAnnual.csv")

## Subset gsa time series
names(gsa)
dat <- gsa[,c("gage_ID","currentwyear","annualfractionnoflow",
              "firstnoflowcaly","peak2z_length")]
l <- split(dat, dat$gage_ID)

## z-normalize function for time series of variables that are not on the same scale
znorm <- function(ts){
  ts.mean <- mean(ts)
  ts.dev <- sd(ts)
  (ts - ts.mean)/ts.dev
}

#############################
## Annual fraction no flow
#############################

## Calculate Euclidean distance matrix
af <- dat[,c("gage_ID","currentwyear","annualfractionnoflow")]
af <- dcast(af, gage_ID ~ currentwyear, value.var = "annualfractionnoflow")

## for now set NAs to zero
af[is.na(af)] <- 0

## turn data frame into matrix
afm <- data.matrix(af[1:30,])

## calculate distance matrix
distance_matrix <- proxy::dist(afm, method = "euclidean", upper = TRUE, diag = TRUE)
print(distance_matrix)
## not working



## Next work session use examples from here:

## https://rdrr.io/cran/dtwclust/man/tsclust.html



data(uciCT)




