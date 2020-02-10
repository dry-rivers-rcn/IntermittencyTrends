## script used by John to calculate trends

library(Kendall)
library(trend)
library(plyr)
library(lubridate)
library(zyp)
library(stringr)
library(data.table)
library(dplyr)

setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_random_forest_cluster_CART")
#seas_ind <- read.csv("prcp_seas_ind.csv")
data <- read.csv("annual_no_flow_and_climate_metrics_020720_trends.csv")
data <- subset(data, data$currentwyear>1979 & data$currentwyear<2019)
fulllengthwyears <- as.data.frame(1980:2018)
colnames(fulllengthwyears) <- "currentwyear"

sites <- unique(data$gage_ID)

for(i in seq_along(sites)){
  # i = 1
  
  current <- subset(data, data$gage_ID == sites[i])
  
  current[current==-Inf] <- NA
  current[current==Inf] <- NA
  
  results <- data.frame(matrix(NA, nrow = 100, ncol = 3))
  rownames(results) <- colnames(data[2:101])
  colnames(results) <- c("tau","pval","slope")
  
  for(c in 2:101){
    # c = 2
    currentcolumnname <- colnames(current)[c]
    currentcolumn <- as.data.frame(current[,c])
    colnames(currentcolumn) <- "variable"
    currentcolumn$currentwyear <- current$currentwyear
    currentcolumn <- merge(currentcolumn, fulllengthwyears, by = "currentwyear", all.y = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    # 4 years of missing data = ~%10 of data
    
    if(nacounts < 5){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~currentwyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      output <- c(tau,pval,sen$coefficients[2])
      results[(c-1),] <- output
    } 
  }
  results$site <- sites[i]
  setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_random_forest_cluster_CART\\trends_020720")
  write.csv(results, paste(sites[i],".trend.summary.csv",sep = ""))
}

setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_random_forest_cluster_CART\\trends_020720")

files <- list.files(pattern = ".csv")
alltrends <- do.call(rbind, lapply(files, read.csv))
write.csv(alltrends, "trends_in_no_flow_and_climate_for_no_flow_sites_34_plus_years_020720.csv")
