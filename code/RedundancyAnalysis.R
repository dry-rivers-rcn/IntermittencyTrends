## RedundancyAnalysis.R
# This script is intended to create a list of gages to eliminate based on redundancy 
# (i.e., gages that are downstream of another gage in the analysis)
# It is modified from a script sent from John Hammond to SCZ via email on 3/17/2021

source(file.path("code", "paths+packages.R"))

library(sf)
library(otuSummary)

# read in shapefiles for watersheds, gages, and find watershed centroids
watersheds <- st_read(file.path(dir_data, "redundancy_analysis", "final_study_watersheds.shp"))
gages <- st_read(file.path(dir_data, "redundancy_analysis", "final_study_gages.shp"))
centroids <- st_centroid(watersheds)

# calculate redundancy metrics (standardized distance and drainage area ratio) 
# For more info, see https://pubs.usgs.gov/sir/2016/5081/sir20165081.pdf

# first, find distances between each pair of watershed centroids
centroid_distances <- as.data.frame(st_distance(centroids))
colnames(centroid_distances) <- centroids$GAGE_ID
row.names(centroid_distances) <- centroids$GAGE_ID

centroid_distances_long <- matrixConvert(centroid_distances, colname = c("gage1", "gage2", "dist_meter"))
centroid_distances_long$dist_miles <- centroid_distances_long$dist_meter*0.000621371

# make sure all units in miles,square miles
watersheds$area_sqmiles <- watersheds$area_km2*0.386102
watershed_areas <- watersheds[,c("gage_num","area_sqmiles")] 
st_geometry(watershed_areas) <- NULL

# match areas with distances
centroid_distances_long <- merge(centroid_distances_long, watershed_areas, by.x = "gage1", by.y = "gage_num")
centroid_distances_long <- merge(centroid_distances_long, watershed_areas, by.x = "gage2", by.y = "gage_num")

colnames(centroid_distances_long) <- c("gage2","gage1","dist_meter","dist_miles","area_sqmiles_1","area_sqmiles_2")

# calculate standardized distance and drainage area ratio
centroid_distances_long <- 
  centroid_distances_long %>% 
  rowwise() %>% 
  mutate(DAR = max(c(area_sqmiles_1/area_sqmiles_2, area_sqmiles_2/area_sqmiles_1)), 
         SD = dist_miles / sqrt(0.5*(area_sqmiles_1+area_sqmiles_2)))

# identify redundancy based on peak flow thresholds SD less than or equal to 0.50 and DAR less than or equal to 5 (Gruber and Stedinger, 2008) 
redundant_in <- filter(centroid_distances_long, SD <= 0.50 & DAR <= 5)
# 95 potential redundancies identified (affecting 120 unique gages)

# count how many times each gage is included
all_gages <- 
  c(redundant_in$gage2, redundant_in$gage1)

all_gages_with_areas <- 
  tibble::tibble(gage_num = all_gages) %>% 
  dplyr::group_by(gage_num) %>% 
  dplyr::summarize(count = n()) %>% 
  dplyr::left_join(watershed_areas, by = c("gage_num")) %>% 
  dplyr::arrange(-count, -area_sqmiles)

# create a while loop to drop gages until all are eliminated
redundant_out <- redundant_in
redundant_gages <- c()
while (dim(redundant_out)[1] > 0){
  remaining_gages_with_areas <- 
    c(redundant_out$gage2, redundant_out$gage1) %>% 
    tibble::tibble(gage_num = .) %>% 
    dplyr::group_by(gage_num) %>% 
    dplyr::summarize(count = n()) %>% 
    dplyr::left_join(watershed_areas, by = c("gage_num")) %>% 
    dplyr::arrange(-count, -area_sqmiles)
  
  # gage to drop
  gid <- remaining_gages_with_areas$gage_num[1]
  redundant_gages <- c(redundant_gages, gid)
  
  # subset data
  redundant_out <- subset(redundant_out, gage1 != gid & gage2 != gid)
  
  # status update
  print(paste0(length(redundant_gages), " gages dropped, ", dim(redundant_out)[1], " redundancies remain"))
  
}

# gages to drop
tibble(gage_ID = redundant_gages,
       redundant = T) %>% 
  readr::write_csv(file.path("results", "RedundancyAnalysis_RedundantGages.csv"))
