## paths+packages.R
# a place to keep variables, functions, etc. relevant across numerous scripts

# load packages
library(tidyverse)
library(lwgeom)
library(cowplot)
library(patchwork)
options(dplyr.summarise.inform=F)   # suppress summarize info

# relative path to directory containing John's data analysis 
# GitHub repository with scripts and data
dir_DataAnalysis <- file.path("..", "DataAnalysis")

# OneDrive directory with bigger data files
dir_data <- file.path("C:/Users/samzipper/OneDrive - The University of Kansas/Research/DryRiversRCN/data")

# OneDrive directory with non-project-specific GIS files
dir_gis <- file.path("C:/Users/samzipper/OneDrive - The University of Kansas/GIS_GeneralFiles")

## common CRS for projecting files
proj_crs <- structure(list(epsg = NA_integer_, proj4string = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"), class = "crs")

## color palettes
# categorical color palette from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

pal_regions <- 
  c("Eastern Forests" = "#009E73",
    "Mediterranean California" = "#F0E442",
    "North Great Plains" = "#0072B2",
    "South Great Plains" = "#E69F00",
    "Western Desert" = "#D55E00",
    "Western Mountains" = "#56B4E9")

pal_regions_dk <- 
  c("Eastern Forests" = "#007756",
    "Mediterranean California" = "#c1b40f",
    "North Great Plains" = "#004064",
    "South Great Plains" = "#bf8400",
    "Western Desert" = "#9a4400",
    "Western Mountains" = "#1986c3")

lab_regions_2line <- 
  c("Eastern Forests" = "Eastern\nForests",
    "Mediterranean California" = "Mediterranean\nCalifornia",
    "North Great Plains" = "North Great\nPlains",
    "South Great Plains" = "South Great\nPlains",
    "Western Desert" = "Western\nDesert",
    "Western Mountains" = "Western\nMountains")

lab_regions_skinny <- 
  c("Eastern Forests" = "East.\nForests",
    "Mediterranean California" = "Med.\nCali.",
    "North Great Plains" = "North\nGreat\nPlains",
    "South Great Plains" = "South\nGreat\nPlains",
    "Western Desert" = "West.\nDesert",
    "Western Mountains" = "West.\nMount.")

lab_regions_skinny1line <- 
  c("Eastern Forests" = "East. Forests",
    "Mediterranean California" = "Med. Cali.",
    "North Great Plains" = "N Great Plains",
    "South Great Plains" = "S Great Plains",
    "Western Desert" = "West Desert",
    "Western Mountains" = "West. Mount.")

## data frame with long names for predictors and their category
df_pred <-
  tibble::tibble(
    predictor = c("clayave", "drain_sqkm", "elev_mean_m_basin", "p.pet_amj", 
                  "p.pet_cy", "p.pet_jas", "p_mm_cy", "permave", "pet_mm_amj", 
                  "pet_mm_jas", "sandave", "siltave", "slope_pct", "srad_wm2_amj", 
                  "srad_wm2_cy", "topwet", "depth_bedrock_m", "p_mm_amj", "p_mm_jas", 
                  "pet_mm_ond", "porosity", "T_min_c_cy", "T_min_c_ond", "pdsi_amj", 
                  "pdsi_cy", "pdsi_jas", "pet_mm_cy", "storage_m", "T_max_c_amj", 
                  "T_max_c_cy", "T_min_c_amj", "T_min_c_jas", "awcave", "srad_wm2_jas", 
                  "T_max_c_jas", "srad_wm2_jfm", "srad_wm2_ond", "T_max_c_jfm", 
                  "pdsi_ond", "T_min_c_jfm", "pcumdist10days", "T_max_c_ond", "pet_mm_jfm", 
                  "p.pet_ond", "swe.p_ond", "p_mm_ond",
                  "p.pet_cy.previous", "p_mm_cy.previous", 
                  "p_mm_jfm.previous", "pet_mm_cy.previous", "p.pet_jas.previous", 
                  "p_mm_jas.previous", "T_min_c_jas.previous", "pcumdist50days", 
                  "srad_wm2_jfm.previous", "p.pet_jfm.previous", "srad_wm2_ond.previous", 
                  "T_max_c_cy.previous", "srad_wm2_cy.previous", "pet_mm_jfm.previous", 
                  "pet_mm_amj.previous", "pet_mm_ond.previous", "pet_mm_jas.previous", 
                  "srad_wm2_jas.previous", "T_min_c_cy.previous", "T_min_c_ond.previous", 
                  "srad_wm2_amj.previous", "p.pet_ond.previous", "p_mm_ond.previous", 
                  "T_max_c_jfm.previous",
                  "dams_n", "maxstorage_af", "normstorage_af", "majordams_n", 
                  "wuse_mm", "irrig_prc", "lulc_water_prc", "lulc_dev_prc", "lulc_wetland_prc",
                  "lulc_forest_prc", "lulc_barren_prc", "lulc_grass_prc", "lulc_ag_prc",
                  "swe.p_amj", "p_mm_amj.previous", "swe_mm_ond.previous", "swe_mm_jfm.previous", 
                  "p.pet_jfm", "swe.p_cy", "swe_mm_cy", "swe.p_cy.previous", "swe.p_amj.previous", 
                  "p_mm_jfm", "swe_mm_ond", "swe.p_ond.previous", "swe.p_jfm", 
                  "swe.p_jfm.previous"),
    Category = factor(c("Physiography", "Physiography", "Physiography", "Climate", 
                        "Climate", "Climate", "Climate", "Physiography", "Climate", 
                        "Climate", "Physiography", "Physiography", "Physiography", "Climate", 
                        "Climate", "Physiography", "Physiography", "Climate", "Climate", 
                        "Climate", "Physiography", "Climate", "Climate", "Climate", 
                        "Climate", "Climate", "Climate", "Physiography", "Climate", 
                        "Climate", "Climate", "Climate", "Physiography", "Climate", 
                        "Climate", "Climate", "Climate", "Climate", "Climate", 
                        "Climate", "Climate", "Climate", "Climate", "Climate", 
                        "Climate", "Climate","Climate", "Climate", 
                        "Climate", "Climate", "Climate", "Climate", 
                        "Climate", "Climate", "Climate", "Climate", 
                        "Climate", "Climate", "Climate", "Climate", 
                        "Climate", "Climate", "Climate", "Climate", 
                        "Climate", "Climate", "Climate", "Climate",
                        "Climate",  "Climate",
                        "Land Use", "Land Use", "Land Use", "Land Use", 
                        "Land Use", "Land Use", "Land Use", "Land Use", "Land Use",
                        "Land Use", "Land Use", "Land Use", "Land Use",
                        "Climate", "Climate", "Climate", "Climate", 
                        "Climate", "Climate", "Climate", "Climate", "Climate", 
                        "Climate", "Climate", "Climate", "Climate", 
                        "Climate")),
    long_name = c("Soil Clay", "Drainage Area", "Elevation", "P/PET (AMJ)", 
                  "P/PET (CY)", "P/PET (JAS)", "P (CY)", "Soil Permeab", "PET (AMJ)", 
                  "PET (JAS)", "Soil Sand", "Soil Silt", "Slope", "SRad (AMJ)", 
                  "SRad (CY)", "Topo Wetness", "Bedrock Depth", "P (AMJ)", "P (JAS)", 
                  "PET (OND)", "Porosity", "Tmin (CY)", "Tmin (OND)", "PDSI (AMJ)", 
                  "PDSI (CY)", "PDSI (JAS)", "PET (CY)", "Storage", "Tmax (AMJ)", 
                  "Tmax (CY)", "Tmin (AMJ)", "Tmin (JAS)", "Soil AWC", "SRad (JAS)", 
                  "Tmax (JAS)", "SRad (JFM)", "SRad (OND)", "Tmax (JFM)", 
                  "PDSI (OND)", "Tmin (JFM)", "Days to 10% P", "Tmax (OND)", "PET (JFM)", 
                  "P/PET (OND)", "SWE/P (OND)", "P (OND)", "P/PET (CY-1)", "P (CY-1)", 
                  "P (JFM-1)", "PET (CY-1)", "P/PET (JAS-1)", 
                  "P (JAS-1)", "Tmin (JAS-1)", "Days to 50% P", 
                  "SRad (JFM-1)", "P/PET (JFM-1)", "SRad (OND-1)", 
                  "Tmax (CY-1)", "SRad (CY-1)", "PET (JFM-1)", 
                  "PET (AMJ-1)", "PET (OND-1)", "PET (JAS-1)", 
                  "SRad (JAS-1)", "Tmin (CY-1)", "Tmin (OND-1)", 
                  "SRad (AMJ-1)", "P/PET (OND-1)", "P (OND-1)", "Tmax (JFM-1)",
                  "# Dams", "Dam Max Storage", "Dam Norm. Storage", "# Major Dams", 
                  "Water Use", "Irrigation", "Water", "Developed", "Wetland",
                  "Forest", "Barren", "Grass", "Agriculture",
                  "SWE/P (AMJ)", "P (AMJ-1)", "SWE (OND-1)", "SWE (JFM-1)", 
                  "P/PET (JFM)", "SWE/P (CY)", "SWE (CY)", "SWE/P (CY-1)", "SWE/P (AMJ-1)", 
                  "P (JFM)", "SWE (OND)", "SWE/OND (P-1)", "SWE/P (JFM)", 
                  "SWE/P (JFM-1)"))

## ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      plot.title=element_text(face="bold", size=rel(1)),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank(),
      plot.margin=unit(c(1,1,1,1), "mm"),
      strip.background=element_blank())
}

theme_set(theme_scz())

## functions
R2 <- function(sim, obs) {
  if (length(sim) != length(obs)) stop("vectors not the same size")
  return((sum((obs-mean(obs))*(sim-mean(sim)))/
            ((sum((obs-mean(obs))^2)^0.5)*(sum((sim-mean(sim))^2)^0.5)))^2)
}

# get p-value from linear model
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
