## 000_CalculateMetrics.R
# This script will calculate streamflow metrics:
#  - annualfractionnoflow = # of no-flow days / total number of days
#  - zeroflowfirst = first day of calendar year with no-flow

source(file.path("code", "paths+packages.R"))

# get list of all files
dir_Q <- file.path(dir_data, "daily_data_just_flow")
files_Q <- list.files(dir_Q)

# scroll through files
for (f in files_Q){
  # read in file
  df <- 
    readr::read_csv(file.path(dir_Q, f), col_types = cols()) %>% 
    dplyr::rename(gage_ID = site_no, Q = X_00060_00003) %>% 
    dplyr::select(gage_ID, Date, Q) %>% 
    dplyr::mutate(Q_rounded = round(Q, 1))
  
  # get gage ID
  gage_ID <- unique(df$gage_ID)
  
  # get climate year
  df$year <- lubridate::year(df$Date)
  df$month <- lubridate::month(df$Date)
  df$currentclimyear <- df$year
  df$currentclimyear[df$month < 4] <- df$year[df$month < 4] - 1
  
  # get day within climate year
  df$dayofclimyear <- as.numeric(df$Date - lubridate::ymd(paste0(df$currentclimyear, "-03-31")))
  
  # get rid of partial years at start/end
  df_trimmed <- subset(df, currentclimyear > 1979 & currentclimyear < 2018)
  
  # summarize by climate year
  df_frac <-
    df_trimmed %>% 
    dplyr::group_by(currentclimyear) %>% 
    dplyr::summarize(noflowdays = sum(Q == 0),
                     totaldays = sum(is.finite(Q)),
                     annualfractionnoflow = noflowdays/totaldays) %>% 
    dplyr::ungroup()
  
  df_first <-
    df_trimmed %>% 
    subset(Q == 0) %>% 
    dplyr::group_by(currentclimyear) %>% 
    dplyr::summarize(zeroflowfirst = min(dayofclimyear))
  
  df_yr <- 
    dplyr::left_join(df_frac, df_first, by = "currentclimyear") %>% 
    dplyr::mutate(gage_ID = gage_ID)
  
  df_frac_rounded <-
    df_trimmed %>% 
    dplyr::group_by(currentclimyear) %>% 
    dplyr::summarize(noflowdays = sum(Q_rounded == 0),
                     totaldays = sum(is.finite(Q_rounded)),
                     annualfractionnoflow = noflowdays/totaldays)
  
  df_first_rounded <-
    df_trimmed %>% 
    subset(Q_rounded == 0) %>% 
    dplyr::group_by(currentclimyear) %>% 
    dplyr::summarize(zeroflowfirst = min(dayofclimyear))
  
  df_yr_rounded <- 
    dplyr::left_join(df_frac_rounded, df_first_rounded, by = "currentclimyear") %>% 
    dplyr::mutate(gage_ID = gage_ID)
  
  # calculate trends
  manken_frac <- rkt::rkt(df_yr$currentclimyear, df_yr$annualfractionnoflow)
  manken_frac_rounded <- rkt::rkt(df_yr_rounded$currentclimyear, df_yr_rounded$annualfractionnoflow)
  if (sum(is.finite(df_yr$zeroflowfirst)) > 10){
    manken_first <- rkt::rkt(df_yr$currentclimyear, df_yr$zeroflowfirst)
    zft <- manken_first$tau
  } else {
    zft <- NA
  }
  
  if (sum(is.finite(df_yr_rounded$zeroflowfirst)) > 10){
    manken_first_rounded <- rkt::rkt(df_yr_rounded$currentclimyear, df_yr_rounded$zeroflowfirst)
    zft_rounded <- manken_first_rounded$tau
  } else {
    zft_rounded <- NA
  }
  
  df_trends <- tibble::tibble(gage_ID = gage_ID,
                              tau_annualfractionnoflow = manken_frac$tau,
                              tau_annualfractionnoflow_rounded = manken_frac_rounded$tau,
                              tau_zeroflowfirst = zft,
                              tau_zeroflowfirst_rounded = zft_rounded)
  
  # combine all output
  if (f == files_Q[1]){
    df_all <- df_yr
    df_all_rounded <- df_yr_rounded
    df_trends_all <- df_trends
  } else {
    df_all <- dplyr::bind_rows(df_all, df_yr)
    df_all_rounded <- dplyr::bind_rows(df_all_rounded, df_yr_rounded)
    df_trends_all <- dplyr::bind_rows(df_trends_all, df_trends)
  }
  
  print(paste0("Completed ", which(files_Q == f), " of ", length(files_Q), "  ", Sys.time()))
  
}

## save output
readr::write_csv(df_all, file = file.path("results", "000_CalculateMetrics_AnnualHydroMetrics.csv"))
readr::write_csv(df_all_rounded, file = file.path("results", "000_CalculateMetrics_AnnualHydroMetrics_Rounded.csv"))
readr::write_csv(df_trends_all, file = file.path("results", "000_CalculateMetrics_HydroMetricsTrends.csv"))
