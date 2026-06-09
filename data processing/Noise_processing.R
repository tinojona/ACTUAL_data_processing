###############################################################################
# DATA CLEANING SCRIPT BASED ON vignettes/noise_preparation.Rmd
# in this file I implement the noise preparation routine
# and save the data in data/participants/week1




rm(list=ls())


# libraries
library(readr);library(tidyr);library(dplyr);library(readxl);library(zoo)
library(lubridate);library(stringr);library(ggplot2);library(gridExtra); library(grid)

# specify the week to compile (needs to match naming convention on synology)
week_indicator = "week_2"




# LOAD DATA
#---- 

# load redcap from CCH
# iButton and Noise data
data <- read_csv(paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Participants/", week_indicator,  "_IB_RAW_data_unclean.csv"))

# REDCap for uids and start and end times
redcap = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_all.csv") |> 
  select(uid, redcap_event_name, pvl_start, pvl_end, starts_with("pvl_ib")) |>
  drop_na(pvl_start) |>
  filter(str_detect(redcap_event_name, week_indicator)) |>
  filter(!(uid %in% c("ACT029U", "ACT034X", "ACT045O"))) |>
  filter(str_starts(uid, "ACT"))


# noise data
data_N <- data |>
  mutate(Variable = if_else(Variable == "_NS", "NS", Variable)) |>
  filter(Variable == "NS")|>
  pivot_wider(names_from = Variable, values_from = Value)|>
  mutate(
    NS_MA = rollmean(NS, k = 8, fill = NA, align = "left"),
    hour = hour(datetime),
    dow = wday(datetime, week_start = 1))


uids <- unique(redcap$uid)


noise_data_hourly <- data.frame()
noise_data_daily <- data.frame()
noise_data_weekly <- data.frame()

# save plots 
plots <- list()

# cleaning routine
for(uidx in uids){
  # browser()
  
  print(uidx)
  
  # select noise data for participant
  data_N_uid <- data_N |>
    filter(uid == uidx)
  
  # skip participant if noise data is empty (missing from the observations)
  if(nrow(data_N_uid) == 0){
    next
  }
  
  # select redcap data for participant
  redcap_uid <-  redcap |>
    filter(uid == uidx)
  
  # cut by observation period pvl
  startvalue <- min(redcap_uid$pvl_end, na.rm = TRUE)
  endvalue <-  max(redcap_uid$pvl_start, na.rm = TRUE)
  
  data_N_uid <-  data_N_uid |>
    filter(datetime >= startvalue & datetime <= endvalue)
  
  
  # restrict to the same observation period for all
  # tuesday 6am to saturday 6am
  data_N_uid <- data_N_uid |>
    filter(
      (dow == 2 & hour >= 6) |  # Tuesday from 6am
        (dow %in% 3:5) |        # Tuesday to Thursday
        (dow == 6 & hour < 6) # Saturday up to 6am
    )
    
  
  # check if data availability is >= 90%
  NA_perc = mean(is.na(data_N_uid$NS))
  cat("Percentage missing values (weekly):", print(NA_perc), "\n") 
  
  # and exclude the participant if it is not the case
  if(NA_perc > .1 | is.na(NA_perc)){
    next
    }
  
  
  # remove outliers based on five day mean and 3x sd
  mean_noise = mean(data_N_uid$NS, na.rm = TRUE)
  sd_noise = sd(data_N_uid$NS, na.rm = TRUE) * 3

  #   Report the number of dropped observations for each time series.
  drop_idx <- data_N_uid$NS > mean_noise + sd_noise |
    data_N_uid$NS < mean_noise - sd_noise
  cat("Dropped observations:", sum(drop_idx, na.rm = TRUE), "\n")
  
  # plot time series
  data_N_uid$drop <- drop_idx
  plot_outliers <- 
    ggplot(data_N_uid,
         aes(x=datetime, y=NS))+
    geom_point() + geom_line()+
    geom_point(data=data_N_uid[data_N_uid$drop==T,], col="red")+
    xlab(paste("Date (", uidx, ")")) +
    theme_classic()
  plots[[uidx]] <- plot_outliers
  data_N_uid$drop <- NULL
  
  data_N_uid <- data_N_uid |>
    mutate(NS = ifelse(NS > mean_noise + sd_noise | NS < mean_noise - sd_noise, NA, NS))
  
  
  # check if data availability is >= 90% after removing outliers
  NA_perc = mean(is.na(data_N_uid$NS))
  cat("Percentage missing values (weekly) after removing outliers:", print(NA_perc), "\n") 
  
  # and exclude the participant if it is not the case
  if(NA_perc > .1 | is.na(NA_perc)){
    next
  }
  
  # calculate hourly, daily, and weekly noise indicators
  
  # total
  L_Aeq_tot = 10 * log10( mean(10^(0.1 * data_N_uid$NS), na.rm = TRUE ) ) 
  
  # hourly
  noise_ind_hourly <- data_N_uid |>
    group_by(dow, hour) |>
    
    # A-weighted equivalent continuous hourly sound levels L_Aeq_1h
    summarize(L_Aeq_1h = 10 * log10( mean(10^(0.1 * NS), na.rm = TRUE ) ),
              .groups = "drop") |>

    mutate(uid = uidx)
  
  # daily
  noise_ind_daily <- data_N_uid |>
    group_by(dow) |>
    
    summarize(
      
      # A-weighted equivalent continuous daily sound levels L_Aeq_24h
      L_Aeq_24h = 10 * log10( mean(10^(0.1 * NS), na.rm = TRUE ) ),
      
      # A-weighted equivalent continuous daytime sound levels between 6-22h L_Aeq_day
      L_Aeq_day = 10 * log10( mean(10^(0.1 * NS[hour >= 6 & hour < 22]), na.rm = TRUE ) ),
      
      # A-weighted equivalent continuous nighttime sound levels 22-6 L_Aeq_night
      L_Aeq_night = 10 * log10( mean(10^(0.1 * NS[hour < 6 | hour >= 22]), na.rm = TRUE ) ),
      
      # Daily intermittency ratio IR_24h
      IR_24h = (10^(0.1 * L_Aeq_24h) ) / (10^(0.1 * L_Aeq_tot) ) * 100,
      
      # daytime intermittency ratio between 6-22 IR_day
      IR_day = (10^(0.1 * L_Aeq_day) ) / (10^(0.1 * L_Aeq_tot) ) * 100,
      
      # nighttime intermittency ratio between 6-22 IR_night
      IR_night = (10^(0.1 * L_Aeq_night) ) / (10^(0.1 * L_Aeq_tot) ) * 100,
      
      # variables for L_den
      L_den_day = 10 * log10( mean(10^(0.1 * NS[hour >= 6 & hour < 18]), na.rm = TRUE ) ),
      L_den_eve = 10 * log10( mean(10^(0.1 * NS[hour >= 18 & hour < 22]), na.rm = TRUE ) ),
      L_den_nig = 10 * log10( mean(10^(0.1 * NS[hour < 6 | hour >= 22]), na.rm = TRUE ) ),
      
      # L_den
      L_den = 10 * log10( (1/24) * 
                            (12 * 10^( L_den_day /10) +
                               4 * 10^( (L_den_eve + 5 ) /10) +
                               8 * 10^( (L_den_nig + 10 ) /10))),
      .groups = "drop") |>
    
    mutate(uid = uidx)
  
  
  
  
  
  # weekly
  noise_ind_weekly <- data_N_uid |>
    summarize(
      
      # A-weighted equivalent continuous daily sound levels L_Aeq_24h
      L_Aeq = 10 * log10( mean(10^(0.1 * NS), na.rm = TRUE ) ),
      
      # A-weighted equivalent continuous daytime sound levels between 6-22h L_Aeq_day
      L_Aeq_day = 10 * log10( mean(10^(0.1 * NS[hour >= 6 & hour < 22]), na.rm = TRUE ) ),
      
      # A-weighted equivalent continuous nighttime sound levels 22-6 L_Aeq_night
      L_Aeq_night = 10 * log10( mean(10^(0.1 * NS[hour < 6 | hour >= 22]), na.rm = TRUE ) ),
      
      # Daily intermittency ratio IR_24h
      IR = (10^(0.1 * L_Aeq) ) / (10^(0.1 * L_Aeq_tot) ) * 100,
      
      # daytime intermittency ratio between 6-22 IR_day
      IR_day = (10^(0.1 * L_Aeq_day) ) / (10^(0.1 * L_Aeq_tot) ) * 100,
      
      # nighttime intermittency ratio between 6-22 IR_night
      IR_night = (10^(0.1 * L_Aeq_night) ) / (10^(0.1 * L_Aeq_tot) ) * 100,
      
      # variables for L_den
      L_den_day = 10 * log10( mean(10^(0.1 * NS[hour >= 6 & hour < 18]), na.rm = TRUE ) ),
      L_den_eve = 10 * log10( mean(10^(0.1 * NS[hour >= 18 & hour < 22]), na.rm = TRUE ) ),
      L_den_nig = 10 * log10( mean(10^(0.1 * NS[hour < 6 | hour >= 22]), na.rm = TRUE ) ),
      
      # L_den
      L_den = 10 * log10( (1/24) * 
                            (12 * 10^( L_den_day /10) +
                               4 * 10^( (L_den_eve + 5 ) /10) +
                               8 * 10^( (L_den_nig + 10 ) /10))),
      .groups = "drop") |>
    
    mutate(uid = uidx)
  
  
  
  noise_data_hourly <- rbind(noise_data_hourly, noise_ind_hourly)
  noise_data_daily <- rbind(noise_data_daily, noise_ind_daily)
  noise_data_weekly <- rbind(noise_data_weekly, noise_ind_weekly)
  
  
  }

# store plots
filename <- sprintf("data processing/noiseplots_outlier_%s.pdf", week_indicator)
pdf(filename,
    width = 8, height = 3)
for (p in plots) {
  print(p)
}
dev.off()

# 
# write_csv(noise_data_hourly, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_NS_indicators_hourly_data_clean.csv"))
# write_csv(noise_data_daily, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_NS_indicators_daily_data_clean.csv"))
# write_csv(noise_data_weekly, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_NS_indicators_weekly_data_clean.csv"))
# 


