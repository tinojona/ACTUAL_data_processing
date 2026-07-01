###############################################################################
# DATA CLEANING SCRIPT BASED ON vignettes/noise_preparation.Rmd
# in this file I implement the noise preparation routine
# and save the data in data/participants/week1


rm(list=ls())


# libraries
library(readr);library(tidyr);library(dplyr);library(readxl);library(zoo)
library(lubridate);library(stringr);library(ggplot2);library(gridExtra); library(grid)

# specify the week to compile (needs to match naming convention on synology)
week_indicator = "week_4"


# LOAD DATA -----------------------------------------------------------------------

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

# Prepare loop ------------------------
noise_data_hourly <- data.frame()
noise_data_weekly <- data.frame()
noise_data_hourly_corr <- data.frame()
noise_data_weekly_corr <- data.frame()

# save plots 
plots <- list()
plots.tuckey.laeq <- list()
plots.tuckey.ir <- list()

output_file <- sprintf("data processing/noise_reports/cleaningroutin_outliers_summary_%s.txt", week_indicator)

# Start redirecting console output to the file
sink(output_file)

# uidx <- uids[7]

# Cleaning routine ----------------------------------


for(uidx in uids){

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
  
    #   Remove outliers
  data_N_uid <- data_N_uid |>
    mutate(NS = ifelse(NS > mean_noise + sd_noise | NS < mean_noise - sd_noise, NA, NS))
  
  # check if data availability is >= 90% after removing outliers
  NA_perc = mean(is.na(data_N_uid$NS))
  cat("Percentage missing values (weekly) after removing outliers:", print(NA_perc), "\n") 
  
  # and exclude the participant if it is not the case
  if(NA_perc > .1 | is.na(NA_perc)){
    next
  }
  
  # calculate hourly and weekly noise indicators
  # total
  L_Aeq_tot = 10 * log10( mean(10^(0.1 * data_N_uid$NS), na.rm = TRUE ) ) 
  
  # hourly
  noise_ind_hourly <- data_N_uid |>
    group_by(dow, hour) |>
    
    # A-weighted equivalent continuous hourly sound levels L_Aeq_1h
    mutate(
      energy = 10^(0.1 * NS) # work on energy level (linear) before going back to decibel scale
    )|>
    summarize(L_Aeq_1h = 10 * log10(mean(energy, na.rm = TRUE )) ,
              IR_1h = {
                Ltot <- 10 * log10(mean(energy, na.rm = TRUE))
                K <- Ltot + 3 # threshold is average sound level + 3
                idx <- NS >= K # identify observations which overpass threshold K
                sum(energy[idx], na.rm = TRUE) / sum(energy, na.rm = TRUE) # IR is the proportion of overall energy of the events to the overall weekly energy
              },
              .groups = "drop")|>
    mutate(uid = uidx)
  
    # weekly
  noise_ind_weekly <- data_N_uid |>
    mutate(
      energy = 10^(0.1 * NS) # work on energy level (linear) before going back to decibel scale
    )|>
    summarize( 
      
      # LAeq --------------------
      L_Aeq_tot = 10 * log10(mean(energy, na.rm = TRUE)),
      L_Aeq_day = 10 * log10(mean(energy[hour >= 6 & hour < 22], na.rm = TRUE)),
      L_Aeq_night = 10 * log10(mean(energy[hour < 6 | hour >= 22], na.rm = TRUE)),
      
      # IR 24h --------------------
      IR_24h = {
        Ltot <- 10 * log10(mean(energy, na.rm = TRUE))
        K <- Ltot + 3 # threshold is average sound level + 3
        idx <- NS >= K # identify observations which overpass threshold K
        sum(energy[idx], na.rm = TRUE) / sum(energy, na.rm = TRUE) # IR is the proportion of overall energy of the events to the overall weekly energy
      },
    
      # IR day ------------------
      IR_day = {
        sel <- hour >= 6 & hour < 22
        x_energy <- energy[sel]
        x_NS <- NS[sel]
        Ltot <- 10 * log10(mean(x_energy, na.rm = TRUE))
        K <- Ltot + 3
        idx <- x_NS >= K
        sum(x_energy[idx], na.rm = TRUE) / sum(x_energy, na.rm = TRUE)
      },
      
      # IR night ------------------
      IR_night = {
        sel <- hour < 6 | hour >= 22
        x_energy <- energy[sel]
        x_NS <- NS[sel]
        Ltot <- 10 * log10(mean(x_energy, na.rm = TRUE))
        K <- Ltot + 3
        idx <- x_NS >= K
        sum(x_energy[idx], na.rm = TRUE) / sum(x_energy, na.rm = TRUE)
      },
      
      # L_den ------------------
      L_den_day = 10 * log10(mean(energy[hour >= 6 & hour < 18], na.rm = TRUE)),
      L_den_eve = 10 * log10(mean(energy[hour >= 18 & hour < 22], na.rm = TRUE)),
      L_den_nig = 10 * log10(mean(energy[hour < 6 | hour >= 22], na.rm = TRUE)),
      
      L_den = 10 * log10(
        (1/24) *
          (12 * 10^(L_den_day / 10) +
             4  * 10^((L_den_eve + 5) / 10) +
             8  * 10^((L_den_nig + 10) / 10))
      ),
      
      .groups = "drop"
    ) |>
    mutate(uid = uidx)
  
  noise_data_hourly <- rbind(noise_data_hourly, noise_ind_hourly)
  noise_data_weekly <- rbind(noise_data_weekly, noise_ind_weekly)
  
#   Weekly corrected (Tuckey's plots) -----------------------------------------
  
  # For LAeq
  
  noise_ind_hourly.corr <- noise_ind_hourly |>
    group_by(hour, uid) |>
    mutate(
      q25 = quantile(L_Aeq_1h, 0.25, na.rm = TRUE),
      q75 = quantile(L_Aeq_1h, 0.75, na.rm = TRUE),
      lowlim = q25 - 1.5 * (q75 - q25),
      highlim = q75 + 1.5 * (q75 - q25),
      median = median(L_Aeq_1h, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      outlier.laeq = L_Aeq_1h < lowlim | L_Aeq_1h > highlim
    ) 
  
  data_N_uid.corr<- merge(
    data_N_uid, noise_ind_hourly.corr%>%
      select(dow, hour, uid, outlier.laeq
      ), 
    by=c("dow", "hour", "uid")
  )
  #   Report the number of dropped observations for each time series.
  cat("Dropped hourly values based on Tuckey's plots (Laqe):", sum(noise_ind_hourly.corr$outlier.laeq, na.rm = TRUE), "\n")
  cat("Dropped observations based on Tuckey's plots (Laqe):", sum(data_N_uid.corr$outlier.laeq, na.rm = TRUE), "\n")
  
  # plot time series
  plot_outliers.laeq <- 
    ggplot(data_N_uid.corr,
           aes(x=datetime, y=NS))+
    geom_point() + geom_line()+
    geom_point(data=data_N_uid[data_N_uid.corr$outlier.laeq==T,], col="red")+
    xlab(paste("Date (", uidx, ")")) +
    theme_classic()
  plots.tuckey.laeq[[uidx]] <- plot_outliers.laeq
  
  
#   Repeat for IQR
  noise_ind_hourly.corr <- noise_ind_hourly.corr |>
    group_by(hour, uid) |>
    mutate(
      q25 = quantile(IR_1h, 0.25, na.rm = TRUE),
      q75 = quantile(IR_1h, 0.75, na.rm = TRUE),
      lowlim = q25 - 1.5 * (q75 - q25),
      highlim = q75 + 1.5 * (q75 - q25),
      median = median(IR_1h, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      outlier.ir= IR_1h < lowlim | IR_1h > highlim
    ) 
  
  data_N_uid.corr<- merge(
    data_N_uid.corr, noise_ind_hourly.corr%>%
      select(dow, hour, uid, outlier.ir
      ), 
    by=c("dow", "hour", "uid")
  )
    
  #   Report the number of dropped observations for each time series.
  cat("Dropped hourly values based on Tuckey's plots (IR):", sum(noise_ind_hourly.corr$outlier.ir, na.rm = TRUE), "\n")
  cat("Dropped observations based on Tuckey's plots (IR):", sum(data_N_uid.corr$outlier.ir, na.rm = TRUE), "\n")
  
  # plot time series
  plot_outliers.ir<- 
    ggplot(data_N_uid.corr,
           aes(x=datetime, y=NS))+
    geom_point() + geom_line()+
    geom_point(data=data_N_uid[data_N_uid.corr$outlier.ir==T,], col="red")+
    xlab(paste("Date (", uidx, ")")) +
    theme_classic()
  plots.tuckey.ir[[uidx]] <- plot_outliers.ir
  
#   Calculate corrected weekly indicators 
  
  noise_ind_weekly.corr <- data_N_uid.corr |>
    mutate(
      energy = 10^(0.1 * NS) # work on energy level (linear) before going back to decibel scale
    )
  
  laeq_indicators <- noise_ind_weekly.corr%>%
    filter(!outlier.laeq) |>
    summarize( 
    
      # LAeq --------------------
      L_Aeq_tot_corr = 10 * log10(mean(energy, na.rm = TRUE)),
      L_Aeq_day_corr = 10 * log10(mean(energy[hour >= 6 & hour < 22], na.rm = TRUE)),
      L_Aeq_night_corr = 10 * log10(mean(energy[hour < 6 | hour >= 22], na.rm = TRUE)),
      
      # L_den ------------------
      L_den_day_corr = 10 * log10(mean(energy[hour >= 6 & hour < 18], na.rm = TRUE)),
      L_den_eve_corr = 10 * log10(mean(energy[hour >= 18 & hour < 22], na.rm = TRUE)),
      L_den_nig_corr = 10 * log10(mean(energy[hour < 6 | hour >= 22], na.rm = TRUE)),
      
      L_den_corr = 10 * log10(
        (1/24) *
          (12 * 10^(L_den_day_corr / 10) +
             4  * 10^((L_den_eve_corr + 5) / 10) +
             8  * 10^((L_den_nig_corr + 10) / 10))
      ),
      
      .groups = "drop"
    ) |>
    mutate(uid = uidx)
      
#   IR indicators
  IR_indicators <- noise_ind_weekly.corr%>%
    filter(!outlier.ir) |>
    summarize( 
      
      # IR 24h --------------------
      IR_24h_corr = {
        Ltot <- 10 * log10(mean(energy, na.rm = TRUE))
        K <- Ltot + 3 # threshold is average sound level + 3
        idx <- NS >= K # identify observations which overpass threshold K
        sum(energy[idx], na.rm = TRUE) / sum(energy, na.rm = TRUE) # IR is the proportion of overall energy of the events to the overall weekly energy
      },
      
      # IR day ------------------
      IR_day_corr = {
        sel <- hour >= 6 & hour < 22
        x_energy <- energy[sel]
        x_NS <- NS[sel]
        Ltot <- 10 * log10(mean(x_energy, na.rm = TRUE))
        K <- Ltot + 3
        idx <- x_NS >= K
        sum(x_energy[idx], na.rm = TRUE) / sum(x_energy, na.rm = TRUE)
      },
      
      # IR night ------------------
      IR_night_corr = {
        sel <- hour < 6 | hour >= 22
        x_energy <- energy[sel]
        x_NS <- NS[sel]
        Ltot <- 10 * log10(mean(x_energy, na.rm = TRUE))
        K <- Ltot + 3
        idx <- x_NS >= K
        sum(x_energy[idx], na.rm = TRUE) / sum(x_energy, na.rm = TRUE)
      },
        .groups = "drop"
    ) |>
    mutate(uid = uidx)
  
  noise_ind_hourly.corr <- noise_ind_hourly.corr%>%select(dow, hour, L_Aeq_1h, IR_1h, uid, contains("outlier"))
  noise_ind_weekly.corr <- cbind(laeq_indicators%>%select(-c("uid")), IR_indicators)
  
  noise_data_hourly_corr <- rbind(noise_data_hourly_corr, noise_ind_hourly.corr)
  noise_data_weekly_corr <- rbind(noise_data_weekly_corr, noise_ind_weekly.corr)

}

sink()

# store plots -----------------

# normal outliers
filename <- sprintf("data processing/noise_reports/noiseplots_outlier_%s.pdf", week_indicator)
pdf(filename,
    width = 8, height = 3)
for (p in plots) {
  print(p)
}
dev.off()

# Tuckey corrected LAeq
filename <- sprintf("data processing/noise_reports/noiseplots_outlier_corrected_LAeq%s.pdf", week_indicator)
pdf(filename,
    width = 8, height = 3)
for (p in plots.tuckey.laeq) {
  print(p)
}
dev.off()

# Tuckey corrected IR
filename <- sprintf("data processing/noise_reports/noiseplots_outlier_corrected_IR%s.pdf", week_indicator)
pdf(filename,
    width = 8, height = 3)
for (p in plots.tuckey.ir) {
  print(p)
}
dev.off()

# save data ----------------------------------
write_csv(noise_data_hourly, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_NS_indicators_hourly_data_clean.csv"))
# write_csv(noise_data_daily, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_NS_indicators_daily_data_clean.csv"))
write_csv(noise_data_weekly, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_NS_indicators_weekly_data_clean.csv"))
# 
write_csv(noise_data_hourly_corr, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_NS_indicators_hourly_data_clean_corr.csv"))
write_csv(noise_data_weekly_corr, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_NS_indicators_weekly_data_clean_corr.csv"))


