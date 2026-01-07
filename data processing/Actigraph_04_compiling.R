################################################################################
### Actigraph compiling
################################################################################

# the purpose of this file

# in this document I load all the actigraph files from the participants individual
# folders,  clean them by the PVL and the weartime validation output from the 
# GGIR routine, rbind them and save them in the data folder without aggregating

# empty environment
rm(list = ls())

# libraries
library(lubridate);library(readr);library(tidyverse)

# specify the week to compile (needs to match naming convention on synology)
week_indicator = "week_4"

# load redcap from CCH for uids and start and end times
redcap = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_all.csv", show_col_types = F) |> 
  select(uid, redcap_event_name, pvl_start, pvl_end, starts_with("pvl_ac")) |>
  drop_na(pvl_start) |>
  filter(str_detect(redcap_event_name, week_indicator)) |>
  filter(!(uid %in% c("ACT029U", "ACT034X", "ACT045O", "ACT048L", "ACT051G", "ACT060E"))) |>
  filter(str_starts(uid, "ACT")) |>
  filter(pvl_actigraph == 1)  # keep only pvls with actions on the actigraph 

# vector of all uids
uids <- unique(redcap$uid)

# file path to participants
filepath_part <- paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/participants/", week_indicator,"/")

# empty dataframe for rbind in loop
df_CR <- data.frame()
df_HR <- data.frame()
df_HRV <- data.frame()
df_IBI <- data.frame()
df_STEPS <- data.frame()
df_Temp <- data.frame()
df_WT <- data.frame()


# loop over all participants
for(uidx in uids[30:72]){
  
  print(uidx)
  
  redcap_uid <- redcap |>
    filter(uid == uidx)
  
  # list the files in the folder of the participant
  files_uid <- list.files(paste0(filepath_part, uidx, "/"))
  
  # get individual files
  file_CR <- files_uid[grepl("CR", files_uid)]
  file_HR <- files_uid[grepl("HR_", files_uid)]
  file_HRV <- files_uid[grepl("HRV", files_uid)]
  file_IBI <- files_uid[grepl("IBI", files_uid)]
  file_STEPS <- files_uid[grepl("Steps", files_uid)]
  file_Temp <- files_uid[grepl("Temp", files_uid)]
  file_WTvalidation <- files_uid[grepl("validation_RAW", files_uid)]
  
  if (length(file_WTvalidation) != 0) {
    
    # load weartime validation and cut by observation period
    wt <- read_csv(paste0(filepath_part, uidx, "/", file_WTvalidation), show_col_types = F) |>
      filter(datetime >= min(redcap_uid$pvl_end) & datetime <= max(redcap_uid$pvl_start))
    
    # set weartime manually to NO WT when there was a pvl
    # 0 for valid, 1 for invalid
    # loop through pvl visits
    for(i in 1:nrow(redcap_uid)) {
      
      startvalue <- redcap_uid$pvl_start[i]
      endvalue <-  redcap_uid$pvl_end[i]
      
      # set values to 1 for invalid during pvl visits
      wt <- wt |>
        mutate(invalidepoch = if_else(datetime > startvalue & datetime < endvalue, 1, invalidepoch),
               uid = uidx) 
      
      df_WT <- rbind(df_WT, wt)
    }
    
  } else{
    print(paste0("skipping...no weartime validation data for participant: ",uidx))
      next}
  

  
  # rbind the files if they exist and
  # cleaning based on weartime validation
  if(length(file_CR) != 0){
    data = read_csv(paste0(filepath_part, uidx, "/", file_CR), show_col_types = F)
    
    if (nrow(data) != 0) {
    
    data <- data |>
      mutate(datetime = floor_date(start_timestamp, unit = "minutes")) |>
      left_join(wt |> select(datetime, invalidepoch), by = "datetime") |>
      mutate(bout_length_seconds = if_else(is.na(invalidepoch) | invalidepoch == 1, NA, bout_length_seconds)) |>
      filter(datetime >= min(redcap_uid$pvl_end) & datetime <= max(redcap_uid$pvl_start))
    

    # remove non matching not necessary columns
    data <- data[ , !(names(data) %in% c("index", "start", "end"))]
    
    df_CR <- rbind(df_CR, data)
    }
  }
  if(length(file_HR) != 0){
    data = read_csv(paste0(filepath_part, uidx, "/", file_HR), show_col_types = F)
    
    if (nrow(data) != 0) {
      
    data <- data |>
      mutate(datetime = timestamp) |>
      left_join(wt |> select(datetime, invalidepoch), by = "datetime") |>
      mutate(HeartRate = if_else(is.na(invalidepoch) | invalidepoch == 1, NA, HeartRate)) |>
      filter(datetime >= min(redcap_uid$pvl_end) & datetime <= max(redcap_uid$pvl_start))
    
    df_HR <- rbind(df_HR, data)
  }
  }
  if(length(file_HRV) != 0){
    data = read_csv(paste0(filepath_part, uidx, "/", file_HRV), show_col_types = F)
    
    if (nrow(data) != 0) {
  
    colname <- names(data)[1]
    
    data <- data |>
      mutate(datetime = floor_date(.data[[colname]], unit = "minutes")) |>
      left_join(wt |> select(datetime, invalidepoch), by = "datetime")
    
    cols_to_clean <- data %>%
      select(-c(datetime, uid, invalidepoch)) %>%
      select(where(is.numeric)) %>%
      names()
    
    data <- data |>
      mutate(across(all_of(cols_to_clean),
                    ~ if_else(is.na(invalidepoch) | invalidepoch == 1, NA_real_, .))) |>
      filter(datetime >= min(redcap_uid$pvl_end) & datetime <= max(redcap_uid$pvl_start))
    
    if (ncol(data) > 16) {
      data <- data |>
        select(-end, -hrv_epoch_start, -hrv_epoch_end) |>
        rename("timestamp" = "start")
      }
    
    df_HRV <- rbind(df_HRV, data)
    }
  }
  if(length(file_IBI) != 0){
    data = read_csv(paste0(filepath_part, uidx, "/", file_IBI), show_col_types = F)
    
    if (nrow(data) != 0) {
      
    data <- data |>
      mutate(datetime = floor_date(timestamp, unit = "minutes")) |>
      left_join(wt |> select(datetime, invalidepoch), by = "datetime") |>
      mutate(interbeat_interval = if_else(is.na(invalidepoch) | invalidepoch == 1, NA, interbeat_interval)) |>
      filter(datetime >= min(redcap_uid$pvl_end) & datetime <= max(redcap_uid$pvl_start))
    
    df_IBI <- rbind(df_IBI, data)
    }
    }
    
  if(length(file_STEPS) != 0){
    
    data = read_csv(paste0(filepath_part, uidx, "/", file_STEPS), show_col_types = F)
    
    if(nrow(data) != 0){
    
    data = data |>
      mutate(datetime = floor_date(timestamp, unit = "minutes")) |>
      left_join(wt |> select(datetime, invalidepoch), by = "datetime") |>
      mutate(steps = if_else(is.na(invalidepoch) | invalidepoch == 1, NA, steps)) |>
      filter(datetime >= min(redcap_uid$pvl_end) & datetime <= max(redcap_uid$pvl_start))
    
    df_STEPS <- rbind(df_STEPS, data)
  }
  }
  if(length(file_Temp) != 0){
    
    data = read_csv(paste0(filepath_part, uidx, "/", file_Temp), show_col_types = F) #, skip = 20) |>
      # dplyr::select(-Timestamp)
    # colnames(data)[1:3] <- c("Temperature", "datetime", "uid")
    # data <- data |>
      # mutate(datetime = floor_date(datetime, unit = "minute"))
    
    if(nrow(data) != 0){
      
    data <- data |>
      left_join(wt |> select(datetime, invalidepoch), by = "datetime") |>
      mutate(Temperature = if_else(is.na(invalidepoch) | invalidepoch == 1, NA, Temperature)) |>
      filter(datetime >= min(redcap_uid$pvl_end) & datetime <= max(redcap_uid$pvl_start))
    
    # remove non matching not necessary columns
    data <- data[ , !(names(data) %in% c("Timestamp"))]
    
    df_Temp <- rbind(df_Temp, data)
    }
  }
}

# save the cleaned but RAW resolution data
write_csv(df_CR, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_actigraph_CR_RAW_data_clean.csv"))
write_csv(df_HR, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_actigraph_HR_RAW_data_clean.csv"))
write_csv(df_HRV, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_actigraph_HRV_RAW_data_clean.csv"))
write_csv(df_IBI, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_actigraph_IBI_RAW_data_clean.csv"))
write_csv(df_STEPS, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_actigraph_Steps_RAW_data_clean.csv"))
write_csv(df_Temp, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_actigraph_Temp_RAW_data_clean.csv"))
write_csv(df_WT, paste0("/Volumes/FS/_ISPM/CCH/Actual_Project/data/Participants/", week_indicator, "/", week_indicator, "_actigraph_weartime_RAW_data_clean.csv"))

