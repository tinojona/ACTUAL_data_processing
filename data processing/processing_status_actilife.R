# Getting the actigraph processing status ---------------------------------

# In this document, I want to determine for which participants there still needs
# to be run the Actilife software and the hr.R script.



# packages
library(tidyverse); library(lubridate)

# REDCap for participants per observation week
redcap = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_all.csv") |>
  filter(!(uid %in% c("ACT029U", "ACT034X", "ACT045O", "ACT048L", "ACT051G")))

# set up empty list for file names
file_list <- list()


# paths to different folders
# 1) RAW files for GGIR
file_list[[1]] <- list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv", full.names = T, pattern = "RAW")

# 2) temperature 
file_list[[2]] <- list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/csv", full.names = T, pattern = "Temp")

# 3) steps
file_list[[3]] <- list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/steps", full.names = T)

# 4) heart rate 
file_list[[4]] <- list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/HR", full.names = T, pattern = "HeartRate.csv")

# 5) heart rate var
file_list[[5]] <- list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/HR", full.names = T, pattern = "HeartRateV")

# 6) inter beat interval
file_list[[6]] <- list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/HR", full.names = T, pattern = "Int")

# 7) cardiac rythym
file_list[[7]] <- list.files("/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/HR", full.names = T, pattern = "Card")


# set up df
# CHANGE WEEK INDICATOR IN LINE 45, 61, 71
redcap_sel <- redcap |>
  filter(str_detect(redcap_event_name, "week_2")) |>
  filter(str_starts(uid, "ACT"))

df <- data.frame(uid = unique(redcap_sel$uid)) 
df[,2:8] = NA
colnames(df) <- c("uid", "raw", "temp", "steps", "hr", "hrv", "ibi", "cr")

# loop through participants
for(i in 1:length(df$uid)){
  print(df$uid[i])
  
  # loop over list of 7 variables
  for(j in 1:7) {
    
    # if a list of a variable is a uid OF A DEFINED WEEK
    # set the row and col to 1
    file_j <- file_list[[j]][grepl(paste0(df$uid[i],"_week2"), file_list[[j]])]
    
    if (length(file_j) == 1) {
      
      df[i,j+1] <- 1
    }
  }
}


write_csv(df, "/Volumes/FS/_ISPM/CCH/Actual_Project/data-raw/Actigraph/Lucy_folder/week2_status.csv")
