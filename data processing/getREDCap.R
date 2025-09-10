################################################################################
### DOWNLOAD OF REDCAP DATA
################################################################################

# the purpose of this file:
# avoid repeatedly loading the redcap data through API
# select only necessary columns and get needed start and end times 
# save the data locally in the folder above of the Git repository so that it is
# not uploaded
# rerun this file if you want to update the redcap data

# WARNING !!!!!
# DELETE THE REDCAP TOKEN AFTER USE
# DONT SAVE THIS FILE WITH THE TOKEN


library(httr);library(jsonlite);library(dplyr);library(stringr);library(lubridate)
rm(list=ls())


# load REDCap data
api_url <- "https://redcap.mrc.gm:8443/redcap/api/"
api_token <- ""     # WARNING -> DELETE TOKEN

# Create the request body
post_body <- list(
  token = api_token,
  content = "record",
  format = "json",
  type = "flat"
)

# Send the API request
response <- POST(api_url, body = post_body, encode = "form")

# Check response status
if (http_status(response)$category == "Success") {
  # Parse JSON response
  data <- fromJSON(content(response, "text"))
  print("Data successfully retrieved")
  # print(head(data))  # Display first few records
} else {
  print("Error in API request:")
  print(content(response, "text"))
}

# save all the data to CCH
# Save dataset
# write.csv(data, "../redcap_all.csv")
write.csv(data, "/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_all.csv")


# select necessary columns and convert to datetime
# return min pvl_end as a start time of the observation week
# return max pvl_start as a end time of the observation week
data_selected <- data |>
  
  filter(str_detect(redcap_event_name, "study_visit_week")) |>
  
  select(uid,
         redcap_event_name,
         pvl_start,
         pvl_end) |>
  
  mutate(pvl_start = ymd_hm(pvl_start),
         pvl_end   = ymd_hm(pvl_end)) |>
  
  group_by(uid, redcap_event_name) |>
    
  mutate(setup = as.Date(case_when(!is.na(pvl_start) ~ min(pvl_start, na.rm = TRUE), TRUE ~ as.POSIXct(NA))),
         v01 = as.Date(if_else(sum(!is.na(pvl_start)) > 1, sort(pvl_start, na.last = NA)[2], as.POSIXct(NA))),
         v02 = as.Date(if_else(sum(!is.na(pvl_start)) > 2, sort(pvl_start, na.last = NA)[3], as.POSIXct(NA))),
         v03 = as.Date(if_else(sum(!is.na(pvl_start)) > 3, sort(pvl_start, na.last = NA)[4], as.POSIXct(NA))),
         v04 = as.Date(if_else(sum(!is.na(pvl_start)) > 4, sort(pvl_start, na.last = NA)[5], as.POSIXct(NA))),
         takedown = as.Date(if_else(sum(!is.na(pvl_start)) > 5, sort(pvl_start, na.last = NA)[6], as.POSIXct(NA)))) |>
  
  summarise(
    starttime = min(pvl_end, na.rm = TRUE),
    endtime   = max(pvl_start, na.rm = TRUE),
    setup     = replace(min(setup, na.rm = TRUE), min(setup, na.rm = TRUE) == Inf, NA),
    v01       = replace(min(v01, na.rm = TRUE), min(v01, na.rm = TRUE) == Inf, NA),
    v02       = replace(min(v02, na.rm = TRUE), min(v02, na.rm = TRUE) == Inf, NA),
    v03       = replace(min(v03, na.rm = TRUE), min(v03, na.rm = TRUE) == Inf, NA),
    v04       = replace(min(v04, na.rm = TRUE), min(v04, na.rm = TRUE) == Inf, NA),
    takedown  = replace(min(takedown, na.rm = TRUE), min(takedown, na.rm = TRUE) == Inf, NA),
    .groups   = "drop"
  ) |>
  mutate(n_days = round(difftime(endtime, starttime, units = "days"), digits = 1))

# Save dataset
write.csv(data_selected, "/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_data.csv")

print("data uploaded")




# filtering data set with the pvl log times
data_pvl <- data |>
  
  filter(str_detect(redcap_event_name, "study_visit_week")) |>
  
  mutate(pvl_start = ymd_hm(pvl_start),
         pvl_end   = ymd_hm(pvl_end)) |>
  
  select(uid,
         redcap_event_name,
         pvl_start,
         pvl_end) |>
  
  na.omit()


# Save dataset
write.csv(data_pvl, "/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_pvl.csv")
print("data uploaded")


