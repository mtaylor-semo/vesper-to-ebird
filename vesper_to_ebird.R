# Script to convert metadata csv output from Vesper into eBird checklist format
# to upload NFC counts.   Inspired by Richard Littauer's Vesper to eBird
# javascripts: https://github.com/RichardLitt/vesper-to-ebird


# Can I make this run from command line? If so, how to input file

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(suncalc)


# Constants ---------------------------------------------------------------

# Most constants are based on eBird's "checklist format" for importing.

location_name <- "Miller Reserve"
latitude <- 37.132025
longitude <- -89.461307
state = "MO" # eBird uses two-letter postal codes
country = "US" # eBird uses two-letter code for US
protocol = "P54" # eBird protocol code for NFC
num_observers <- 1 # Single observer
duration <- "60"
all_obs_reported <- "N"
dist_traveled = 0 # if needed for stationary NFC
area_covered = 0 # if needed for stationary NFC
notes = "Recorded using Wildlife Acoustics SM mini with single stub microphone
at 48kHz, 16 bit mono. NFC calls detected using Vesper (https://github.com/HaroldMills/Vesper) with Nighthawk detector (https://github.com/bmvandoren/Nighthawk) unless noted. Local calls detected manually and with Hawkears (https://github.com/jhuus/HawkEars). This checklist was created automatically using https://github.com/mtaylor-semo/vesper-to-ebird."


# site <- "prairie" # this is from Vesper, from the name of the specific SMmini recorder
# recorder <- "SMmini"
# mic <- "stub"
# station_info <- ""

time_zone <- "Etc/GMT+6"
dt_format = "%m/%d/%y %H:%M:%S"


# Functions ---------------------------------------------------------------

get_dusk_dawn <- function(x, y) {
  dusk <- vebird$nautical_dusk[1]
  dusk <- dusk - hours(1)
  
}

# Read and wrangle the data -----------------------------------------------

# Can play around with this for running from command line later, if desired.
# file_to_open <- commandArgs(trailingOnly = TRUE)
# 
# if (length(file_to_open) == 0) {
#   print("Please specify a csv file to open.")
# } else{
#   dat <- read_csv(args)  
#   head(dat)
# }

# Hard-coded file name for testing
vebird <- read_csv(
  "two_days_out.csv",
  skip = 1,
  col_names = c("season", "year", "detector", "species", "site", "date",
                "recording_start", "recording_length", "detection_time",
                "real_detection_time", "real_detection_datetime",
                "rounded_to_half_hour", "duplicate", "sunset", "civil_dusk",
                "nautical_dusk", "astronomical_dusk", "astronomical_dawn",
                "nautical_dawn", "civil_dawn", "sunrise", "moon_altitude",
                "moon_illumination"))

vebird <- vebird |>
  select(species, site, date, recording_start, detection_time,
         real_detection_time, real_detection_datetime, duplicate,
         nautical_dusk, astronomical_dusk, astronomical_dawn, nautical_dawn)

vebird <- vebird |>
  mutate(
    date = as_date(
      date,
      format = "%m/%d/%y"),
    real_detection_datetime = as_datetime(
      real_detection_datetime,
      format = dt_format,
      tz = time_zone
    ),
    nautical_dusk = as_datetime(
      nautical_dusk,
      format = dt_format,
      tz = time_zone),
    nautical_dawn = as_datetime(
      nautical_dawn,
      format = dt_format,
      tz = time_zone),
    astronomical_dusk = as_datetime(
      astronomical_dusk,
      format = dt_format,
      tz = time_zone),
    astronomical_dawn = as_datetime(
      astronomical_dawn,
      format = dt_format,
      tz = time_zone)
  )

# Create a detection time ceiling that should make grouping easier

vebird <- vebird |> 
  mutate(start_time = floor_date(real_detection_datetime, "hour"),
         group_date = floor_date(start_time + hours(12), 'day'))

# Modify first start time of each date to coincide with astronical dusk.
# do the same for dawn
vebird <- vebird |>
  mutate(
    start_time = if_else(
      start_time < nautical_dusk,
      ceiling_date(nautical_dusk, "minute"),
      start_time
    ),
    duration = if_else(
      start_time == ceiling_date(nautical_dusk, "minute"),
      as.character(
        ceiling_date(nautical_dusk, "hour") - ceiling_date(nautical_dusk, "minute")
      ),
      "60"
    )
  )



# Use floor to get start time, then add 60 minutes for duration
# If floor is less than first_dusk (below), then replace with first dusk and
# calculate duration.


vebird |> 
  group_by(detection_time_ceiling) |> 
  summarise(N = n())



# This from https://stackoverflow.com/a/54807068/3832941
# Not sure if it will be helpful but saving just in case.
vebird %>%
  group_by(detection_time_ceiling) %>%
  group_walk(~ write_csv(.x, paste0(.y$detection_time_ceiling, "ebird.csv")))



# Prepare header for csv --------------------------------------------------

# This is the constant information for the Miller Reserve site
# that will appear at the start of all NFC checklists.  Based on
# eBird's "checklist format."


# Make first rows of eBird checklist format



# set up data frame. MOVE THIS DOWN

col_a <- c("", "Latitude", "Longitude", "Date", "Start Time", "State",
           "Country", "Protocol", "Num Observers", "Duration (min)",
           "All Obs Reported (Y/N)", "Dist Traveled (miles)",
           "Area Covered (Acres)", "Notes")
col_b <- c(rep("",14))
col_c <- c(
  location_name,
  latitude,
  longitude,
  format(vebird$date[1], "%m/%d/%y"), # this needs to get converted to proper format.Change for each date
  format(vebird$start_time[1], "%H:%M"), # use either first_dusk or hour
  state,
  country,
  protocol,
  num_observers,
  duration, # duration is 60 minutes unless first recording or final recording.
  all_obs_reported,
  dist_traveled,
  area_covered,
  notes
)

tibble(col_a, col_b, col_c)

write_csv(
  tibble(col_a, col_b, col_c), 
  "for_ebird_import.csv",
  col_names = FALSE)






vebird |> group_by(group_date, start_time) |> 
  summarise(N = n())
