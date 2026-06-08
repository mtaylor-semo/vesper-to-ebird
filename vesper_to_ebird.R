# Script to convert metadata csv output from Vesper into eBird checklist format
# to upload NFC counts.   Inspired by Richard Littauer's Vesper to eBird
# javascripts: https://github.com/RichardLitt/vesper-to-ebird


# Can I make this run from command line? If so, how to pass options

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(suncalc)


# Constants ---------------------------------------------------------------

station <- "Miller Reserve"
site <- "prairie"
recorder <- "SMmini"
mic <- "stub"
station_info <- ""
latitude <- 37.132025
longitude <- -89.461307
  
time_zone <- "Etc/GMT+6"

dt_format = "%m/%d/%y %H:%M:%S"

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
  "output.csv",
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


# Time Buckets ------------------------------------------------------------

# THIS NEEDS TO BE A FUNCTION
# Time provided by Vesper is local time, so CDT during migration.
# Convert to CST by subtracting one hour

# Dusk is start of astronomical dusk (end of nautical dusk),
# the appropriate starting time for eBird NFC

dusk <- vebird$nautical_dusk[1]
dusk <- dusk - hours(1)

# dawn is end of astronomical dawn (start of nautical dawn),
# the appropriate end time for eBird NFC
dawn <- vebird$nautical_dawn[1]
dawn <- dawn - hours(1)

# Create buckets based on hours. First dusk and last dawn are the first and
# final buckets at the dusk/dawn boundary.
first_dusk <- ceiling_date(dusk, "hour")
last_dawn <- floor_date(dawn, "hour")
hour_buckets <- seq(first_dusk, last_dawn, by = "hours")


