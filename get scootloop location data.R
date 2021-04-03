library(dplyr)
library(httr)
library(jsonlite)
library(sf)
library(lubridate)

source("API key.R")

# login
headers <- add_headers("Ocp-Apim-Subscription-Key" = mykey)


# get scoot location data  

# still need how to get them all or filtered or somehting

# endpoint <- "https://api.tfgm.com/odata/ScootLoops?$expand=EndLocation,StartLocation&$top=100"
endpoint <- "https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation&$top=10"

# get all
endpoint <- "https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation"

response <- httr::GET(
  url = endpoint,
  config = headers)

# parse as text
scoot_data <- fromJSON(content(response, "text"))$value %>%
  janitor::clean_names()

# start & end points are each in thier little spatial data frame
# not got these to read as spatial yet tho

scoot_data2 <- scoot_data %>% 
  mutate(start_location_point = start_location$LocationSpatial$Geography$WellKnownText,
         end_location_point = end_location$LocationSpatial$Geography$WellKnownText) %>%
  select(-c(start_location, end_location)) %>%
  # make date format
  mutate(last_updated = as.POSIXct(last_updated)) %>%
  # missing locations don't seem to be being updated anyway
  filter(!is.na(start_location_point)) %>%
  # well known text ie common text format for points
  # works with col num but not name. needs missings deleted & unnesting
  st_as_sf(wkt = 7, crs = 4326) # lat/ long


