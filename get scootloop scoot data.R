library(dplyr)
library(httr)
library(jsonlite)
library(sf)
library(lubridate)

source("API key.R")

# login
headers <- add_headers("Ocp-Apim-Subscription-Key" = mykey)

# chorley new scoots
chorley_new <- c(2338, 2335, 2334, 2336) #2337 seems to have been discontinued


#2338 = bottom of chorley new
# won't work with spaces in web address, replace with +
# eq is equals
endpoint <- "https://api.tfgm.com/odata/ScootLoops?$expand=ScootDetails&$filter=Id+eq+2338&$top=10"
endpoint <- "https://api.tfgm.com/odata/ScootLoops?$expand=ScootDetails&$filter=Id+eq+2338+Or+Id+eq+2335&$top=10"
endpoint <- "https://api.tfgm.com/odata/ScootLoops?$expand=ScootDetails&$filter=startswith(SCN,'N53211')&$top=10"
endpoint <- "https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation,ScootDetails&$filter=startswith(SCN,'N53211')"
# endpoint <- glue::glue("https://api.tfgm.com/odata/ScootLoops?$expand=ScootDetails&$filter=Id{paste0(chorley_new, collapse = 'Or')})")

response <- httr::GET(
  url = endpoint,
  config = headers)

# parse as text
scoot_dat <- fromJSON(content(response, "text"))$value %>%
  # get geography
  mutate(start_location_point = StartLocation$LocationSpatial$Geography$WellKnownText,
         end_location_point = EndLocation$LocationSpatial$Geography$WellKnownText,
         scoot_linestring = paste0(
           "LINESTRING (",
           stringr::str_sub(start_location_point, 8,-2),
           ", ",
           stringr::str_sub(end_location_point, 8,-2),
           ")"
           )
         ) %>%
  select(-c(Id, SCN, StartLocationId, EndLocationId, StartLocation, EndLocation)) %>%
  # select(ScootDetails) %>%
  tidyr::unnest(cols = ScootDetails) %>%
  mutate(LastUpdated = as.POSIXct(LastUpdated, format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
  rename(average_speed_kmph = AverageSpeed) %>%
  mutate(average_speed_mph = round(average_speed_kmph * 0.62137119223733, 0)) %>%
  # well known text ie common text format for points
  # works with col num but not name. needs missings deleted & unnesting
  filter(!is.na(start_location_point)) %>%
  st_as_sf(wkt = 11, crs = 4326) # lat/ long

scoot_dat_linestring <- scoot_dat %>%
  st_drop_geometry() %>%
  select(-end_location_point) %>%
  # well known text ie common text format for points
  # works with col num but not name. needs missings deleted & unnesting
  st_as_sf(wkt = 11, crs = 4326) # lat/ long

# volume & speed is average over 5 mins (in Hull at least)
# speed in kmph
