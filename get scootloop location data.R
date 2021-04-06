library(dplyr)
library(httr)
library(jsonlite)
library(sf)
library(lubridate)

source("API key.R")

# time in GMT (even when in BST)

# login
headers <- add_headers("Ocp-Apim-Subscription-Key" = mykey)


# get scoot location data  

# endpoint <- "https://api.tfgm.com/odata/ScootLoops?$expand=EndLocation,StartLocation&$top=100"
# endpoint <- "https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation&$top=10"

# get all
endpoint <- "https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation"

response <- httr::GET(
  url = endpoint,
  config = headers)

# parse as text
scoot_loc <- fromJSON(content(response, "text"))$value %>%
  janitor::clean_names()

# start & end points are each in thier little spatial data frame
scoot_loc2 <- scoot_loc %>% 
  mutate(start_location_point = start_location$LocationSpatial$Geography$WellKnownText,
         end_location_point = end_location$LocationSpatial$Geography$WellKnownText) %>%
  mutate(scoot_linestring = paste0(
    "LINESTRING (",
    stringr::str_sub(start_location_point, 8,-2),
    ", ",
    stringr::str_sub(end_location_point, 8,-2),
    ")"
    )
  ) %>%
  select(-c(start_location, end_location)) %>%
  # make date format
  mutate(last_updated = as.POSIXct(last_updated, format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
  # missing locations don't seem to be being updated anyway
  filter(!is.na(start_location_point)) %>%
  # scn seems to link junctions, but just with a different last letter. 
  # so remove last letter
  mutate(junction = stringr::str_sub(scn, 1, -2)) %>%
  # well known text ie common text format for points
  # works with col num but not name. needs missings deleted & unnesting
  # col 8 = end location. All scoots point towards the junction, so end is at the junction
  st_as_sf(wkt = 8, crs = 4326) # lat/ long

# chorley new scoots
    chorley_new <- c(2338, 2335, 2334, 2336) #2337 seems to have been discontinued
    
    chorley_new_scoots_end <- scoot_loc2 %>%
      filter(id %in% chorley_new)
    
    chorley_new_scoots_start <- chorley_new_scoots_end %>%
      st_drop_geometry() %>%
      # well known text ie common text format for points
      # works with col num but not name. needs missings deleted & unnesting
      st_as_sf(wkt = 7, crs = 4326) # lat/ long

# get list of unique junctions (from SCN minus last character)  
    all_junctions <- scoot_loc2 %>% 
      # drop geometry
      st_drop_geometry() %>%
      # filter out any that may not be being currently updated 
      # ie at least in last fortnight
      filter(last_updated >= max(last_updated - lubridate::weeks(2))) %>%
      select(junction) %>%
      unique()
    

# how many scoots does each juntion have?
  scoot_loc2 %>%
    st_drop_geometry() %>%
    # only scoots that have been updated in the last 2 weeks
    filter(last_updated >= max(last_updated - lubridate::weeks(2))) %>%
    group_by(junction) %>%
    summarise(num_scoots = n()) %>%
    ungroup() %>%
    summarise(   min_scoots = min(num_scoots), 
              max_scoots = max(num_scoots),
              median_scoots = median(num_scoots),
              mean_scoots = mean(num_scoots),
              cenile_25_scoots = quantile(num_scoots, 0.25),
              cenile_75_scoots = quantile(num_scoots, 0.75)
              )
