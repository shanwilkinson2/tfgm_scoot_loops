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
# endpoint <- glue::glue("https://api.tfgm.com/odata/ScootLoops?$expand=ScootDetails&$filter=Id{paste0(chorley_new, collapse = 'Or')})")

response <- httr::GET(
  url = endpoint,
  config = headers)

# parse as text
scoot_dat <- fromJSON(content(response, "text"))$value %>%
  select(-c(Id, SCN, StartLocationId, EndLocationId)) %>%
  # select(ScootDetails) %>%
  tidyr::unnest(cols = ScootDetails) %>%
  rename(average_speed_kmph = AverageSpeed) %>%
  mutate(average_speed_mph = round(average_speed_kmph * 0.62137119223733, 0))

# volume & speed is average over 5 mins (in Hull at least)
# speed in kmph
