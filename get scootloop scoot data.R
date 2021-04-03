library(dplyr)
library(httr)
library(jsonlite)
library(sf)
library(lubridate)

source("API key.R")

# login
headers <- add_headers("Ocp-Apim-Subscription-Key" = mykey)

#2338 = bottom of chorley new
endpoint <- "https://api.tfgm.com/odata/ScootLoops({2338})"

response <- httr::GET(
  url = endpoint,
  config = headers)

# parse as text
scoot_loc <- fromJSON(content(response, "text"))$value %>%
  janitor::clean_names()