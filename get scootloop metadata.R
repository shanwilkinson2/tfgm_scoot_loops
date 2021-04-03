library(dplyr)
library(httr)
library(jsonlite)
library(sf)
library(lubridate)

library(xml2)
library(XML)
library(purrr)

source("API key.R")

# login
  headers <- add_headers("Ocp-Apim-Subscription-Key" = mykey)

# get scoot metadata 

endpoint <- "https://api.tfgm.com/odata/$metadata"

response <- httr::GET(
  url = endpoint,
  config = headers)

scoot_meta <- 
  # extract as text
  content(response, "text")

scoot_meta2 <- scoot_meta %>% 
  # convert text to xml
  read_xml()

scoot_meta2
  xml_children() %>%
  xml_children() %>%
    [1]


# scoot_meta2 <- xmlParse(scoot_meta)

scoot_meta3 <- xmlToList(scoot_meta)
# 
# 
# 
# scoot_meta3 <- scoot_meta2 %>%
#   xmlTreeParse(asText = TRUE, useInternalNodes = TRUE)
# 
# name_path <- "EntityType"
# target_path <- "//Target"
# entitytype_path <- "//EntityType"
# schema_path <- "Schema"
# 
# scoot_meta_df <- data.frame(
#   schema = sapply(scoot_meta2[name_path], xmlValue)
# )
# 
# do.call("rbind", xpathApply(scoot_meta3, "//Points/Point", function(x)
#   data.frame(id = as.numeric(xmlAttrs(x)[["id"]]),
#              Tags = c(gsub('"', '', xmlValue(x[["Tags"]])), NA)[[1]],
#              Position = as.numeric(xmlAttrs(x[["Point"]])[["Position"]],
#                                    stringsAsFactors = FALSE)
#              )
#   ))
# 
# scoot_meta3 <- 
#   xml_find_all(scoot_meta2, ".//Points/Point") %>%
#   map_df(function(x) {
#     list(
#       Point=xml_attr(x, "Id"),
#       Tag=xml_find_first(x, ".//Tags") %>% xml_text() %>% gsub('^"|$', "", .),
#       Position=xml_find_first(x, ".//Point") %>% xml_attr("Position") 
#     )
#   })
#  
#   #### example
