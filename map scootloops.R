
library(leaflet)
library(leaflet.extras)
#library(glue)

# map scootloops 
 
leaflet(scoot_loc2) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addResetMapButton %>%
  addCircleMarkers(radius = 3, color = "blue", 
                   popup = ~glue("Last updated: {format(last_updated, '%d/%m/%y')}<br>ID: {id}<br>SCN: {scn}<br>Desription: {description} <br> (end of scoot)")
  ) %>%
  addControl(glue("<b>Location of GM scootloops</b><br>Click on points for more detail"), 
             position = "topright")

## map chorley new rd scoots

# chorley_new_title <- glue("<b>Scootloops near Chorley New Road</b>")

leaflet() %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addResetMapButton %>%
  addCircleMarkers(data = chorley_new_scoots_start, radius = 3, color = "green", 
                   popup = ~glue("Last updated: {format(last_updated, '%d/%m/%y')} <br> ID: {id} <br> Desription: {description} <br> (start of scoot)")
  ) %>%
  addCircleMarkers(data = chorley_new_scoots_end, radius = 3, color = "red", 
                   popup = ~glue("Last updated: {format(last_updated, '%d/%m/%y')} <br> ID: {id} <br> Desription: {description} <br> (end of scoot)")
  ) %>%
  addControl(glue("<b>Scootloops near Chorley New Road</b>"), 
             position = "topright")


# map selected scootloops
leaflet() %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addResetMapButton %>%
  addCircleMarkers(data = scoot_dat, radius = 5, color = "red", 
                   popup = ~glue::glue("Scoot ID: {Id}<br>Scoot description: {Description}<br>Congestion percentage: {CongestionPercentage}<br>Current flow: {CurrentFlow}<br>Average speed: {average_speed_mph}mph<br>Link status: {LinkStatus}<br>Link travel time: {LinkTravelTime}<br>(start of scoot)")
  ) %>%
  addControl(glue::glue("<b>Selected junction map</b>"), 
             position = "topright")

# trying to get the changing colours to work

selected_var <- "CurrentFlow"

map_pal <-   colorNumeric(palette = "YlOrRd",
               domain = c(
                 min(st_drop_geometry(scoot_dat[selected_var]), na.rm = TRUE),
                 max(st_drop_geometry(scoot_dat[selected_var]), na.rm = TRUE)
               ),
               na.color = "grey"
)

# selected junction map
  leaflet() %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addResetMapButton %>%
    addCircleMarkers(data = scoot_dat, radius = 8, 
                     fillColor = ~map_pal(.data[[selected_var]]) #input$select_indicator
                     
                     , weight = 2,
                     fillOpacity = 0.8, color = "black", 
                     popup = ~glue::glue("Scoot ID: {Id}<br>Scoot description: {Description}<br>Congestion percentage: {CongestionPercentage}<br>Current flow: {CurrentFlow}<br>Adjusted average speed: {average_speed_mph} mph<br>Link status: {LinkStatus}<br>Link travel time: {LinkTravelTime} secs<br>(start of scoot)")
    ) %>%
    addPolylines(data = scoot_dat_linestring, color = ~map_pal(CurrentFlow),
                 popup = ~glue::glue("Scoot ID: {Id}<br>Scoot description: {Description}<br>Congestion percentage: {CongestionPercentage}<br>Current flow: {CurrentFlow}<br>Adjusted average speed: {average_speed_mph} mph<br>Link status: {LinkStatus}<br>Link travel time: {LinkTravelTime} secs<br>(length of scoot)")
    ) %>%
    addControl(glue::glue("<b>Selected junction map</b><br>Click on points for more detail"), 
               position = "topright")
  
  # use crosstalk to select junction ###########################
  
  # get data for all junctions
  
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(httr)
  library(jsonlite)
  library(sf)
  library(lubridate)
  library(leaflet)
  library(leaflet.extras)
  
  setwd("C:/Users/shan_/OneDrive/Documents/tfgm_scoot_loops/scootloop_app")
  
  source("api_key.R")
  
  # api login details in headers
  # login
  headers <- add_headers("Ocp-Apim-Subscription-Key" = mykey)
  
  # get all scootloop locations
  response <- httr::GET(
    url = "https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation",
    config = headers)
  
  # parse as text
  scoot_loc2 <- fromJSON(content(response, "text"))$value %>%
    janitor::clean_names() %>%
    # start & end points are each in thier little spatial data frame
    mutate(start_location_point = start_location$LocationSpatial$Geography$WellKnownText,
           end_location_point = end_location$LocationSpatial$Geography$WellKnownText) %>%
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

  # get list of unique junctions (from SCN minus last character)  
  all_junctions <- scoot_loc2 %>% 
    # drop geometry
    st_drop_geometry() %>%
    # last updated seems to be a system change rather than live detector so leave all in 
    select(junction) %>%
    arrange(junction) %>%
    unique()    

  library(crosstalk)
  
  shared_data <- SharedData$new(scoot_loc2, key = scn)  