
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
  
  # chorley new scoots
  chorley_new <- c("N53211", "N53141", "N53222") #2337 seems to have been discontinued
  chorley_new <- c("N53211")
  myfilter <- paste0("startswith(SCN,'", 
    paste0(chorley_new,
    collapse = "')Or+startswith(SCN,'"
    ),
    "')")
  endpoint <- paste0("https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation,ScootDetails&$filter=", myfilter)
  
 # selected_jct_data <- (#reactive({
    # # endpoint for selected junction
    # endpoint <- glue::glue("https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation,ScootDetails&$filter=startswith(SCN,'{chorley_new_pasted}')")
    # # endpoint for multiple junctions
    # #endpoint <- glue::glue("https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation,ScootDetails&$filter=startswith(SCN,'{paste0(input$select_junction, collapse = 'Or')}')") 
    # # pull data
    response <- httr::GET(url = endpoint, config = headers)
    # process data
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
      # remove these cols as have already dealt with them above
      select(-c(Id, SCN, StartLocationId, EndLocationId, 
                StartLocation, EndLocation)) %>%
      tidyr::unnest(cols = ScootDetails) %>%
      mutate(LastUpdated = as.POSIXct(LastUpdated, format = "%Y-%m-%dT%H:%M:%OSZ"),
             scoot_letter = stringr::str_sub(SCN, -1, nchar(SCN))
      ) %>%
      rename(average_speed_kmph = AverageSpeed) %>%
      mutate(average_speed_mph = round(average_speed_kmph * 0.62137119223733, 0),
             # when link travel time is 0 (ie arm has all red phase because of no vehicles)
             # average speed defaults to 50mph. Create adjusted average speed to sort this out
             adjusted_average_speed_mph = ifelse(LinkTravelTime == 0, NA, average_speed_mph),
             # when flow is low seems to default to 50mph
             adjusted_average_speed_mph = ifelse(adjusted_average_speed_mph ==50 & CurrentFlow <5, NA, adjusted_average_speed_mph)
      ) %>%
      # well known text ie common text format for points
      # works with col num but not name. needs missings deleted & unnesting
      filter(!is.na(start_location_point)) %>%
      # order by scoot letter
      arrange(scoot_letter) %>%
      st_as_sf(wkt = 11, crs = 4326) # lat/ long
  #})
  
      selected_junction_mapdata <- scoot_dat %>%
    tidyr::gather(key = "indicator", value = "value",
                  c(CongestionPercentage, CurrentFlow,
                    adjusted_average_speed_mph, LinkStatus, LinkTravelTime)) %>%
      # filter just to selected indicator
      filter(indicator == "adjusted_average_speed_mph")
    