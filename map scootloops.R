
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
