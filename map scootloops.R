
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
