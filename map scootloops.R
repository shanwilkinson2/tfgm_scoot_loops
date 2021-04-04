
library(leaflet)
library(leaflet.extras)
library(glue)

# map scootloops from 'get scootloops data'
 
leaflet(scoot_loc2) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  #addResetMapButton %>%
  addCircleMarkers(radius = 3, color = "blue", 
                   popup = ~glue("Last updated: {format(last_updated, '%d/%m/%y')} <br> ID: {id} <br> {scn} <br> Desription: {description} <br> (end of scoot)")
  ) %>%
  addControl(glue("<b>Location of GM scootloops</b>"), 
             position = "topright")

## map chorley new rd scoots

# chorley_new_title <- glue("<b>Scootloops near Chorley New Road</b>")

leaflet() %>%
  addProviderTiles("Stamen.TonerLite") %>%
  #addResetMapButton %>%
  addCircleMarkers(data = chorley_new_scoots_start, radius = 3, color = "green", 
                   popup = ~glue("Last updated: {format(last_updated, '%d/%m/%y')} <br> ID: {id} <br> Desription: {description} <br> (start of scoot)")
  ) %>%
  addCircleMarkers(data = chorley_new_scoots_end, radius = 3, color = "red", 
                   popup = ~glue("Last updated: {format(last_updated, '%d/%m/%y')} <br> ID: {id} <br> Desription: {description} <br> (end of scoot)")
  ) %>%
  addControl(glue("<b>Scootloops near Chorley New Road</b>"), 
             position = "topright")
