
library(leaflet)
library(leaflet.extras)
library(glue)

# map scootloops from 'get scootloops data'

mytitle <- glue("<b>Location of GM scootloops</b>")

leaflet(scoot_data2) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  #addResetMapButton %>%
  addCircleMarkers(radius = 3, color = "red", 
                   popup = ~glue("Last updated: {format(last_updated, '%d/%m/%y')} <br> ID: {id} <br> Desription: {description} <br> (start of scoot)")
  ) %>%
  addControl(mytitle, position = "topright")
