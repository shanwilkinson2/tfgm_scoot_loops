
library(leaflet)
library(leaflet.extras)
library(glue)

# map scootloops from 'get scootloops data'

leaflet(scoot_data) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  #addResetMapButton %>%
  addCircleMarkers(radius = 3, color = "red", 
                   popup = ~glue("Last updated: {format(last_updated, '%d/%m/%y')} <br> {description} <br> (start of scoot)")
  )
