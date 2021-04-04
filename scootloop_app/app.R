#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(httr)
library(jsonlite)
library(sf)
library(lubridate)
library(leaflet)
library(leaflet.extras)

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
        mutate(last_updated = as.POSIXct(last_updated)) %>%
        # missing locations don't seem to be being updated anyway
        filter(!is.na(start_location_point)) %>%
        # scn seems to link junctions, but just with a different last letter. 
        # so remove last letter
        mutate(junction = stringr::str_sub(scn, 1, -2)) %>%
        # well known text ie common text format for points
        # works with col num but not name. needs missings deleted & unnesting
        # col 8 = end location. All scoots point towards the junction, so end is at the junction
        st_as_sf(wkt = 8, crs = 4326) # lat/ long

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Scootloops"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        # tabPanel(
                        #     "Sample",
                        #     plotOutput("distPlot")
                        # ),
                        tabPanel("All scoots map",
                                 leafletOutput("scoot_location_plot")
                                 ),
                        tabPanel("About",
                                 p("This data contains details of SCOOT loops on highways in Greater Manchester."),
                                 p("SCOOT is a measure of congestion at traffic signals."),
                                 p("Contains Transport for Greater Manchester data. Contains OS data © Crown copyright and database right 2016."))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # map of scootloop locations   
    output$scoot_location_plot <- renderLeaflet({
        leaflet(scoot_loc2) %>%
            addProviderTiles("Stamen.TonerLite") %>%
            addResetMapButton %>%
            addCircleMarkers(radius = 3, color = "blue", 
                             popup = ~glue("Last updated: {format(last_updated, '%d/%m/%y')}<br>ID: {id}<br>SCN: {scn}<br>Desription: {description} <br> (end of scoot)")
            ) %>%
            addControl(glue("<b>Location of GM scootloops</b><br>Click on points for more detail"), 
                       position = "topright")
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
