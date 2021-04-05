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

# non responsive stuff ##########################

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
    
        # get list of unique junctions (from SCN minus last character)  
        all_junctions <- scoot_loc2 %>% 
            # drop geometry
            st_drop_geometry() %>%
            # filter out any that may not be being currently updated 
            # ie at least in last fortnight
            filter(last_updated >= max(last_updated - lubridate::weeks(2))) %>%
            select(junction) %>%
            arrange(junction) %>%
            unique()    
 
###############################
           
# Define UI for app
ui <- fluidPage(

    # Application title
    titlePanel("Scootloops"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # dropdown. selectize = TRUE lets you type in
            selectInput(
                inputId = "select_junction",
                label = "Select junction:",
                choices = all_junctions,
                selected = "N53211",
                selectize = TRUE
            ),
            em("(Delete selected junction to type & search)"),
            p("Junction numbers on map")
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
                        tabPanel("Selected scoots",
                            DT::DTOutput("selected_jct_table"),
                            h3("Explanation of fields"),
                            p("Id, SCN = id numbers for the scootloop"),
                            p("Description = SCN or a written description of the location"),
                            p("Last updated = date/ time of update. **Time always in GMT - need to sort this.**"),
                            p("Congestion percentage = not totally sure"),
                            p("Current flow = vehicles in 5 mins (I think)"),
                            p("Average speed = average speed in 5 mins (I think). I think the scoots measure in KMPH so converted to MPH as well."),
                            p("Link status = not totally sure"),
                            p("Link travel time = in seconds? Not totally sure.")
                        ),
                        tabPanel("About",
                                 p("This data contains details of SCOOT loops on highways in Greater Manchester."),
                                 p("SCOOT is a measure of congestion at traffic signals."),
                                 p("Contains Transport for Greater Manchester data. Contains OS data © Crown copyright and database right 2016."),
                                 a("TfGM", href = "https://developer.tfgm.com/developer", target = "_blank"),
                                 p("Code for this app is on my github."),
                                 a("Github", href = "https://github.com/shanwilkinson2/tfgm_scoot_loops", target = "_blank")
                                 )
            )
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    # map of scootloop locations   
    output$scoot_location_plot <- renderLeaflet({
        leaflet(scoot_loc2) %>%
            addProviderTiles("Stamen.TonerLite") %>%
            addResetMapButton %>%
            addCircleMarkers(radius = 3, color = "blue", 
                             popup = ~glue::glue("Last updated: {format(last_updated, '%d/%m/%y')}<br>Scoot id: {id}<br>Junction number: {junction}<br>Desription: {description} <br> (end of scoot)")
            ) %>%
            addControl(glue::glue("<b>Location of GM scootloops</b><br>Click on points for more detail"), 
                       position = "topright")
    })
    
    # reactive dataset for seleted junction
    selected_jct_data <- reactive({
        # endpoint for selected junction
        endpoint <- glue::glue("https://api.tfgm.com/odata/ScootLoops?$expand=ScootDetails&$filter=startswith(SCN,'{input$select_junction}')")
        # pull data
        response <- httr::GET(
            url = endpoint,
            config = headers)
        # process data
        # parse as text
        scoot_dat <- fromJSON(content(response, "text"))$value %>%
            select(-c(Id, SCN, StartLocationId, EndLocationId)) %>%
            tidyr::unnest(cols = ScootDetails) %>%
            rename(average_speed_kmph = AverageSpeed) %>%
            mutate(average_speed_mph = round(average_speed_kmph * 0.62137119223733, 0))
    })
    
    # generate table for selected junction
    output$selected_jct_table <- DT::renderDT({
        data = selected_jct_data() %>%
            select(Id, SCN, Description, 
                   "Last updated" = LastUpdated, "Congestion percentage" = CongestionPercentage,
                   "Current flow" = CurrentFlow, "Average speed (kmph)" = average_speed_kmph, 
                   "Average speed (mph)" = average_speed_mph, "Link status" = LinkStatus,
                   "Link travel time" = LinkTravelTime
                   )
        },
        rownames = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
