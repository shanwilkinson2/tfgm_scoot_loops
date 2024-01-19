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

        # names of selected indicator as a vector, so can refer to it   
        # indicator_names <- c("Current flow" = "CurrentFlow", 
        #                      "Adjusted average speed" = "adjusted_average_speed_mph", 
        #                      "Congestion percentage" = "CongestionPercentage",
        #                      "Link travel time" = "LinkTravelTime",
        #                      "Link status" = "LinkStatus"
        # )
        
        indicator_names <- data.frame(
            varname = c("CurrentFlow", "adjusted_average_speed_mph", "CongestionPercentage", "LinkTravelTime", "LinkStatus"),
            prettyname = c("Current flow", "Adjusted average speed", "Congestion percentage", "Link travel time", "Link status"),
            suffix = c(" passenger car units (in 5 mins)", " mph (over 5 mins)", "%", " secs", "")
            )
           
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
            p("Junction numbers on map"),
            h5("'Selected' tabs will display info for selected junction only"),
            selectInput(
                inputId = "select_indicator",
                label = "Select indicator to colour selected junction by",
                choices = c("Current flow" = "CurrentFlow", 
                            "Adjusted average speed" = "adjusted_average_speed_mph", 
                            "Congestion percentage" = "CongestionPercentage",
                            "Link travel time" = "LinkTravelTime",
                            "Link status" = "LinkStatus"
                            ),
                selectize = TRUE
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("All scoots map",
                                 leafletOutput("scoot_location_plot")
                                 ),
                        tabPanel("Selected scoots map",
                                 leafletOutput("selected_jct_map"),
                                 h4("All scoots run towards the junction."),
                                 p("When link travel time is 0, ie arm is all red phase because of no vehicles, average speed defaults to 50mph. Adjusted average speed shows this as NA instead."),
                                 p("Speed seems to sometimes default to 50mph when flows are low."),
                                 p("Congestion percentage = congestion is identified when a detector placed where the end of a normal queue at red would be has been continuously occupied for 4 secs."),
                                 p("Once this has occurred, congestion percentage is calculated using: num secs detector occupied in the cycle * 100 / cycle time in secs")
                                 ),
                        tabPanel("Selected scoots table ",
                                  DT::DTOutput("selected_jct_table"),
                            h3("Explanation of fields"),
                            p("Id, SCN = id numbers for the scootloop"),
                            p("Description = SCN or a written description of the location"),
                            p("Congestion percentage = congestion is identified when a detector placed where the end of a normal queue at red would be has been continuously occupied for 4 secs."),
                            p("Once this has occurred, congestion percentage is calculated using: num secs detector occupied in the cycle * 100 / cycle time in secs"),
                            p("Current flow = vehicles (passenger car units) in 5 mins."),
                            p("Average speed = average speed in 5 mins. Measured in KMPH so converted to MPH."),
                            p("Link status = 0 - normal, 1 - suspect"),
                            p("Link travel time = in seconds"),
                            p("When link travel time is 0, ie arm is running on all red phase because of no vehicles, average speed defaults to 50mph"),
                            p("Looks like there might be some other defaulting to 50mph when flows are low.")
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
            addProviderTiles("Esri.WorldGrayCanvas") %>%
            addResetMapButton %>%
            addCircleMarkers(radius = 3, color = "blue", 
                             popup = ~glue::glue("Last updated: {format(last_updated, '%d/%m/%y')}<br>Scoot id: {id}<br>Junction number: {junction}<br>Desription: {description} <br> (end of scoot)")
            ) %>%
            addControl(glue::glue("<b>Location of GM scootloops</b><br>Click on points for more detail"), 
                       position = "topright")
    })
    
    # selected junction map
    output$selected_jct_map <- renderLeaflet({
        leaflet() %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
            addResetMapButton %>%
            addCircleMarkers(data = selected_jct_mapdata(), radius = 8, 
                             fillColor = ~map_pal()(value), 
                             weight = 2,
                             fillOpacity = 0.8, color = "black", 
                             popup = ~glue::glue("Scoot letter: {scoot_letter}<br>{indicator_names[indicator_names$varname==input$select_indicator,'prettyname']}: {ifelse(indicator != 'LinkStatus', value, ifelse(value == 0, 'Normal', 'Suspect'))}{indicator_names[indicator_names$varname==input$select_indicator,'suffix']}<br>(start of scoot)")
            ) %>%
            addPolylines(data = selected_jct_linestring(), color = ~map_pal()(value),
                         popup = ~glue::glue("Scoot letter: {scoot_letter}<br>{indicator_names[indicator_names$varname==input$select_indicator,'prettyname']}: {ifelse(indicator != 'LinkStatus', value, ifelse(value == 0, 'Normal', 'Suspect'))}{indicator_names[indicator_names$varname==input$select_indicator,'suffix']}<br>(length of scoot)")
            ) %>%
            addControl(glue::glue("<b>Selected junction map</b><br>Click on points for more detail"), 
                       position = "topright")
    })
    
    # reactive dataset for seleted junction
    selected_jct_data <- reactive({
        # endpoint for selected junction
        endpoint <- glue::glue("https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation,ScootDetails&$filter=startswith(SCN,'{input$select_junction}')")
        # pull data
        response <- httr::GET(
            url = endpoint,
            config = headers)
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
            select(-c(Id, SCN, StartLocationId, EndLocationId, StartLocation, EndLocation)) %>%
            # select(ScootDetails) %>%
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
    })
    
    # pivoted data to colour for selected junction map points
    selected_jct_mapdata <- reactive({
        selected_jct_data() %>%
            # pivot_longer doesn't work on spatial df
            tidyr::gather(key = "indicator", value = "value",
                          c(CongestionPercentage, CurrentFlow,
                            adjusted_average_speed_mph, LinkStatus, LinkTravelTime)) %>%
            # filter just to selected indicator
            filter(indicator == input$select_indicator)
    })
    
    # pivoted data to colour for selected junction map lines
    selected_jct_linestring <- reactive({
        selected_jct_mapdata() %>%
            st_drop_geometry() %>%
            select(-end_location_point) %>%
            # well known text ie common text format for points
            # works with col num but not name. needs missings deleted & unnesting
            st_as_sf(wkt = 7, crs = 4326) # lat/ long
    })
    
    # generate table for selected junction
    output$selected_jct_table <- DT::renderDT({
        data = selected_jct_data() %>%
            st_drop_geometry() %>%
            select("Scoot letter" = scoot_letter, Id, SCN, Description,
                   "Congestion percentage" = CongestionPercentage,
                   "Current flow" = CurrentFlow, "Adjusted average speed (mph)" = adjusted_average_speed_mph,
                   "Average speed (mph)" = average_speed_mph, "Link status" = LinkStatus,
                   "Link travel time" = LinkTravelTime
                   )
        },
        rownames = FALSE)

    # palette for selected juntion map 
    map_pal <- reactive({
        colorNumeric(palette = "YlOrRd",
                     domain = c(
                        min(selected_jct_mapdata()$value, na.rm = TRUE),
                        # when all values are 0 to get them to display as low rather than mid
                        ifelse(sum(selected_jct_mapdata()$value ==0),
                               100,
                               max(selected_jct_mapdata()$value, na.rm = TRUE)
                        )
                        ),
                     na.color = "grey"
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
