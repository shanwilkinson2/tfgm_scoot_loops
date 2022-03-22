
library(shiny)
library(shinydashboard)
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
         # start & end points are each in their little spatial data frame
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
        
    # aggregate by juntion & get centroid
        # so only one point per junction
        scoot_loc3 <- scoot_loc2 %>%
            aggregate(., by = list(scoot_loc2$junction), function(x) x = x[1]) %>% 
            st_centroid() %>% 
            select(-c(Group.1: description, start_location_id:start_location_point))
        scoot_loc3 <- scoot_loc3 %>%
            mutate(rownum = seq.int(nrow(scoot_loc3)))
        
        # get list of unique junctions (from SCN minus last character)  
        all_junctions <- scoot_loc2 %>% 
            # drop geometry
            st_drop_geometry() %>%
            # last updated seems to be a system change rather than live detector so leave all in 
            select(junction) %>%
            arrange(junction) %>%
            unique()    
        
        indicator_names <- data.frame(
            varname = c("CurrentFlow", "adjusted_average_speed_mph", "CongestionPercentage",
                        "LinkTravelTime", "LinkStatus", "jct_flow_hr"),
            prettyname = c("Current flow", "Adjusted average speed", "Congestion percentage", 
                           "Link travel time", "Link status", "Total junction flow"),
            suffix = c(" passenger car units (in 5 mins)", " mph (over 5 mins)", "%",
                       "secs", "", " PCU per hour equivalent")
            )
           
        # nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
        #     st_transform(4326)
        
###############################
           
# Define UI for app
ui <- dashboardPage(skin = "purple",

    # Application title
    dashboardHeader(title = "Scootloops"),

    # Sidebar  
        dashboardSidebar(
            # tab selector menu
            sidebarMenu(
                menuItem("All junctions map", tabName = "all_map"),
                menuItem("Selected junction map", tabName = "selected_map"),
                menuItem("Selected junction table", tabName = "selected_table"),
                menuItem("About", tabName = "about_app")
                ),
            h5("'Selected' tabs will display info for selected junction only"),
            selectInput(
                inputId = "select_indicator",
                label = "Select indicator to colour selected junction by",
                choices = c("Current flow" = "CurrentFlow", 
                            "Adjusted average speed" = "adjusted_average_speed_mph", 
                            "Congestion percentage" = "CongestionPercentage",
                            "Link travel time" = "LinkTravelTime",
                            "Link status" = "LinkStatus",
                            "Total junction flow" = "jct_flow_hr"
                            ),
                selectize = TRUE
            ),
            selectizeInput(inputId = "selected_jct",
                           label = "Selected junctions",
                           choices = all_junctions,
                           selected = NULL,
                           multiple = TRUE)
         ),

        # 
        dashboardBody(
           tabItems(
                tabItem(tabName = "all_map",
                        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                        leafletOutput("scoot_location_plot2"),
                        p("More information about scoots in selected junctions will show in the other tabs.")
                         ),
                tabItem(tabName = "selected_map",
                        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                        leafletOutput("selected_jct_map"),
                         h4("All scoots run towards the junction."),
                         p("When link travel time is 0, ie arm is all red phase because of no vehicles, average speed defaults to 50mph. Adjusted average speed shows this as NA instead."),
                         p("Speed seems to sometimes default to 50mph when flows are low."),
                         p("Congestion percentage = congestion is identified when a detector placed where the end of a normal queue at red would be has been continuously occupied for 4 secs."),
                         p("Once this has occurred, congestion percentage is calculated using: num secs detector occupied in the cycle * 100 / cycle time in secs"),
                         p("1000 Passenger Car Units (PCU) per hour at peak is used as a threshold for perception of safety when cycling, in the GM healthy streets design checklist. 
                             This is equivalent to 84+ PCU's per 5 mins. Above this threshold a protected lane must be provided. This could also be useful as a wider indicator of comfort.
                             Stronger purple colours indicate values increasingly higher than 1000, while stronger green colours indicate values increasingly lower than 1000."),
                         ),
                tabItem(tabName = "selected_table",
                    downloadButton("selected_jct_data_download", "Get the data (csv)"),
                    DT::DTOutput("selected_jct_table"),
                    DT::DTOutput("selected_jct_table_jct_flow"),
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
                tabItem(tabName = "about_app",
                        h2("What is SCOOT?"),
                        p("SCOOT loops are installed at traffic signals, and are used to measure various elements of traffic flow and adjust signal timings in real time to reduce traffic congestion.
                        This app uses data from SCOOT loops on highways in Greater Manchester."),
                         p("SCOOT stands for 'Split Cycle Offset Optimisation Technique'. 
                           The system uses data from traffic sensors to automatically adjust the traffic signal timings across a network of nearby signals to adapt to current traffic conditions.
                           It aims to minimise delay, stops, and reduce congestion.").
                         p("A SCOOT network is divided into 'regions', each containing a number of 'nodes' (signallised junctions and crossings) 
                            which are all run at the same cycle time (or a multiplier such as half or double) to allow co-ordination. 
                            The SCOOT software adjusts the duration of each green and red phase within the cycle (the split optimiser); 
                            the overall duration of the cycle (the cycle optimiser);
                            and the amount of time the phases are offset from those on other nearby traffic signals (the offset optimiser)."),
                         p("This has the effect of providing more green on the busier approaches when required, 
                            but also taking into account the effect this has on the surrounding traffic signal network, so traffic moving through on green doesn't 
                            block the junciton but joins another moving queue, so maximising junction capacity."),
                        p("Many transport authorities use SCOOT or other systems on their traffic signals."),
                        br(),
                        h2("About this app"),
                        p("Not all signal controlled junctions in Greater Manchester use SCOOT, this app displays information from those that do."),
                        br(),
                        h2("About the data"),
                        p("Contains Transport for Greater Manchester data. Contains OS data © Crown copyright and database right 2016."),
                         a("TfGM", href = "https://developer.tfgm.com/developer", target = "_blank"),
                         p("Code for this app is on my github."),
                         a("Github", href = "https://github.com/shanwilkinson2/tfgm_scoot_loops", target = "_blank")
                         )
            )
        )
    )
#)

# Define server logic 
server <- function(input, output, session) {

# sort out stuff for click select/ deselect on all map or dropdown
    
    #define leaflet proxy for selected junction map
    proxy_all_jct <- leafletProxy("scoot_location_plot2")
    
    #create empty vector to hold all click ids
    selected2 <- reactiveValues(groups = vector())
    
    # observe event
    # click map to select/ deselect
    observeEvent(input$scoot_location_plot2_marker_click, {
        # if clicked isn't already added, add to selected$groups
        if(input$scoot_location_plot2_marker_click$group == "unselected"){
            selected2$groups <- c(selected2$groups, input$scoot_location_plot2_marker_click$id)
            proxy_all_jct %>% showGroup(group = input$scoot_location_plot2_marker_click$id)
        } else {
            # if clicked is already added, find the element that haven't just been clicked
            # setdiff(x, y) - returns elements in x but not in y
            selected2$groups <- setdiff(selected2$groups, 
                                        input$scoot_location_plot2_marker_click$group
                                        )
            proxy_all_jct %>% hideGroup(group = input$scoot_location_plot2_marker_click$group)
        }
        updateSelectizeInput(session,
                             inputId = "selected_jct",
                             label = "Selected junctions",
                             choices = all_junctions,
                             selected = selected2$groups)
    })

    # observe event 
    # coordinate select via map or dropdown
    observeEvent(input$selected_jct, {
        removed_via_selectInput <- setdiff(selected2$groups, input$selected_jct)
        added_via_selectInput <- setdiff(input$selected_jct, selected2$groups)
        
        if(length(removed_via_selectInput) > 0){
            selected2$groups <- input$selected_jct # changed selected to selected2 - see if this solves the crashing when deselect on dropdown
            proxy_all_jct %>% hideGroup(group = removed_via_selectInput)
        }
        
        if(length(added_via_selectInput) > 0){
            selected2$groups <- input$selected_jct
            proxy_all_jct %>% showGroup(group = added_via_selectInput)
        }
    }, ignoreNULL = FALSE)
    
    # map of all scoot juctions  
    output$scoot_location_plot2 <- renderLeaflet({
        
        mylabels <- as.list(glue::glue("Junction number: {scoot_loc3$junction}<br>Last updated: {format(scoot_loc3$last_updated, '%d/%m/%y')}"))
        
        leaflet(scoot_loc3) %>%
            addProviderTiles("Stamen.TonerLite") %>%
            addResetMapButton %>%
            # unselected points
            addCircleMarkers(radius = 4, fillColor = "blue", 
                             fillOpacity = 0.8,
                             weight = 2, color = "black",
                             label = lapply(mylabels, HTML),
                             group = "unselected", 
                             layerId = ~junction
            ) %>%
            # selected points
            addCircleMarkers(radius = 6, fillColor = "red", 
                             fillOpacity = 0.8,
                             weight = 2, color = "black",
                             label = lapply(mylabels, HTML),
                             group = ~junction, 
                             layerId = ~rownum
            ) %>%
            hideGroup(group = scoot_loc3$junction) %>%
            addControl(glue::glue("<b>Location of GM scoot junctions</b><br>Click to select/ deselect"), 
                       position = "topright")
    })
    
    ################## other stuff #
    
    # selected junction map
    output$selected_jct_map <- renderLeaflet({
        leaflet() %>%
        addProviderTiles("Stamen.TonerLite") %>%
            addResetMapButton %>%
            addCircleMarkers(data = selected_jct_mapdata(), 
                             radius = 8, 
                             fillColor = ~map_pal()(value), 
                             weight = 2,
                             fillOpacity = 0.8, color = "black", 
                             popup = ~glue::glue("Junction: {junction}<br>Scoot letter: {scoot_letter}<br>{indicator_names[indicator_names$varname==input$select_indicator,'prettyname']}: {ifelse(indicator != 'LinkStatus', value, ifelse(value == 0, 'Normal', 'Suspect'))}{indicator_names[indicator_names$varname==input$select_indicator,'suffix']}<br>(start of scoot)")
            ) %>%
            addPolylines(data = selected_jct_linestring(), color = ~map_pal()(value),
                         popup = ~glue::glue("Junction: {junction}<br>Scoot letter: {scoot_letter}<br>{indicator_names[indicator_names$varname==input$select_indicator,'prettyname']}: {ifelse(indicator != 'LinkStatus', value, ifelse(value == 0, 'Normal', 'Suspect'))}{indicator_names[indicator_names$varname==input$select_indicator,'suffix']}<br>(length of scoot)")
            ) %>%
            addControl(glue::glue("<b>Selected junction map</b><br>Click on points for more detail"), 
                       position = "topright") %>%
            addLegend(pal = map_pal(),
                      values = selected_jct_mapdata()$value,
                      position = "bottomleft",
                      title = paste(indicator_names[indicator_names$varname == input$select_indicator, 2],
                                    "<br>",
                                    indicator_names[indicator_names$varname == input$select_indicator, "suffix"])
                      )
    })
    
    # reactive dataset for selected junction
    selected_jct_data <- reactive({
        # endpoint for multiple junctions
        myfilter <- paste0("startswith(SCN,'", 
                           paste0(input$selected_jct,
                               #input$select_junction,
                                  collapse = "')Or+startswith(SCN,'"
                           ),
                           "')")
        endpoint <- paste0("https://api.tfgm.com/odata/ScootLoops?$expand=StartLocation,EndLocation,ScootDetails&$filter=", myfilter)
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
                       ")",
                       # scn seems to link junctions, but just with a different last letter. 
                       # so remove last letter
                       junction = stringr::str_sub(SCN, 1, -2)
                   )
            ) %>%
            # remove these cols as have already extracted above so would have 2
            select(-c(Id, SCN, StartLocationId, EndLocationId, 
                     StartLocation, EndLocation)) %>%
            tidyr::unnest(cols = ScootDetails) %>%
            mutate(LastUpdated = as.POSIXct(LastUpdated, format = "%Y-%m-%dT%H:%M:%OSZ"),
                   scoot_letter = stringr::str_sub(SCN, -1, nchar(SCN)),
                   junction = stringr::str_sub(SCN, 1, -2)
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
            # calculate junction total flow
            group_by(junction) %>%
            mutate(jct_flow = sum(CurrentFlow),
                   jct_flow_hr = jct_flow*12,
                   jct_flow_over1000 = ifelse(jct_flow_hr>1000, TRUE, FALSE),
                   extracted = Sys.time()
                   ) %>%
            # order by scoot letter
            arrange(junction, scoot_letter) %>%
            st_as_sf(wkt = 11, crs = 4326) # lat/ long
    })
    
    # generate data for download button
    output$selected_jct_data_download <- downloadHandler(filename = "scootloop_junction_data.csv",
                                           # create file for downloading
                                           content = function(file){
                                               write.csv(selected_jct_data()
                                                         , file)
                                           })
    
    # pivoted data to colour for selected junction map points
    selected_jct_mapdata <- reactive({
        selected_jct_data() %>%
            # pivot_longer doesn't work on spatial df
            tidyr::gather(key = "indicator", value = "value",
                          c(CongestionPercentage, CurrentFlow,
                            adjusted_average_speed_mph, LinkStatus, 
                            LinkTravelTime, jct_flow_hr)) %>%
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
            select(Junction = junction,
                   "Scoot letter" = scoot_letter, Description,
                   "Congestion percentage" = CongestionPercentage,
                   "Current flow" = CurrentFlow, "Adjusted average speed (mph)" = adjusted_average_speed_mph,
                   "Average speed (mph)" = average_speed_mph, "Link status" = LinkStatus,
                   "Link travel time" = LinkTravelTime
                   )
        },
        rownames = FALSE)

    
    # generate table for selected junction - overall junction flow
    output$selected_jct_table_jct_flow <- DT::renderDT({
        data = selected_jct_data() %>%
            st_drop_geometry() %>%
            group_by(junction) %>%
            slice(1) %>%
            select(Junction = junction,
                   `Total junction flow` = jct_flow,
                   `Hourly flow` = jct_flow_hr,
                   `Hourly flow over 1000?` = jct_flow_over1000
            )
    },
    rownames = FALSE)
    
    # palette for selected junction map 
    map_pal <- reactive({if(input$select_indicator == "jct_flow_hr"){
        # diverging palette for how much over/ under 1000 PCU per day
        
        # if all values <= 1000
        if(max(selected_jct_mapdata()$value, na.rm = TRUE)<=1000){
            colorNumeric(
                palette = colorRampPalette(colors = c("darkgreen", "white"), space = "Lab")(1000),
                domain = c(0, 1000),
                na.color = "grey"
            )
        } else
        
        # if all values are>1000
            if(min(selected_jct_mapdata()$value, na.rm = TRUE)>1000){
                colorNumeric(
                    palette = colorRampPalette(colors = c("white", "darkslateblue"), space = "Lab")(max(selected_jct_mapdata()$value, na.rm = TRUE)-1000),
                    domain = c(1000, 
                               max(selected_jct_mapdata()$value, na.rm = TRUE)
                               ),
                    na.color = "grey"
                )
            } 
        
        else { # if values are a mix of above & below 1000
            colorNumeric(
                palette = c(
                    colorRampPalette(colors = c("darkgreen", "white"), space = "Lab")(1000),
                    colorRampPalette(colors = c("white", "darkslateblue"), space = "Lab")(max(selected_jct_mapdata()$value, na.rm = TRUE)-1000)
                ),
                domain = c(0, max(selected_jct_mapdata()$value, na.rm = TRUE)),
                na.color = "grey"       
            )
        }
    } else{
        colorNumeric(palette = "YlOrRd",
                     domain = selected_jct_mapdata()$value,
                     na.color = "grey"
                    )
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
