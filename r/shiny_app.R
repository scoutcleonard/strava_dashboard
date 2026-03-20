##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------ STRAVA ACTIVITY DASHBOARD ---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install and load librarian package if needed
if (!require(librarian)) {
  install.packages("librarian")
  library(librarian)
}

# Load packages
shelf(here, 
      ARTofR, 
      ggplot2, 
      shiny,
      rStrava,
      tidyverse,
      leaflet,
      lubridate,
      scales,
      feather,
      googleway,
      leaflet,
      htmlwidgets, 
      rsconnect)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------- DATA PREPARATION --------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Only fetch fresh data locally; use saved CSV on server
if (!file.exists(here("data/strava_data.csv"))) {
  
  source("../../../credentials/strava_credentials.R")
  
  my_data  <- get_activity_list(stoken)
  
  act_data <- compile_activities(my_data) |> 
    
    write_csv(here("data/strava_data.csv"))
  
}

act_data <- read_csv(here("data/strava_data.csv"))

# Select columns of interest
columns_of_interest <- c('distance',
                         'elapsed_time',
                         'elev_high',
                         'elev_low',
                         'moving_time',
                         'start_date',
                         'start_date_local',
                         'type',
                         'map.summary_polyline',
                         'upload_id',
                         'start_latlng1',
                         'start_latlng2',
                         'total_elevation_gain')

activities <- select(act_data, match(columns_of_interest,
                                     names(act_data)))

# Process activities data
activities <- activities  |>  
  mutate(elapsed_time = round(elapsed_time / 60 / 60, digits = 2),
         moving_time = round(moving_time / 60 / 60, digits = 2),
         date = gsub("T.*$", '', start_date) |> 
           as.POSIXct(., format = "%Y-%m-%d"),
         year = year(date),
         month = month(date),
         week = week(date),
         year_month = format(date, "%Y-%m"),
         year_week = paste0(year, "-W", sprintf("%02d", week))) |> 
  rename(latitude = "start_latlng1",
         longitude = "start_latlng2") |> 
  filter(type %in% c("Ride", "Run", "Hike"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- SHINY UI -------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Ranchers&family=Nunito:wght@400;500;600&display=swap"),
    
    tags$style(HTML("
    
      /* ---- palette variables ---- */
      :root {
        --col-navy:   #2d3ea1;
        --col-cream:  #e4e6cc;
        --col-red:    #bd423e;
        --col-sky:    #91c0d9;
        --col-espresso: #3d302f;
      }
      
      /* ---- base ---- */
      body {
        font-family: 'Nunito', sans-serif;
        background-color: var(--col-cream);
        color: var(--col-espresso);
      }
      
      /* ---- title panel ---- */
      .navbar, h2, .shiny-title-panel {
        font-family: 'Ranchers', cursive !important;
      }
      .container-fluid > h2 {
         background-color: var(--col-navy);
         color: var(--col-cream) !important;
         margin: -15px -15px 15px;
         padding: 35px 30px;
         font-size: 70px;   
         font-weight: 400;
       }
      
      /* ---- sidebar ---- */
      .well {
        background-color: var(--col-espresso) !important;
        border: none !important;
        border-radius: 0 !important;
      }
      .well label, .well h4,
      .well .control-label,
      .well select, .well p, .well b {
        color: var(--col-cream) !important;
        font-family: 'Nunito', sans-serif;
      }
      .well h4 {
        font-family: 'Ranchers', cursive !important;
        font-size: 18px;
        font-weight: 400;
      }
      .well hr {
        border-color: rgba(228,230,204,0.25);
      }
      
      /* ---- selects ---- */
      .selectize-input {
        background: rgba(228,230,204,0.12) !important;
        border-color: rgba(228,230,204,0.3) !important;
        color: var(--col-cream) !important;
        font-family: 'Nunito', sans-serif;
      }
      .selectize-dropdown {
        background: var(--col-espresso) !important;
        color: var(--col-cream) !important;
      }
      
      /* ---- leaflet tooltip ---- */
      .leaflet-tooltip {
        font-family: 'Nunito', sans-serif !important;
        font-size: 13px;
        background: var(--col-espresso);
        color: var(--col-cream);
        border: none;
        border-radius: 6px;
        padding: 8px 12px;
      }
      .leaflet-tooltip-left:before,
      .leaflet-tooltip-right:before {
        border-right-color: var(--col-espresso);
        border-left-color: var(--col-espresso);
      }
      
    "))
  ),
  
  titlePanel("Scout's Pastry-Fuelled Miles 🥐"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("time_period", "Select Time Period:",
                  choices = c("All Time", "Year", "Month", "Week"),
                  selected = "All Time"),
      conditionalPanel(condition = "input.time_period == 'Year'",
                       selectInput("selected_year", "Select Year:", 
                                   choices = NULL)),
      conditionalPanel(condition = "input.time_period == 'Month'",
                       selectInput("selected_month", "Select Month:", 
                                   choices = NULL)),
      conditionalPanel(condition = "input.time_period == 'Week'",
                       selectInput("selected_week", "Select Week:", 
                                   choices = NULL)),
      hr(),
      h4("Summary Statistics"),
      uiOutput("summary_stats")
    ),
    mainPanel(
      width = 9,
      div(
        style = paste(
          "border: 2px solid #2d3ea1;",
          "border-radius: 10px;",
          "overflow: hidden;",       
          "margin: 8px;",
          "box-shadow: 4px 4px 0px #3d302f;"
        ),
        leafletOutput("activity_map", height = 620)  
      )
    )
  )
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------ SHINY SERVER ----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {
  
  # Year choices
  observe({
    years <- sort(unique(activities$year), decreasing = TRUE)
    updateSelectInput(session, "selected_year",
                      choices = years,
                      selected = years[1])
  })
  
  # Mnth choices
  observe({
    months <- sort(unique(activities$year_month), decreasing = TRUE)
    month_labels <- format(as.Date(paste0(months, "-01")), "%B %Y")
    names(months) <- month_labels
    updateSelectInput(session, "selected_month",
                      choices = months,
                      selected = months[1])
  })
  
  # Week choices
  observe({
    weeks <- sort(unique(activities$year_week), decreasing = TRUE)
    updateSelectInput(session, "selected_week",
                      choices = weeks,
                      selected = weeks[1])
  })
  
  # Filter activities based on selection
  filtered_activities <- reactive({
    data <- activities
    
    if (input$time_period == "Year") {
      data <- data |> filter(year == input$selected_year)
    } else if (input$time_period == "Month") {
      data <- data |> filter(year_month == input$selected_month)
    } else if (input$time_period == "Week") {
      data <- data |> filter(year_week == input$selected_week)
    } 
    
    return(data)
  })
  
  # Summary statistics
  output$summary_stats <- renderUI({
    data <- filtered_activities()
    
    total_activities <- nrow(data)
    total_distance <- round(sum(data$distance, na.rm = TRUE), 1)
    total_time <- round(sum(data$elapsed_time, na.rm = TRUE), 1)
    total_elevation <- round(sum(data$total_elevation_gain, na.rm = TRUE), 0)
    
    rides <- sum(data$type == "Ride")
    runs <- sum(data$type == "Run")
    hikes <- sum(data$type == "Hike")
    
    HTML(paste0(
      "<p><b>Total Activities:</b> ", total_activities, "</p>",
      "<p><b>Total Distance:</b> ", total_distance, " miles</p>",
      "<p><b>Total Time:</b> ", total_time, " hours</p>",
      "<p><b>Total Elevation:</b> ", format(total_elevation, big.mark = ","), " feet</p>",
      "<hr>",
      "<p><b>Rides:</b> ", rides, "</p>",
      "<p><b>Runs:</b> ", runs, "</p>",
      "<p><b>Hikes:</b> ", hikes, "</p>"
    ))
  })
  
  # Create the map
  output$activity_map <- renderLeaflet({
    data <- filtered_activities()
    
    # Map bounds
    lons.range <- c(-123, -121)
    lats.range <- c(37, 38.7)
    
    # Create blank map
    map <- leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles('CartoDB.Positron',
                       options = providerTileOptions(noWrap = TRUE,
                                                     minZoom = 7,
                                                     maxZoom = 15)) |>
      fitBounds(lng1 = min(lons.range),
                lat1 = max(lats.range),
                lng2 = max(lons.range),
                lat2 = min(lats.range)) |> 
      addLegend(colors = c("#bd423e", "#91c0d9", "#2d3ea1"),
                labels = c("Ride", "Run", "Hike"),
                position = "bottomright")
    
    # Add activities to map
    if (nrow(data) > 0) {
      unique_activities <- unique(data$upload_id)
      
      for (i in unique_activities) {
        # Get activity
        activity <- filter(data, upload_id == i)
        
        # Skip if no polyline
        if (is.na(activity$map.summary_polyline) || activity$map.summary_polyline == "") {
          next
        }
        
        # Decode polyline
        coords <- decode_pl(activity$map.summary_polyline)
        
        # Create labels
        labs <- paste0('<p>',
                       '<b>Activity Date: </b>',
                       format(activity$date, "%Y-%m-%d"),
                       '<p></p>',
                       '<b>Distance (Miles): </b>',
                       round(activity$distance, 2),
                       '<p></p>',
                       '<b>Time (Hours): </b>',
                       activity$elapsed_time,
                       '<p></p>',
                       '<b>Elevation Gain (Feet): </b>',
                       round(activity$total_elevation_gain, 0),
                       '<p>') |> 
          htmltools::HTML()
        
        # Add polyline with appropriate color
        if (activity$type == "Ride") {
          map <- addPolylines(map,
                              lng = coords$lon,
                              lat = coords$lat,
                              color = "#bd423e",
                              weight = 2,
                              opacity = 1/2,
                              label = labs,
                              labelOptions = labelOptions(
                                style = list("font-family" = "serif",
                                             "font-style" = "bold",
                                             "box-shadow" = "3px 3px rgba(0, 0, 0, 0.25)")))
        } else if (activity$type == "Run") {
          map <- addPolylines(map,
                              lng = coords$lon,
                              lat = coords$lat,
                              color = "#91c0d9",
                              weight = 2,
                              opacity = 1/2,
                              label = labs,
                              labelOptions = labelOptions(
                                style = list("font-family" = "serif",
                                             "font-style" = "bold",
                                             "box-shadow" = "3px 3px rgba(0, 0, 0, 0.25)")))
        } else if (activity$type == "Hike") {
          map <- addPolylines(map,
                              lng = coords$lon,
                              lat = coords$lat,
                              color = "#2d3ea1",
                              weight = 2,
                              opacity = 1/2,
                              label = labs,
                              labelOptions = labelOptions(
                                style = list("font-family" = "serif",
                                             "font-style" = "bold",
                                             "box-shadow" = "3px 3px rgba(0, 0, 0, 0.25)")))
        }
      }
    }
    
    map
  })
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- RUN APP --------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shinyApp(ui = ui, server = server)
