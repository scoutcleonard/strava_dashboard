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
  
  output$activity_table <- DT::renderDataTable({
    filtered_activities() |>
      select(name, 
             type, 
             date, 
             distance, 
             elapsed_time,
             total_elevation_gain, 
             average_speed, 
             average_heartrate) |>
      mutate(date = format(date, "%Y-%m-%d"),
             distance = round(distance, 1),
             average_speed = round(average_speed, 2),
             average_heartrate = round(average_heartrate, 0)) |>
      rename(`Activity Name`= name,
             Type = type,
             Date = date,
             `Distance (miles)` = distance,
             `Time (hours)`= elapsed_time,
             `Elevation (ft)` = total_elevation_gain,
             `Average Speed` = average_speed,
             `Avg Heartrate`= average_heartrate)}, 
    options = list(pageLength = 15,
                   order = list(list(2, "desc")), # start with most recent activities
                   dom = "frtip"), # search box + table + pagination, no extra buttons
  rownames = FALSE
  )
}