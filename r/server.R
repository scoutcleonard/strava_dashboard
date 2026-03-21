server <- function(input, output, session) {
  
  # Year choices
  observe({
    years <- sort(unique(activities$year), decreasing = TRUE)
    updateSelectInput(session, "selected_year",
                      choices = years, selected = years[1])
  })
  
  # Month choices
  observe({
    months <- sort(unique(activities$year_month), decreasing = TRUE)
    month_labels <- format(as.Date(paste0(months, "-01")), "%B %Y")
    names(months) <- month_labels
    updateSelectInput(session, "selected_month",
                      choices = months, selected = months[1])
  })
  
  # Week choices
  observe({
    weeks <- sort(unique(activities$year_week), decreasing = TRUE)
    updateSelectInput(session, "selected_week",
                      choices = weeks, selected = weeks[1])
  })
  
  # Filter activities
  filtered_activities <- reactive({
    data <- activities
    
    if (input$time_period == "Year") {
      data <- data |> filter(year == input$selected_year)
    } else if (input$time_period == "Month") {
      data <- data |> filter(year_month == input$selected_month)
    } else if (input$time_period == "Week") {
      data <- data |> filter(year_week == input$selected_week)
    }
    
    data <- data |> filter(type %in% input$selected_types)
    
    return(data)
  })
  
  # Summary statistics
  output$summary_stats <- renderUI({
    data <- filtered_activities()
    
    total_activities <- nrow(data)
    total_distance   <- round(sum(data$distance, na.rm = TRUE), 1)
    total_time       <- round(sum(data$elapsed_time, na.rm = TRUE), 1)
    total_elevation  <- round(sum(data$total_elevation_gain, na.rm = TRUE), 0)
    rides  <- sum(data$type == "Ride")
    runs   <- sum(data$type == "Run")
    hikes  <- sum(data$type == "Hike")
    
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
  
  # Map
  output$activity_map <- renderLeaflet({
    data <- filtered_activities()
    
    lons.range <- c(-123, -121)
    lats.range <- c(37, 38.7)
    
    # define the lookup vector here
    color_map <- c("Ride" = "#bd423e", "Run" = "#91c0d9", "Hike" = "#2d3ea1")
    
    map <- leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles('CartoDB.Positron',
                       options = providerTileOptions(noWrap = TRUE,
                                                     minZoom = 7,
                                                     maxZoom = 15)) |>
      fitBounds(lng1 = min(lons.range), lat1 = max(lats.range),
                lng2 = max(lons.range), lat2 = min(lats.range)) |>
      addLegend(colors = c("#bd423e", "#91c0d9", "#2d3ea1"),
                labels  = c("Ride", "Run", "Hike"),
                position = "bottomright")
    
    if (nrow(data) > 0) {
      data <- data |>
        filter(!is.na(map.summary_polyline), map.summary_polyline != "")
      
      if (nrow(data) > 0) {
        plot_data <- activities_coords |>
          filter(upload_id %in% data$upload_id)
        
        for (i in seq_len(nrow(plot_data))) {
          activity <- plot_data[i, ]
          coords   <- activity$coords[[1]]
          
          labs <- htmltools::HTML(paste0(
            '<p><b>Activity Date: </b>',    format(activity$date, "%Y-%m-%d"), '</p>',
            '<p><b>Distance (Miles): </b>', round(activity$distance, 2),       '</p>',
            '<p><b>Time (Hours): </b>',     activity$elapsed_time,             '</p>',
            '<p><b>Elevation (Feet): </b>', round(activity$total_elevation_gain, 0), '</p>'
          ))
          
          map <- addPolylines(map,
                              lng     = coords$lon,
                              lat     = coords$lat,
                              color   = unname(color_map[activity$type]), # lookup happens here
                              weight  = 2,
                              opacity = 0.5,
                              label   = labs,
                              labelOptions = labelOptions(
                                style = list(
                                  "font-family" = "Nunito, sans-serif",
                                  "font-weight" = "600",
                                  "box-shadow"  = "3px 3px rgba(0,0,0,0.25)"
                                )))
        }
      }
    }
    
    map
  })
  
  #Table
  output$activity_table <- DT::renderDataTable({
    filtered_activities() |>
      select(any_of(c("name", "type", "date", "distance", "elapsed_time",
                      "total_elevation_gain", "average_speed", "average_heartrate"))) |>
      mutate(date          = format(date, "%Y-%m-%d"),
             distance      = round(distance, 1),
             across(any_of("average_speed"),     ~ round(.x, 2)),
             across(any_of("average_heartrate"), ~ round(.x, 0))) |>
      rename(any_of(c(
        `Activity Name`    = "name",
        Type               = "type",
        Date               = "date",
        `Distance (miles)` = "distance",
        `Time (hours)`     = "elapsed_time",
        `Elevation (ft)`   = "total_elevation_gain",
        `Average Speed`    = "average_speed",
        `Avg Heartrate`    = "average_heartrate"
      )))
  },
  options  = list(pageLength = 15,
                  order = list(list(2, "desc")),
                  dom   = "frtip"),
  rownames = FALSE
  )
  
  # ---- year to date stats ----
  output$stat_activities <- renderText({
    format(n_activities_to_date, big.mark = ",")
  })
  
  output$stat_miles <- renderText({
    format(round(total_miles_to_date, 0), big.mark = ",")
  })
  
  output$stat_kudos <- renderText({
    format(total_kudos_to_date, big.mark = ",")
  })
  
  output$stat_hours <- renderText({
    format(round(sum(act_data_current_year$elapsed_time, na.rm = TRUE), 0), big.mark = ",")
  })
  
  output$stat_elevation <- renderText({
    format(round(sum(act_data_current_year$total_elevation_gain, na.rm = TRUE), 0), big.mark = ",")
  })
  
  output$stat_prs <- renderText({
    format(sum(act_data_current_year$pr_count, na.rm = TRUE), big.mark = ",")
  })
}              