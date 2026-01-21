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
      scales)

source("../../../credentials/strava_credentials.R")

# Download activities
my_data  <- get_activity_list(stoken)

# Compile activities
act_data <- compile_activities(my_data)

# Data preparation
act_data <- act_data %>%
  mutate(
    start_date = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    distance_miles = distance * 0.621371,  # Convert km to miles
    moving_time_hours = moving_time / 3600,
    elapsed_time_hours = elapsed_time / 3600,
    sport_type = ifelse(is.na(sport_type), type, sport_type)
  ) %>%
  arrange(start_date)

# Calculate cumulative miles by sport
act_data <- act_data %>%
  group_by(sport_type) %>%
  mutate(cumulative_miles = cumsum(distance_miles)) %>%
  ungroup()

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700;900&family=Fraunces:wght@700;900&display=swap');
      
      body {
        background: linear-gradient(135deg, #F5F3F0 0%, #E8E5E0 50%, #F0F5F5 100%);
        font-family: 'Fraunces', serif;
      }
      
      h2, h3, h4 {
        font-family: 'Playfair Display', serif;
        text-transform: uppercase;
        letter-spacing: -0.5px;
        color: #C84C3C;
        text-shadow: 2px 2px 0px rgba(0,0,0,0.08);
      }
      
      .well {
        background: linear-gradient(135deg, #D85A4A 0%, #C84C3C 100%);
        border: 4px solid #2C4A52;
        border-radius: 20px;
        box-shadow: 6px 6px 0px rgba(44,74,82,0.15);
        color: #FFF;
        font-weight: 700;
      }
      
      .well h4 {
        color: #FAF8F5;
        font-size: 14px;
        margin-bottom: 10px;
        text-shadow: 1px 1px 0px rgba(0,0,0,0.2);
      }
      
      .well h2 {
        color: #FFFFFF;
        font-size: 48px;
        font-weight: 900;
        margin: 0;
        text-shadow: 2px 2px 0px rgba(0,0,0,0.2);
      }
      
      .nav-tabs > li > a {
        background: #8B7355;
        color: #FAF8F5;
        font-weight: 900;
        text-transform: uppercase;
        border: 3px solid #2C4A52;
        border-radius: 15px 15px 0 0;
        margin-right: 5px;
        font-family: 'Playfair Display', serif;
      }
      
      .nav-tabs > li.active > a {
        background: #4DB8C4;
        color: #FFF;
        border-bottom: 3px solid #4DB8C4;
      }
      
      .checkbox label {
        font-weight: 700;
        font-size: 16px;
        color: #2C4A52;
        text-transform: uppercase;
      }
      
      h1 {
        font-family: 'Playfair Display', serif;
        color: #C84C3C;
        font-size: 52px;
        text-transform: uppercase;
        letter-spacing: -1px;
        text-shadow: 3px 3px 0px rgba(0,0,0,0.08);
        background: linear-gradient(135deg, #D85A4A, #4DB8C4, #8B7355);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
      
      .form-group label {
        font-weight: 700;
        color: #2C4A52;
        text-transform: uppercase;
        font-size: 14px;
      }
      
      hr {
        border-top: 4px solid #4DB8C4;
      }
    "))
  ),
  
  titlePanel("🏃 STRAVA ACTIVITY DASHBOARD 🚴"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "sport_filter",
        "Select Sport Types:",
        choices = unique(act_data$sport_type),
        selected = unique(act_data$sport_type)
      ),
      hr(),
      dateRangeInput(
        "date_range",
        "Date Range:",
        start = min(act_data$start_date, na.rm = TRUE),
        end = max(act_data$start_date, na.rm = TRUE)
      ),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 fluidRow(
                   column(4, wellPanel(
                     h4("Total Activities"),
                     h2(textOutput("total_activities"))
                   )),
                   column(4, wellPanel(
                     h4("Total Distance (miles)"),
                     h2(textOutput("total_distance"))
                   )),
                   column(4, wellPanel(
                     h4("Total Time (hours)"),
                     h2(textOutput("total_time"))
                   ))
                 ),
                 hr(),
                 h3("Time Spent by Sport Type"),
                 plotOutput("time_by_sport", height = "400px")
        ),
        
        tabPanel("Route Heatmap",
                 leafletOutput("route_map", height = "600px")
        ),
        
        tabPanel("Cumulative Miles",
                 plotOutput("cumulative_plot", height = "600px")
        )
      ),
      width = 9
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    act_data %>%
      filter(
        sport_type %in% input$sport_filter,
        start_date >= input$date_range[1],
        start_date <= input$date_range[2]
      )
  })
  
  # Summary metrics
  output$total_activities <- renderText({
    nrow(filtered_data())
  })
  
  output$total_distance <- renderText({
    format(round(sum(filtered_data()$distance_miles, na.rm = TRUE), 1), big.mark = ",")
  })
  
  output$total_time <- renderText({
    format(round(sum(filtered_data()$moving_time_hours, na.rm = TRUE), 1), big.mark = ",")
  })
  
  # Time by sport plot
  output$time_by_sport <- renderPlot({
    time_summary <- filtered_data() %>%
      group_by(sport_type) %>%
      summarise(total_hours = sum(moving_time_hours, na.rm = TRUE)) %>%
      arrange(desc(total_hours))
    
    # Scandinavian-inspired color palette
    colors <- c("#D85A4A", "#4DB8C4", "#8B7355", "#2C4A52", "#C84C3C", "#5BA99D", "#A68A6D")
    
    ggplot(time_summary, aes(x = reorder(sport_type, total_hours), y = total_hours, fill = sport_type)) +
      geom_col(color = "#2C4A52", size = 3) +
      coord_flip() +
      labs(x = "", y = "HOURS", title = "") +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "none",
        text = element_text(family = "serif", face = "bold", color = "#2C4A52"),
        axis.text = element_text(size = 14, face = "bold", color = "#2C4A52"),
        axis.title = element_text(size = 16, face = "bold", color = "#C84C3C"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#E8E5E0", size = 1),
        plot.background = element_rect(fill = "#F5F3F0", color = NA),
        panel.background = element_rect(fill = "#F5F3F0", color = NA)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      scale_fill_manual(values = rep(colors, length.out = nrow(time_summary)))
  })
  
  # Route heatmap
  output$route_map <- renderLeaflet({
    map_data <- filtered_data() %>%
      filter(!is.na(start_latlng1), !is.na(start_latlng2))
    
    if (nrow(map_data) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = -122.29, lat = 37.88, zoom = 10))
    }
    
    leaflet(map_data) %>%
      addTiles() %>%
      addHeatmap(
        lng = ~start_latlng2,
        lat = ~start_latlng1,
        intensity = ~distance_miles,
        blur = 20,
        max = 0.5,
        radius = 15
      ) %>%
      fitBounds(
        lng1 = min(map_data$start_latlng2, na.rm = TRUE),
        lat1 = min(map_data$start_latlng1, na.rm = TRUE),
        lng2 = max(map_data$start_latlng2, na.rm = TRUE),
        lat2 = max(map_data$start_latlng1, na.rm = TRUE)
      )
  })
  
  # Cumulative miles plot
  output$cumulative_plot <- renderPlot({
    cum_data <- filtered_data() %>%
      group_by(sport_type) %>%
      arrange(start_date) %>%
      mutate(cumulative_miles = cumsum(distance_miles)) %>%
      ungroup()
    
    # Scandinavian-inspired color palette
    colors <- c("#D85A4A", "#4DB8C4", "#8B7355", "#2C4A52", "#C84C3C", "#5BA99D", "#A68A6D")
    
    ggplot(cum_data, aes(x = start_date, y = cumulative_miles, color = sport_type)) +
      geom_line(size = 3) +
      labs(
        x = "DATE",
        y = "CUMULATIVE MILES",
        color = "SPORT TYPE",
        title = "CUMULATIVE MILES BY SPORT TYPE"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold", color = "#C84C3C"),
        text = element_text(family = "serif", face = "bold", color = "#2C4A52"),
        axis.text = element_text(size = 12, face = "bold", color = "#2C4A52"),
        axis.title = element_text(size = 16, face = "bold", color = "#C84C3C"),
        plot.title = element_text(size = 20, face = "bold", color = "#C84C3C", hjust = 0.5),
        panel.grid.major = element_line(color = "#E8E5E0", size = 1),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F5F3F0", color = NA),
        panel.background = element_rect(fill = "#F5F3F0", color = NA),
        legend.background = element_rect(fill = "#F5F3F0", color = NA)
      ) +
      scale_y_continuous(labels = comma) +
      scale_x_datetime(date_labels = "%b %Y") +
      scale_color_manual(values = rep(colors, length.out = length(unique(cum_data$sport_type))))
  })
}

# Generate the app
shinyApp(ui = ui, server = server)
