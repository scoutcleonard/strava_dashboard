ui <- navbarPage(
  title = "Scout's Pastry-Fuelled Miles 🥐",
  
  # apply your existing styles globally
  header = tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Ranchers&family=Nunito:wght@400;500;600&display=swap"),
    tags$link(rel = "stylesheet",
              href = "styles.css")  # Shiny always serves www/ from the app root
  ),
  
  # ---- Tab 1: Map ----
  tabPanel("Map",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput("time_period", 
                           "Select Time Period:",
                           choices = c("All Time", 
                                       "Year", 
                                       "Month", 
                                       "Week"),
                           selected = "All Time"),
               conditionalPanel(condition = "input.time_period == 'Year'",
                                selectInput("selected_year", 
                                            "Select Year:", 
                                            choices = NULL)),
               conditionalPanel(condition = "input.time_period == 'Month'",
                                selectInput("selected_month", 
                                            "Select Month:", 
                                            choices = NULL)),
               conditionalPanel(condition = "input.time_period == 'Week'",
                                selectInput("selected_week", 
                                            "Select Week:", 
                                            choices = NULL)),
               hr(),
               checkboxGroupInput(
                 inputId  = "selected_types",
                 label    = "Activity Type:",
                 choices  = c("Ride", "Run", "Hike"),
                 selected = c("Ride", "Run", "Hike") 
               ),
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
  ),
  
  # ---- Tab 2: Data Table ----
  tabPanel("Strava Data",
           div(style = "padding: 20px;",
               fluidRow(
                 column(12,
                        h4("All Activities"),
                        DT::dataTableOutput("activity_table")
                 )
               )
           )
  )
)