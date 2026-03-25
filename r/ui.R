ui <- navbarPage(
  
  title = "Scout's Pastry-Fueled Miles 🥐",
  
  # apply your existing styles globally
  header = tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Ranchers&family=Nunito:wght@400;500;600&display=swap"
    ),
    tags$style(
      HTML(paste(readLines(here::here("www/styles.css")), collapse = "\n"))
    )
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
  
  # ---- Tab 2: Year to Date ----
  tabPanel("Year to Date",
           div(style = "padding: 32px;",
               
               h3(style = paste("font-family: 'Ranchers', cursive;",
                                "color: #2d3ea1;",
                                "font-size: 28px;",
                                "margin-bottom: 24px;"),
                  paste(year, "so far...")),
               
               # ---- row 1: big three ----
               fluidRow(
                 column(4,
                        div(class = "stat-card",
                            div(class = "stat-number", textOutput("stat_activities")),
                            div(class = "stat-label", "activities")
                        )
                 ),
                 column(4,
                        div(class = "stat-card",
                            div(class = "stat-number", textOutput("stat_miles")),
                            div(class = "stat-label", "miles")
                        )
                 ),
                 column(4,
                        div(class = "stat-card",
                            div(class = "stat-number", textOutput("stat_kudos")),
                            div(class = "stat-label", "kudos")
                        )
                 )
               ),
               
               # ---- row 2: more stats ----
               fluidRow(
                 style = "margin-top: 20px;",
                 column(4,
                        div(class = "stat-card",
                            div(class = "stat-number", textOutput("stat_hours")),
                            div(class = "stat-label", "hours moving")
                        )
                 ),
                 column(4,
                        div(class = "stat-card",
                            div(class = "stat-number", textOutput("stat_elevation")),
                            div(class = "stat-label", "feet climbed")
                        )
                 ),
                 column(4,
                        div(class = "stat-card",
                            div(class = "stat-number", textOutput("stat_prs")),
                            div(class = "stat-label", "personal records")
                        )
                 )
               ),
               
               # ---- row 3: heatmap ---- (now INSIDE the tabPanel)
               fluidRow(
                 style = "margin-top: 32px;",
                 column(12,
                        div(
                          style = paste(
                            "background: #e4e6cc;",
                            "border-radius: 10px;",
                            "padding: 16px;",
                            "box-shadow: 4px 4px 0px #3d302f;"
                          ),
                          plotOutput("heatmap_plot", height = "300px")
                        )
                 )
               )
               
           )  # closes div(style = "padding: 32px;")
  ),   # closes tabPanel("Year to Date")
  
  
  # ---- Tab 3: Data Table ----
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