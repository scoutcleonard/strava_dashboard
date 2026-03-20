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