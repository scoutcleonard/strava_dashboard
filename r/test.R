
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- DASHBOARD LIBRARIES-------------------------------
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
      htmlwidgets)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------- PULL STRAVA DATA--------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#strava credentials, outside this github repo
source("../../../credentials/strava_credentials.R")

# Download activities
my_data  <- get_activity_list(stoken)

# Compile activities
act_data <- compile_activities(my_data)

act_data <- compile_activities(my_data) %>% 
  write_csv(here("data/strava_data.csv"))

act_data <- read_csv(here("data/strava_data.csv"))

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
                         'total_elevation_gain',
                         'upload_id')

activities <- select(act_data, match(columns_of_interest,
                                     names(act_data)))

activities <- activities %>% 
  mutate(elapsed_time = round(elapsed_time / 60 /60, digits = 2),
         moving_time = round(moving_time / 60 / 60, digits = 2),
         date = gsub("T.*$", '', start_date) %>% 
           as.POSIXct(., format = "%Y-%m-%d")) %>% 
  rename(latitude = "start_latlng1",
         longitude = "start_latlng2") %>% 
  filter(type == c("Ride", "Run", "Hike"))


## Create blank map bounded by given lon and lat
lons.range <- c(-123, -121)
lats.range <- c(37, 38.7)

#create a blank map
map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles('CartoDB.Positron',
                   options = providerTileOptions(noWrap = T,
                                                 minZoom = 7,
                                                 maxZoom = 15)) %>%
  fitBounds(lng1 = min(lons.range),
            lat1 = max(lats.range),
            lng2 <- max(lons.range),
            lat2 = min(lats.range)) %>% 
  addLegend(colors = c("#262d42", "#f7c267", "#591c19"),
            labels = c("Ride", "Run", "Hike"),
            position = "bottomright")

map 

unique_activites <- unique(activities$upload_id)

for (i in unique_activites){
  
  #get activity
  activity <- filter(activities,
                     upload_id == i)
  
  #decode polyline
  coords <- decode_pl(activity$map.summary_polyline)
  
  #labs
  labs <- paste0('<p>',
                 '<b>',
                 "Activity Date: ",
                 '</b>',
                 activity$date,
                 '<p></p>',
                 '<b>',
                 "Distance (Miles): ",
                 '</b>',
                 activity$distance,
                 '<p></p>',
                 '<b>',
                 "Time (Hours): ",
                 '</b>',
                 activity$elapsed_time,
                 '<p></p>',
                 '<b>',
                 "Elevation Gain (Feet): ",
                 '</b>',
                 activity$total_elevation_gain,
                 '<p>') %>% 
    htmltools::HTML()
  
  #plot activity! 
  map <- if (activity$type == "Ride") {
    addPolylines(map,
                 lng = coords$lon,
                 lat = coords$lat,
                 color = "#262d42",
                 weight = 2,
                 opacity = 1/2,
                 label = labs,
                 labelOptions = labelOptions(style = list("font-family" = "serif",
                                                          "font-style" = "bold",
                                                          "box-shadow" = "3px 3px rgba(0, 0, 0, 0.25)")))
  } else if (activity$type == "Run") {
    addPolylines(map,
                 lng = coords$lon,
                 lat = coords$lat,
                 color = "#f7c267",
                 weight = 2,
                 opacity = 1/2,
                 label = labs,
                 labelOptions = labelOptions(style = list("font-family" = "serif",
                                                          "font-style" = "bold",
                                                          "box-shadow" = "3px 3px rgba(0, 0, 0, 0.25)")))
  } else if (activity$type == "Hike") {
    addPolylines(map,
                 lng = coords$lon,
                 lat = coords$lat,
                 color = "#591c19",
                 weight = 2,
                 opacity = 1/2,
                 label = labs,
                 labelOptions = labelOptions(style = list("font-family" = "serif",
                                                          "font-style" = "bold",
                                                          "box-shadow" = "3px 3px rgba(0, 0, 0, 0.25)")))
  }
}

map
