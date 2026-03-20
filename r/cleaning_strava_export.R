
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
                         'total_elevation_gain',
                         'achievement_count',
                         'average_heartrate',
                         'average_speed',
                         'commute',
                         'kudos_count',
                         'max_heartrate', 
                         'max_speed',
                         'pr_count',
                         'name')


# Only fetch fresh data locally; use saved CSV on server
if (!file.exists(here("data/strava_data.csv"))) {
  
  source("../../../credentials/strava_credentials.R")
  my_data  <- get_activity_list(stoken)
  act_data <- compile_activities(my_data) |> 
    select(any_of(columns_of_interest)) |> 
    write_csv(here("data/strava_data.csv"))
}

act_data <- read_csv(here("data/strava_data.csv"))

activities <- act_data |>  
  mutate(elapsed_time = round(elapsed_time / 60 / 60, digits = 2),
         moving_time = round(moving_time / 60 / 60, digits = 2),
         date = as.POSIXct(gsub("T.*$", "", start_date), format = "%Y-%m-%d"),
         year = year(date),
         month = month(date),
         week = week(date),
         year_month = format(date, "%Y-%m"),
         year_week = paste0(year, "-W", sprintf("%02d", week))) |> 
  rename(latitude = "start_latlng1",
         longitude = "start_latlng2") |> 
  filter(type %in% c("Ride", "Run", "Hike"))
