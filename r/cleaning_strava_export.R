
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

if (file.exists(here::here("data/strava_data.csv"))) {
  # skip API fetch, just read the CSV
  act_data <- readr::read_csv(here::here("data/strava_data.csv"))
} else {
  # only runs locally when CSV is missing
  library(rStrava)
  source("../../../credentials/strava_credentials.R")
  my_data  <- get_activity_list(stoken)
  compile_activities(my_data) |>
    select(any_of(columns_of_interest)) |>
    write_csv(here::here("data/strava_data.csv"))
  act_data <- read_csv(here::here("data/strava_data.csv"))
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
         year_week = paste0(year, "-W", sprintf("%02d", week)),
         distance = distance * 0.621371,
         average_speed = average_speed * 0.621371,
         max_speed = 0.621371 * max_speed,
         elev_high = elev_high * 3.28084,
         elev_low = elev_low * 3.28084,
         total_elevation_gain = total_elevation_gain * 3.28084) |> 
  rename(latitude = "start_latlng1",
         longitude = "start_latlng2") |> 
  filter(type %in% c("Ride", "Run", "Hike")) # pre-decode all polylines once at startup


activities_coords <- activities |>
  filter(!is.na(map.summary_polyline), map.summary_polyline != "") |>
  mutate(coords = map(map.summary_polyline, decode_pl))
