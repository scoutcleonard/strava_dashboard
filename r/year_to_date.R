year <- year(Sys.Date())

act_data_current_year <- act_data |> 
  filter(year(start_date) == year)

n_activities_to_date <- nrow(act_data_current_year)

total_kudos_to_date <- sum(act_data_current_year$kudos_count)

total_miles_to_date <- sum(act_data_current_year$distance)
