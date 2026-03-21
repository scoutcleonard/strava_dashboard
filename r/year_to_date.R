year <- year(Sys.Date())

act_data_current_year <- act_data |> 
  filter(year(start_date) == year)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------- NUMBERS FOR TILES-------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n_activities_to_date <- nrow(act_data_current_year)

total_kudos_to_date <- sum(act_data_current_year$kudos_count)

total_miles_to_date <- sum(act_data_current_year$distance)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- TIME SERIES HEATMAP-------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

heatmap_data <- act_data |>
  select(distance, elapsed_time, type, start_date) |>
  mutate(year  = year(start_date),
         month = month(start_date, label = TRUE)) |>
  group_by(year, month) |>
  summarise(total_hours = sum(elapsed_time, na.rm = TRUE),
            .groups = "drop")

heatmap <- ggplot(heatmap_data,
                  aes(x = year, y = month, fill = total_hours)) +
  geom_tile(color = "#e4e6cc", linewidth = 0.5) +
  scale_fill_gradient(low = "#91c0d9", high = "#2d3ea1",
                      name = "Hours") +
  scale_y_discrete(limits = rev(month.abb)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Nunito"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "#3d302f", 
                             size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#e4e6cc", 
                                   color = NA),
    panel.background = element_rect(fill = "#e4e6cc", 
                                    color = NA))
