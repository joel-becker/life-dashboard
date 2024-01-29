goals_list_2024_weightlifting <- c(
  "Bench Press (Barbell)" = 260,
  "Overhead Press (Barbell)" = 140,
  "Squat (Barbell)" = 260,
  "Romanian Deadlift (Barbell)" = 320,
  "Bent Over Row (Barbell)" = 200,
  "Chin Up" = 260
)

plot_goals_weightlifting <- function(weightlifting_data, goals_list=goals_list_2024_weightlifting) {
  # Filter data for relevant exercises
  weightlifting_data <- weightlifting_data %>%
    filter(exercise_name %in% names(goals_list)) %>%
    select(exercise_name, date, one_rep_max)

  # Set all exercises on Jan 1 to their max for the previous 90 days
  baseline_max <- weightlifting_data %>%
    filter(date >= as.Date("2023-10-01") & date < as.Date("2024-01-01")) %>%
    group_by(exercise_name) %>%
    summarise(baseline_1RM = max(one_rep_max, na.rm = TRUE))

  # Create rows for Jan 1st, 2024 with baseline max
  jan_1_data <- baseline_max %>%
    mutate(
        date = as.Date("2024-01-01"), 
        year = "2024",
        one_rep_max = baseline_1RM
        )

  # Bind these rows with 2024 data
  data_2024 <- weightlifting_data %>%
    left_join(baseline_max, by = "exercise_name") %>%
    filter(format(date, "%Y") == "2024") %>%
    bind_rows(jan_1_data) %>%
    arrange(exercise_name, date)

  # Calculate cumulative max for 2024
  data_2024 <- data_2024 %>%
    group_by(exercise_name) %>%
    mutate(
      cummax_1RM = cummax(one_rep_max),
      goal = goals_list[exercise_name]
      ) %>%
    ungroup()

  # Normalize progress
  normalized_data <- data_2024 %>%
    mutate(
      progress = (cummax_1RM - baseline_1RM) / (goal - baseline_1RM)
    ) %>%
    group_by(exercise_name, date) %>%
    filter(format(date, "%Y") == "2024" & one_rep_max >= max(one_rep_max))

  # Plotting
  ggplot(
    normalized_data, 
    aes(x = date, y = progress, group = exercise_name, color = exercise_name)
    ) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed") +  # Goal line
    scale_y_continuous(name = "Progress towards goal (%)", labels = scales::percent_format()) +
    labs(colour = "Exercise") +
    theme_minimal() +
    scale_x_date(
      name = "Date",
      date_minor_breaks = "1 month",
      date_labels =  "%b %Y",
      limits = c(as.Date("2024-01-01"), as.Date("2024-12-31"))
    )
}
