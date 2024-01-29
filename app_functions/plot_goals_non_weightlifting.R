# Define your goals
goals_list_2024_non_weightlifting <- list(
  Soccer = list(times = 30, min_duration = 30),
  Cardio = list(times = 24, min_duration = 10),
  Stretching = list(times = 48, min_duration = 5)
)

# Exercise to category mapping
exercise_category_mapping <- c(
  Soccer = "Soccer",
  Running = "Cardio",
  Swimming = "Cardio",
  Cycling = "Cardio",
  Hiking = "Cardio",
  StairClimbing = "Cardio",
  CardioDance = "Cardio",
  Rowing = "Cardio",
  Yoga = "Stretching",
  Cooldown = "Stretching"
)

# Function to plot progress towards goals in percentage terms
plot_goals_non_weightlifting <- function(
  workout_data, 
  goals_list=goals_list_2024_non_weightlifting, 
  exercise_mapping=exercise_category_mapping
  ) {
  # Map exercise types to categories
  workout_data$category <- exercise_mapping[workout_data$type]

  # Create rows for Jan 1st
  jan_1_data <- data.frame()

  # Filter and process data for each activity category
  progress <- lapply(names(goals_list), function(cat) {
    data <- workout_data %>%
      filter(category == cat, duration >= goals_list[[cat]]$min_duration) %>%
      mutate(date = as.Date(date), year = year(date)) %>%
      filter(year == 2024) %>%
      arrange(date) %>%
      mutate(cumulative_count = row_number()) %>%
      select(date, cumulative_count)

    data$goal = goals_list[[cat]]$times
    data$progress_percentage = (data$cumulative_count / data$goal)
    data$activity = cat
    data
  })

  progress <- do.call(rbind, progress)

  baseline_data <- data.frame(
    date = as.Date("2024-01-01"),
    activity = names(goals_list),
    cumulative_count = 0,
    goal = 0,
    progress_percentage = 0
  )

  progress <- rbind(baseline_data, progress)

  # Plotting
  ggplot(progress, aes(x = date, y = progress_percentage, color = activity)) +
    geom_step() +
    geom_point() +
    geom_hline(yintercept = 1, linetype = "dashed") + # 100% goal line
    scale_y_continuous(name = "Progress towards goal (%)", labels = scales::percent_format()) +
    labs(color = "Activity") +
    theme_minimal() +
    scale_x_date(
      name = "Date",
      date_minor_breaks = "1 month",
      date_labels =  "%b %Y",
      limits = c(as.Date("2024-01-01"), as.Date("2024-12-31"))
    )
}
