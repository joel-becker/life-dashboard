# Define goals
goals_list <- list(
  ProfessionalMeetings = 50,
  FriendMeetings = 100,
  AskFriendsUpdates = 50,
  FlossingWeeks = 26
)

# Main function to process data and plot progress
plot_goals_non_exercise <- function(data, goals = goals_list) {
  data$date <- as.Date(data$date)
  data$week <- floor_date(data$date, "week")
  
  summary_data <- tibble(date = as.Date(character()), Goal = character(), Count = integer(), Cumulative = integer(), progress_percentage = numeric())
  
  categories <- c("energizing_professional_interactions", "asked_how_friends_are", "energizing_personal_interactions")
  category_names <- c("ProfessionalMeetings", "AskFriendsUpdates", "FriendMeetings")
  
  for (i in seq_along(categories)) {
    cat_data <- data %>%
      filter(.data[[categories[i]]] > 0) %>%
      group_by(date) %>%
      summarise(Count = sum(.data[[categories[i]]]), .groups = 'drop') %>%
      mutate(Goal = category_names[i], 
             date = as.Date(date), 
             Cumulative = cumsum(Count),
             progress_percentage = Cumulative / goals[[category_names[i]]]) %>%
      select(date, Goal, Count, Cumulative, progress_percentage)
  
    summary_data <- bind_rows(summary_data, cat_data)
  }
  
  flossing_data <- data %>%
    filter(!is.na(floss), floss > 0) %>%
    group_by(week) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    filter(Count >= 3) %>%
    mutate(
        Goal = "FlossingWeeks", 
        date = as.Date(week), 
        Cumulative = cumsum(Count >= 3)
        ) %>%
    mutate(progress_percentage = Cumulative / goals$FlossingWeeks) %>%
    select(date, Goal, Count, Cumulative, progress_percentage)
  
  summary_data <- bind_rows(summary_data, flossing_data)

  baseline_data <- data.frame(
    date = as.Date("2024-01-01"),
    Goal = names(goals),
    Count = 0,
    Cumulative = 0,
    progress_percentage = 0
  )

  summary_data <- rbind(baseline_data, summary_data)
  
  # Plotting the progress
  ggplot(summary_data, aes(x = date, y = progress_percentage, color = Goal)) +
    geom_step() + 
    geom_point() +
    geom_hline(yintercept = 1, linetype = "dashed") + # 100% goal line
    scale_y_continuous(name = "Progress towards goal (%)", labels = scales::percent_format()) +
    theme_minimal() +
    scale_x_date(
      name = "Date",
      date_minor_breaks = "1 month",
      date_labels =  "%b %Y",
      limits = c(as.Date("2024-01-01"), as.Date("2024-12-31"))
    )
}