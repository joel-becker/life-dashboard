#------------------------------------------------------------------------------#
# Produces charts
# Author: Joel Becker

# Notes:
#
#------------------------------------------------------------------------------#


########################################################
######################## Set-up ########################
########################################################

# load libraries
library("dplyr")
library("janitor")
library("zoo")

race_chart <- function(data, exercise_names = c()) {
  if (length(exercise_names) == 0) {
    exercise_names <- arrange(count(data, exercise_name), -n)$exercise_name[c(2, 3)]
  }

  filtered_data <- data %>%
    filter(exercise_name %in% exercise_names)

  plot <- ggplot(
    filtered_data,
    aes(x = date, y = one_rep_max, group = exercise_name, colour = exercise_name)
  ) +
    geom_line(aes(y = cummax_one_rep_max), size = 1.5) +
    geom_point(size = 3, alpha = 1 / 3) +
    # geom_line(stat="smooth", method = "lm", formula = y ~ poly(x, 2), se=FALSE, size=1.5, alpha=1/3) +

    xlim(min(data$date), max(data$date)) +
    ylim(0, NA) +
    xlab("Date") +
    ylab("Estimated one rep max") +
    scale_color_manual(
      values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
      aesthetics = c("colour", "fill")
    ) +
    theme_minimal()

  if (length(exercise_names) == 1) {
    plot <- plot +
      theme(legend.position = "none")
  }

  return(plot)
}

volume_chart <- function(data, exercise_categories = c(), rollmean_length = 14) {
  # if (length(exercise_names) == 0) {
  #  exercise_names <- arrange(count(data, exercise_name), -n)$exercise_name[c(2, 3)]
  # }
  #
  # filtered_data <- data %>%
  #  filter(exercise_name %in% exercise_names)

  plot <- data %>%
    ggplot(aes(x = date, y = volume)) +
    geom_col(alpha = 1 / 3) +
    geom_line(
      aes(
        y = rollmean(volume, rollmean_length, na.pad = TRUE),
        fill = NULL
      ),
      size = 1,
      colour = "#7570b3"
    ) +
    scale_fill_manual(
      values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
    ) +
    xlim(min(data$date), max(data$date)) +
    xlab("Date") +
    # ylim(min(data$value) - 1, max(data$value) + 1) +
    # ylab("Calorie deficit") +
    labs(title = "Volume (in 1RM units)") +
    theme_minimal()

  return(plot)
}

diet_chart <- function(data, metric_names, rollmean_length = 7) {
  data <- data %>%
    filter(
      metric %in% metric_names &
        !is.na(value)
    )

  if ("EnergyDeficit" %in% metric_names | "EnergyDeficitPct" %in% metric_names) {
    plot <- data %>%
      ggplot(aes(x = date, y = value, fill = positive_value)) +
      geom_col(alpha = 1 / 3) #+
    # geom_ribbon(aes(ymin = 0, ymax = value, fill = positive_value))
  } else {
    plot <- data %>%
      ggplot(aes(x = date, y = value)) +
      geom_point(alpha = 1 / 3)
  }

  plot <- plot +
    geom_line(
      aes(
        y = rollmean(value, rollmean_length, na.pad = TRUE),
        fill = NULL
      ),
      size = 1,
      colour = "#7570b3"
    ) +

    scale_fill_manual(
      values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
    ) +

    xlim(min(data$date), max(data$date)) +
    xlab("Date") +
    # ylim(min(data$value) - 1, max(data$value) + 1) +
    # ylab("Calorie deficit") +
    labs(title = "Calorie deficit and surplus") +

    theme_minimal()

  return(plot)
}
