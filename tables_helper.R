#------------------------------------------------------------------------------#
# Functions for wrangling tables
# Author: Joel Becker

# Notes:
#
#------------------------------------------------------------------------------#


########################################################
######################## Set-up ########################
########################################################

# load libraries
packages <- c("tidyverse", "janitor", "lubridate", "XML")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = TRUE)

# set wd
setwd("/Users/joel/projects/life-dashboard")

# source
source("path_names.R")

# functions

clean_health_data <- function(data){
  data <- data %>%
    clean_names() %>%
    mutate(
      device = gsub(".*(name:)|,.*", "",device),
      value = as.numeric(as.character(value)),
      end_date = ymd_hms(end_date,tz="America/New_York"),
      date = date(end_date),
      year = year(end_date),
      month = month(end_date),
      day = day(end_date),
      yday = yday(end_date),
      wday = wday(end_date),
      hour = hour(end_date),
      minute = minute(end_date),
      type = str_remove(type, "HKQuantityTypeIdentifier")
    )

  return(data)
}

join_dates <- function(data){
  dates <- data.frame(date = seq.Date(min(data$date), Sys.Date(), 1))

  data <- data %>%
    # join intensity data to dates
    full_join(dates, by = "date") %>%
    arrange(date)

  return(data)
}

wrangle_weight_data <- function(data){
  data <- data %>%
    clean_health_data() %>%
    filter(type == 'BodyMass') %>%
    select(date, value) %>%
    join_dates() %>%
    fill("value") %>%
    dplyr::rename(body_mass = value) %>%
    arrange(date) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(body_mass = min(body_mass))

  return(data)
}

wrangle_health_data <- function(data){
  data <- data %>%
    clean_health_data()

  return(data)
}

wrangle_deficit_data <- function(data){
  data <- data %>%
    clean_health_data() %>%
    filter(
      end_date >= '2020/12/24' &
        type %in% c(
          'BasalEnergyBurned',
          'ActiveEnergyBurned',
          'DietaryEnergyConsumed',
          "DietarySugar",
          "DietaryProtein"
        )
    ) %>%
    select(date, type, value) %>%
    dplyr::group_by(date, type) %>%
    dplyr::summarise(value = sum(value)) %>%
    ungroup() %>%
    spread(type, value) %>%
    mutate(
      ActiveEnergyBurned = ActiveEnergyBurned * 0.9,
      DietaryEnergyConsumed = DietaryEnergyConsumed * 1.1,
      EnergyBurned = ActiveEnergyBurned + BasalEnergyBurned,
      EnergyConsumed = DietaryEnergyConsumed,
      EnergyDeficit = EnergyBurned - EnergyConsumed,
      EnergyDeficitPct = EnergyDeficit / EnergyBurned#,
      #EnergyDeficitCat = if_else(EnergyDeficit > 0, 'Deficit', 'Surplus')
    ) %>%
    filter(EnergyConsumed > 0) %>%
    select(-c(ActiveEnergyBurned, BasalEnergyBurned, DietaryEnergyConsumed)) %>%
    pivot_longer(!date, names_to = "metric", values_to = "value") %>%
    mutate(
      positive_value = case_when(
        value > 0 ~ "Deficit",
        value <= 0 ~ "Surplus",
        TRUE ~ NA_character_
      )
    )

  return(data)
}

clean_exercise_data <- function(data){
  data <- data %>%
    clean_names() %>%
    mutate(date = as.Date(date)) %>%
    select(date, exercise_name, set_order, weight, reps)

  return(data)
}

filter_exercise_data <- function(data){
  data <- data %>%
    filter(!(date < "2020-10-01" & exercise_name == "Triceps Dip (Assisted)")) %>%
    filter(!(date < "2020-10-01" & exercise_name == "Lateral Raise (Machine)")) %>%
    filter(!(date < "2020-10-01" & exercise_name == "Reverse Fly (Machine)")) %>%
    filter(!(date < "2020-10-01" & exercise_name == "Lat Pulldown (Cable)")) %>%
    filter(!(exercise_name == "Bicep Curl (Machine)"))

  #filter(date > as.Date("2020-09-01"))
}

calculate_1RM <- function(data){
  unweighted_exercises <- c(
    "Chin Up",
    "Pull Up",
    "Press Up",
    "Triceps Dip"
  )

  all_exercises <- unique(data$exercise_name)
  barbell_exercises <- all_exercises[grepl("(Barbell)", all_exercises, fixed=TRUE)]

  data <- data %>%
    mutate(
      weight = case_when(
        exercise_name %in% barbell_exercises ~ weight + 45,
        TRUE ~ weight
      ),
      one_rep_max = case_when(
        !(exercise_name %in% unweighted_exercises) ~ weight * (1 + (reps / 25)),
        exercise_name %in% unweighted_exercises ~ (body_mass * 2.20462) * (1 + (reps / 25)),
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::group_by(exercise_name) %>%
    dplyr::mutate(
      cummax_one_rep_max = cummax(one_rep_max)
    )
  #  group_by(date) %>%
  #  mutate(
  #    exercise_order = as.character(length(unique(exercise_name))),
  #    set_order = as.character(set_order)
  #  )
  #
  #model <- lm(
  #  log(one_rep_max) ~ exercise_name +
  #    log(as.numeric(as.Date(date))) +
  #    exercise_order +
  #    set_order,
  #  data
  #  )
  #summary(model)

  return(data)
}

wrangle_exercise_data <- function(data, weight_data){
  data <- data %>%
    clean_exercise_data() %>%
    filter_exercise_data() %>%
    right_join(weight_data, by = "date") %>%
    arrange(date) %>%
    filter(!is.na(exercise_name)) %>%
    calculate_1RM()

  return(data)
}

wrangle_volume_data <- function(data){
  data <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(volume = sum(one_rep_max)) %>%
    join_dates() %>%
    mutate(volume = replace_na(volume, 0)) %>%
    arrange(date)

  return(data)
}

unpack_mentalhealth_data <- function(){
  system(
    "mv /Users/joel/Downloads/*.emoods* raw_data/mental_health/zip"
  )
  system(
    "mv raw_data/mental_health/zip/*.emoods* raw_data/mental_health/zip/emoods_$(date +%F).zip"
  )
  system(
    "unzip raw_data/mental_health/zip/emoods_$(date +%F).zip -d raw_data/mental_health/csv/"
  )
}

wrangle_mentalhealth_data <- function(data){
  data <- data %>%
    clean_names() %>%
    mutate(
      date = as_date(date_yyyy_mm_dd),
      # explanation for mental health metric:
      # https://www.wolframalpha.com/input/?i=0.01+*+x%5E4+-+0.01+%3D+1
      # intention is to get kind of exponential score between -1 and 1
      mental_health = (elevated^3.3291) - (
        (0.1*(irritability^6.6582)) +
          (0.45*(anxiety^6.6582)) +
          (0.45*(depressed^6.6582))
      )^0.5,
      sleep = replace(sleep, sleep == 0.0, NA),
      positive_value = case_when(
        mental_health > 0 ~ "Net positive",
        mental_health <= 0 ~ "Net negative",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-c(
      id,
      date_yyyy_mm_dd,
      #elevated,
      #irritability,
      #anxiety,
      #depressed,
      psychotic_symptoms,
      note,
      menstrual_cycle,
      weight
    ))
}

wrangle_sleep_data <- function(data){

}
