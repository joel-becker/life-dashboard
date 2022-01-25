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
packages <- c("tidyverse", "janitor", "lubridate", "XML", "triangle", "zoo")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = TRUE)

# set wd
setwd("/Users/joel/projects/life-dashboard")

# source
source("path_names.R")


########################################################
####################### Functions ######################
########################################################

clean_health_data <- function(data){
  # cleans health data
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

join_dates <- function(data, multiple_metrics = FALSE, type = "energy"){
  # joins dates between earliest date in data and now with data, to remove gaps
  dates <- seq.Date(min(data$date), Sys.Date(), 1)
  
  if (multiple_metrics == FALSE) {
    dates <- data.frame(date = dates)  
  } else if (type == "energy") {
    dates <- data.frame(
      date = rep(dates, 4),
      metric = c(
        rep("Calorie expenditure", length(dates)),
        rep("Calorie intake", length(dates)),
        rep("Calorie deficit (absolute)", length(dates)),
        rep("Calorie deficit (relative)", length(dates))
      )
    )
  } else if (type == "nutrition") {
    dates <- data.frame(
      date = rep(dates, 5),
      metric = c(
        rep("Protein", length(dates)),
        rep("Sugar", length(dates)),
        rep("Water", length(dates)),
        rep("Carbohydrates", length(dates)),
        rep("Fat", length(dates))
      )
    )
  } else if (type == "volume") {
    dates <- data.frame(
      date = rep(dates, 2),
      metric = c(
        rep("hypertrophy_adjusted_volume", length(dates)),
        rep("volume", length(dates))
      )
    )
  }
  
  if (multiple_metrics == FALSE){
    data <- data %>%
      # join intensity data to dates
      full_join(dates, by = "date") %>%
      arrange(date)
  } else {
    data <- data %>%
      # join intensity data to dates
      full_join(dates, by = c("date", "metric")) %>%
      arrange(date)
  }
  
  return(data)
}

wrangle_weight_data <- function(data){
  # wrangles weight data
  data <- data %>%
    clean_health_data() %>%
    filter(type == 'BodyMass') %>%
    dplyr::select(date, value) %>%
    join_dates() %>%
    #fill("value") %>%
    mutate(value = na.approx(value * 2.20462, na.rm = FALSE)) %>%
    dplyr::rename(body_mass = value) %>%
    arrange(date) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(body_mass = min(body_mass))
  
  return(data)
}

wrangle_nutrition_data <- function(data){
  # wrangles nutrition data
  data <- data %>%
    clean_health_data() %>%
    filter(
      end_date >= '2020/12/24' &
        type %in% c(
          "DietaryWater",
          "DietarySugar",
          "DietaryProtein",
          "DietaryFatTotal",
          "DietaryCarbohydrates"
        )
      ) %>%
    dplyr::select(date, type, value) %>%
    dplyr::group_by(date, type) %>%
    dplyr::summarise(value = sum(value)) %>%
    ungroup() %>%
    spread(type, value) %>%
    mutate(
      Water = DietaryWater,
      Sugar = DietarySugar *
        (1 + 0.165 *
           (0.6 + 0.8 * time_length(interval(date, max(date)), "week") /
              time_length(interval(min(date), max(date)), "week"))),
      Carbohydrates = DietaryCarbohydrates *
        (1 + 0.165 *
           (0.6 + 0.8 * time_length(interval(date, max(date)), "week") /
              time_length(interval(min(date), max(date)), "week"))),
      Fat = DietaryFatTotal *
        (1 + 0.165 *
           (0.6 + 0.8 * time_length(interval(date, max(date)), "week") /
              time_length(interval(min(date), max(date)), "week"))),
      Protein = DietaryProtein *
        (1 + 0.165 *
           (0.6 + 0.8 * time_length(interval(date, max(date)), "week") /
              time_length(interval(min(date), max(date)), "week")))
    ) %>%
    dplyr::select(-contains("Dietary")) %>%
    pivot_longer(!date, names_to = "metric", values_to = "value") %>%
    join_dates(multiple_metrics = TRUE, type = "nutrition") %>%
    filter(date < ymd(Sys.Date()))
  
  return(data)
}

wrangle_energy_data <- function(data){
  # wrangles energy data
  #percent_time_to_earliest_date <- function(date, min_date, max_date) {
  #  
  #}
  
  data <- data %>%
    clean_health_data() %>%
    filter(
      end_date >= '2020/12/24' &
        type %in% c(
          'BasalEnergyBurned',
          'ActiveEnergyBurned',
          'DietaryEnergyConsumed'
        )
    ) %>%
    dplyr::select(date, type, value) %>%
    dplyr::group_by(date, type) %>%
    dplyr::summarise(value = sum(value)) %>%
    ungroup() %>%
    spread(type, value) %>%
    mutate(
      ActiveEnergyBurned = ActiveEnergyBurned,
      DietaryEnergyConsumed = DietaryEnergyConsumed *
        (1 + 0.165 *
        (0.6 + 0.8 * time_length(interval(date, max(date)), "week") /
        time_length(interval(min(date), max(date)), "week"))),
      EnergyBurned = ActiveEnergyBurned + BasalEnergyBurned,
      EnergyConsumed = DietaryEnergyConsumed,
      EnergyDeficit = EnergyBurned - EnergyConsumed,
      EnergyDeficitPct = (EnergyDeficit / EnergyBurned)#,
      #EnergyDeficitCat = if_else(EnergyDeficit > 0, 'Deficit', 'Surplus')
    ) %>%
    filter(EnergyConsumed > 0) %>%
    dplyr::select(-c(ActiveEnergyBurned, BasalEnergyBurned, DietaryEnergyConsumed)) %>%
    pivot_longer(!date, names_to = "metric", values_to = "value") %>%
    mutate(
      positive_value = case_when(
        value > 0 ~ "Deficit",
        value <= 0 ~ "Surplus",
        TRUE ~ NA_character_
      ),
      metric = case_when(
        #metric == "DietaryProtein" ~ "Protein",
        #metric == "DietarySugar" ~ "Sugar",
        metric == "EnergyBurned" ~ "Calorie expenditure",
        metric == "EnergyConsumed" ~ "Calorie intake",
        metric == "EnergyDeficit" ~ "Calorie deficit (absolute)",
        metric == "EnergyDeficitPct" ~ "Calorie deficit (relative)",
        TRUE ~ metric
      )
    ) %>%
    join_dates(multiple_metrics = TRUE, type = "energy") #%>%
    #filter(date < max(date))
  
  return(data)
}

clean_exercise_data <- function(data){
  # cleans exercise data
  data <- data %>%
    clean_names() %>%
    mutate(date = as.Date(date)) %>%
    dplyr::select(date, exercise_name, set_order, weight, reps)
  
  return(data)
}

filter_exercise_data <- function(data, weight_data){
  # filters exercise data to remove probable dropsets and easier versions of machines
  data <- data %>%
    wrangle_exercise_data(., weight_data) %>%
    dplyr::filter(!(date < "2020-10-01" & exercise_name == "Triceps Dip (Assisted)")) %>%
    dplyr::filter(!(date < "2020-10-01" & exercise_name == "Lateral Raise (Machine)")) %>%
    dplyr::filter(!(date < "2020-10-01" & exercise_name == "Reverse Fly (Machine)")) %>%
    dplyr::filter(!(date < "2020-10-01" & exercise_name == "Lat Pulldown (Cable)")) %>%
    dplyr::filter(!(date < "2021-09-01" & exercise_name == "Bicep Curl (Dumbbell)")) %>%
    dplyr::filter(!(date < "2021-10-05" & exercise_name == "Hammer Curl (Dumbbell)")) %>%
    dplyr::filter(!(date < "2020-10-01" & exercise_name == "Calf Press on Seated Leg Press")) %>%
    dplyr::filter(!(exercise_name == "Bicep Curl (Machine)")) %>%
    dplyr::group_by(exercise_name) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      first_one_rep_max = dplyr::first(one_rep_max),
      cummax_one_rep_max = cummax(one_rep_max),
      cummax_hypertrophy_adjusted_one_rep_max = cummax(hypertrophy_adjusted_one_rep_max)
      ) %>%
    ungroup() %>%
    filter(one_rep_max >= (2/3) * first_one_rep_max) %>%
    dplyr::select(date, exercise_name, one_rep_max, cummax_one_rep_max)
  
  data
}

hypertrophy_weight <- function(reps, adjustment = 1.2, type = "step") {
  # calculates weight for a given rep count
  if (type == "step") {
    hypertrophy_weights <- data.frame(n_reps = seq(0, 50)) %>%
      mutate(
        weight = case_when(
          n_reps == 0 ~ 0,
          n_reps > 0 & n_reps <= 5 ~ 0.5,
          n_reps > 5 & n_reps <= 7 ~ 0.75,
          n_reps > 7 & n_reps <= 12 ~ 1,
          n_reps > 12 & n_reps <= 14 ~ 0.75,
          n_reps > 14 & n_reps <= 20 ~ 0.5,
          n_reps > 20 ~ 0
        )
      )
  } else if (type == "triangular") {
    hypertrophy_weights <- data.frame(n_reps = seq(0, 50)) %>%
      mutate(
        weight = case_when(
          n_reps == 0 ~ 0,
          n_reps > 0 & n_reps <= 10 ~ 0.5 + 0.05 * n_reps,
          n_reps > 10 & n_reps <= 30 ~ 1.5 - 0.05 * n_reps,
          n_reps > 30 ~ 0
        )
      )
  }
  
  hypertrophy_weight <- filter(hypertrophy_weights, n_reps == reps)[["weight"]]
  hypertrophy_weight <- hypertrophy_weight * adjustment
  
  return(hypertrophy_weight)
}

calculate_1RM <- function(data){
  # calculates 1RM for each set

  # I give the user the option to change the "reps deflator" `x` to negotiate
  # between two competing forces. On one hand, most people find that `x = 30`
  # better tracks their progress and the literal meaning of "estimated 1RM". On
  # the other, a lower `x` would get us closer to a universal weightlifting
  # metric since, all else equal, a 12-rep set with the same estimated 1RM as a
  # 1-rep set is likely to provide greater stimulus.

  unweighted_exercises <- c(
    "Chin Up",
    "Pull Up",
    "Wide Pull Up",
    "Back Extension",
    "Ab Coaster",
    "Battle Ropes",
    "Press Up",
    "Chest Dip",
    "Triceps Dip",
    "Curved Hack Squat",
    "Bulgarian Split Squat (Plate-Loaded)",
    "Knee Raise (Captain's Chair)",
    "Half-Hack Squat",
    "Goblet Squat (Kettlebell)",
    "Lunge (Dumbbell)"
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
        exercise_name %in% unweighted_exercises ~ body_mass * (1 + (reps / 25)),
        TRUE ~ NA_real_
      )
    )
  
  for (i in 1:nrow(data)) {
    data$hypertrophy_adjusted_one_rep_max[i] <- data$one_rep_max[i] * hypertrophy_weight(data$reps[i], 1)
  }
  
  data <- data %>%
    dplyr::group_by(exercise_name) %>%
    dplyr::mutate(
      cummax_one_rep_max = cummax(one_rep_max),
      cummax_hypertrophy_adjusted_one_rep_max = cummax(hypertrophy_adjusted_one_rep_max)
    )
  
  return(data)
}

wrangle_nonweightlifting_data <- function(data) {
  # wrangles non-weightlifting data

  nonweightlifting_activities <- c(
    "HKWorkoutActivityTypeSoccer",
    "HKWorkoutActivityTypeRunning",
    #"HKWorkoutActivityTypeSwimming",
    "HKWorkoutActivityTypeHiking",
    "HKWorkoutActivityTypeStairClimbing"
  )
  
  data <- data %>%
    clean_names() %>%
    filter(workout_activity_type %in% nonweightlifting_activities) %>%
    mutate(
      date = ymd(as.Date(creation_date)),
      total_energy_burned
    ) %>%
    dplyr::select(date, total_energy_burned)

  return(data)
}

wrangle_exercise_data <- function(data, weight_data){
  # wrangles exercise data
  data <- data %>%
    clean_exercise_data() %>%
    right_join(weight_data, by = "date") %>%
    arrange(date) %>%
    filter(!is.na(exercise_name)) %>%
    calculate_1RM()
  
  return(data)
}

wrangle_volume_data <- function(weightlifting_data, nonweightlifting_data, weight_data){
  # wrangles exercise volume data

  nonweightlifting_data <- wrangle_nonweightlifting_data(nonweightlifting_data)
  
  data <- weightlifting_data %>%
    wrangle_exercise_data(., weight_data) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      volume = sum(one_rep_max),
      hypertrophy_adjusted_volume = sum(hypertrophy_adjusted_one_rep_max)
      ) %>%
    pivot_longer(!date, names_to = "metric", values_to = "value") %>%
    join_dates(multiple_metrics = "TRUE", type = "volume") %>%
    mutate(value = replace_na(value, 0)) %>%
    full_join(nonweightlifting_data, by = "date") %>%
    mutate(
      total_energy_burned = case_when(
        is.na(total_energy_burned) ~ 0,
        TRUE ~ as.numeric(total_energy_burned)
      )
    ) %>%
    arrange(date)
  
  return(data)
}

unpack_mentalhealth_data <- function(){
  # unpack mental health data
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

wrangle_mentalhealth_data <- function(data, custom_entries, custom_symptoms){
  # wrangles mental health data

  # merge custom mentalhealth data, reshape to merge with main mentalhealth data
  custom_data <- custom_symptoms %>%
    dplyr::rename(SYMPTOM = ID) %>%
    full_join(custom_entries, by = "SYMPTOM") %>%
    dplyr::select(SYMPTOM = NAME, ID, ENTRY, VALUE) %>%
    mutate(
      days_since_last = ENTRY - max(ENTRY),
      date = ymd(Sys.Date()) + days(days_since_last)
    ) %>%
    select(-c(ID, ENTRY, days_since_last)) %>%
    pivot_wider(
      names_from = "SYMPTOM",
      values_from = "VALUE"
      ) %>%
    clean_names()

  data <- data %>%
    clean_names() %>%
    mutate(
      date = as_date(date_yyyy_mm_dd),
      # manually add to exceptional days
      elevated = case_when(
        grepl("private tour of Aruba’s national park", note) ~ 4.5,
        grepl("performed Part of Your World", note) ~ 5.0,
        grepl("Day after Duelling Pianos", note) ~ 4.0,
        grepl("magical island off of Vieques", note) ~ 5.0,
        grepl("jam session at Linden", note) ~ 4.5,
        grepl("hotel Halloween party", note) ~ 5.0,
        grepl("Won FTX fellowship", note) ~ 4.0,
        grepl("Did well in important exam", note) ~ 4.5,
        grepl("art gallery rationalist party", note) ~ 4.0,
        grepl("last full day in nyc", note) ~ 4.5,
        grepl("Received great grades", note) ~ 4.0,
        grepl("first day living in Nassau", note) ~ 5.0,
        grepl("day two in the Bahamas", note) ~ 4.0,
        grepl("Met up with FTX staff", note) ~ 4.0,
        grepl("big welcome dinner for FTX fellows", note) ~ 4.0,
        grepl("going to war with FTX fellows", note) ~ 4.0,
        grepl("amazing opportunity in the Bahamas", note) ~ 4.0,
        TRUE ~ as.numeric(elevated)
      ),
      # explanation for mental health metric:
      # https://www.wolframalpha.com/input/?i=0.01+*+x%5E4+-+0.01+%3D+1
      # intention is to get kind of exponential score between -1 and 1
      mental_health = (elevated^2.8675) - (
          (0.5*(anxiety^6.6582)) +
          (0.5*(depressed^6.6582))
      )^0.5,
      #mental_health = (elevated^2.8675) - (
      #  (0.1*(irritability^6.6582)) +
      #    (0.45*(anxiety^6.6582)) +
      #    (0.45*(depressed^6.6582))
      #)^0.5,
      #life_satisfaction = 
      sleep = replace(sleep, sleep == 0.0, NA),
      positive_value = case_when(
        mental_health > 0 ~ "Net positive",
        mental_health <= 0 ~ "Net negative",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-c(
      id,
      date_yyyy_mm_dd,
      #elevated,
      #irritability,
      #anxiety,
      #depressed,
      psychotic_symptoms,
      #note,
      menstrual_cycle,
      weight
    )) %>%
    full_join(custom_data, by = "date") %>%
    mutate(
      work_depth = replace_na(work_depth, 2),
      integrity = replace_na(integrity, 2),
      optimism = replace_na(optimism, 2),
      professional_mastery = replace_na(professional_mastery, 2),

      subjective_well_being = elevated + energy + fun + 
        dog_interaction - depressed - anxiety -
        conflict

      life_satisfaction = self_acceptance + positive_liberty + negative_liberty +
        value_alignment + health + security +
        relationship_satisfaction + achievement + learning +
        integrity + optimism - shame -
        suicidality,
      
      work_satisfaction = energy + value_alignment + security +
        achievement + learning + work_depth +
        professional_mastery
    )
}

wrangle_sleep_data <- function(data){
  # wrangles sleep data
  
}

wrangle_VAR_data <- function(energy_data, mentalhealth_data, volume_data) {
  # wrangles VAR data
  energy_data <- energy_data %>%
    dplyr::select(-positive_value) %>%
    pivot_wider(
      names_from = "metric",
      values_from = "value"
    )
  
  volume_data <- volume_data %>%
    pivot_wider(
      names_from = "metric",
      values_from = "value"
    )
  
  data <- energy_data %>%
    #full_join(nutrition_data, by = "date") %>%
    full_join(mentalhealth_data, by = "date") %>%
    full_join(volume_data, by = "date") %>%
    clean_names() %>%
    dplyr::select(
      date,
      #Protein, Sugar,
      calorie_expenditure, calorie_intake, #water,
      anxiety, depressed, elevated, sleep, mental_health, #therapy,
      volume#, total_energy_burned
    ) %>%
    filter(date >= ymd("2021-06-27") & date < max(date)) %>%
    mutate(
      calorie_expenditure = na.approx(calorie_expenditure, date),
      calorie_intake = na.approx(calorie_intake, date),
      sleep = na.approx(sleep, date),
      #calorie_intake = na.approx(calorie_intake, date),
      dplyr::across(everything(), ~ na.approx(.x, date))
    ) %>%
    dplyr::select(-date)
  
  return(data)
}
