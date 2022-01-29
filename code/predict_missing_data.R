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
packages <- c(
  "tidyverse", 
  "tidymodels", 
  "ranger", #not sure
  "workflow", # ML workflow
  "parsnip", # specify model 
  "janitor", # clean names
  "rsample" # for test/train split
  )
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = TRUE)

# set wd
setwd("/Users/joel/projects/life-dashboard")

# source
source("path_names.R")


########################################################
#######################  ######################
########################################################

# WORKFLOW
# wrangle, export intermediate, predict, export final?

# MODEL
# calories out as a function of volume, steps, calories in (and previous calories? difficult because missing data. do most recent)
# weight as a function of calories in, predicted calories out, most recent weight

# step 1: get data on labels and features

# load data 
weight_data <- read_csv(file = "temp/weight_data.csv")
exercise_data <- read_csv(file = "temp/exercise_data.csv")
volume_data <- read_csv(file = "temp/volume_data.csv")
energy_data <- read_csv(file = "temp/energy_data.csv")
nutrition_data <- read_csv(file = "temp/nutrition_data.csv")
mentalhealth_data <- read_csv(file = "temp/mentalhealth_data.csv")
work_data <- read_csv(file = "temp/work_data.csv")
VAR_data <- read_csv(file = "temp/VAR_data.csv")

# wrangle data 
energy_data <- energy_data %>% 
  filter(metric %in% c("Calorie expenditure", "Calorie intake")) %>%
  dplyr::select(-positive_value) %>%
  pivot_wider(
      names_from = "metric",
      values_from = "value"
  ) %>%
  clean_names()

volume_data <- volume_data %>% 
  filter(metric == "volume") %>%
  dplyr::select(date, volume = value, total_energy_burned)

calorie_expenditure_data <- energy_data %>% 
  inner_join(volume_data, by = "date") %>% 
  drop_na()

# step 2: ML preparation

set.seed(1234)

calorie_expenditure_split <- initial_split(calorie_expenditure_data, prop = 0.7)
calorie_expenditure_train <- training(calorie_expenditure_split)
calorie_expenditure_test <- testing(calorie_expenditure_split)
calorie_expenditure_cv <- vfold_cv(calorie_expenditure_train)

calorie_expenditure_recipe <- calorie_expenditure_data %>% 
  recipe(
    calorie_expenditure ~ calorie_intake + volume + total_energy_burned,
    data = .
  ) %>% 
  #step_normalise(all_numeric()) %>% 
  step_impute_knn(all_predictors()) 

calorie_expenditure_model <- rand_forest(mode = "regression") %>% 
  set_engine("ranger", importance = "impurity")


calorie_expenditure_workflow <- workflow() %>% 
  add_recipe(calorie_expenditure_recipe) %>% 
  add_model(calorie_expenditure_model)

calorie_expenditure_fit <- calorie_expenditure_workflow %>% 
  last_fit(calorie_expenditure_split)

calorie_expenditure_performance <- calorie_expenditure_fit %>% 
  collect_metrics()

