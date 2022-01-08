#------------------------------------------------------------------------------#
# Exports tables by calling functions from `tables_helper.R`
# Author: Joel Becker

# Notes:
#
#------------------------------------------------------------------------------#


########################################################
######################## Set-up ########################
########################################################

# load libraries
packages <- c("tidyverse", "janitor", "lubridate", "XML", "data.table")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = TRUE)

# set wd
setwd("/Users/joel/projects/life-dashboard")

# load helper files
source("code/tables_helper.R")
source("path_names.R")


########################################################
####################### Load data ######################
########################################################

# load data
exercise_data_raw <- read.csv("raw_data/strong/strong.csv")

health_xml <- xmlParse("raw_data/apple_health_export/export.xml")
df_record <- XML:::xmlAttrsToDataFrame(health_xml["//Record"])
# df_activity <- XML:::xmlAttrsToDataFrame(health_xml["//ActivitySummary"])
# df_workout <- XML:::xmlAttrsToDataFrame(health_xml["//Workout"])
# df_clinical <- XML:::xmlAttrsToDataFrame(health_xml["//ClinicalRecord"])

mentalhealth_csv <- read.csv("raw_data/mental_health/csv/entry.csv")

print("-----")
print("Successfully loaded raw data!")


########################################################
##################### Wrangle data #####################
########################################################

# wrangle data
weight_data <- wrangle_weight_data(df_record)
exercise_data <- wrangle_exercise_data(exercise_data_raw, weight_data)
volume_data <- wrangle_volume_data(exercise_data)
energy_data <- wrangle_energy_data(df_record)
nutrition_data <- wrangle_nutrition_data(df_record)
mentalhealth_data <- wrangle_mentalhealth_data(mentalhealth_csv)
VAR_data <- wrangle_VAR_data(energy_data, mentalhealth_data, volume_data)

print("-----")
print("Successfully wrangled data!")


########################################################
####################### Save data ######################
########################################################

# save data
write_csv(weight_data, file = "temp/weight_data.csv")
write_csv(exercise_data, file = "temp/exercise_data.csv")
write_csv(volume_data, file = "temp/volume_data.csv")
write_csv(energy_data, file = "temp/energy_data.csv")
write_csv(nutrition_data, file = "temp/nutrition_data.csv")
write_csv(mentalhealth_data, file = "temp/mentalhealth_data.csv")
write_csv(VAR_data, file = "temp/VAR_data.csv")

print("-----")
print("Successfully saved wrangled data!")
