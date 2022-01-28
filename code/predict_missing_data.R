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
packages <- c("tidyverse", "tidymodels")
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