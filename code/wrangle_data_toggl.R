#------------------------------------------------------------------------------#
# Functions for wrangling  t
# Author: Joel Becker

# Notes:
#
#------------------------------------------------------------------------------#


########################################################
######################## Set-up ########################
########################################################

# load libraries
packages <- c("tidyverse", "lubridate")
packages_for_toggl <- c("assertthat","dplyr","getPass","glue","httr","jsonlite","keyring","lubridate","magrittr","parsedate","prettyunits","purrr","rstudioapi")
new.packages <- packages_for_toggl[!(packages_for_toggl %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages_for_toggl, library, character.only = TRUE)

if (!requireNamespace("devtools")){install.packages("devtools", dependencies = TRUE)}
devtools::install_github("ThinkR-open/togglr")
library(togglr)

# set wd
setwd("/Users/joel/projects/life-dashboard")

# source
source("path_names.R")


########################################################
####################### Functions ######################
########################################################

get_toggl_data <- function(api_token = toggl_token, since = ymd_hms("2022-01-17-01-00-00-UTC"), until = Sys.time()){
  # largely attributed to https://github.com/ThinkR-open/togglr/blob/master/R/Get_time_entries.R
  url <- glue::glue("https://api.track.toggl.com/api/v8/time_entries?start_date={format_iso_8601(since)}&end_date={format_iso_8601(until)}")
  res <- content(GET(url,
    # verbose(),
    authenticate(api_token, "api_token"),
    encode = "json"
  ))

  if (length(res) == 0) {
    return(data.frame())
  }

  # extract list of tags
  tags <- map(res, ~ .x$tags)

  res <- res %>%
    # remove tags column, otherwise bind_rows() adds additional rows if
    # length(tags) > 1
    map(~ {
      .x$tags <- NULL
      return(.x)
    }) %>%
    bind_rows() %>%
    # add list column
    mutate(tags = tags)

  return(res)
}

clean_toggl_data <- function(toggl_data){
  # cleans toggl data

  # TODO: add ratio of work to break over time

  data <- toggl_data %>% 
    mutate(
      start = parse_iso_8601(start),
      duration = case_when(
        duration < 0 ~ as.integer(difftime(
          Sys.time(), start,
          units = "sec"
        )),
        TRUE ~ duration
      ),
      pretty_duration = prettyunits::pretty_sec(duration),
      stop = case_when(
        is.na(stop) ~ "0",
        TRUE ~ stop
      ),
      stop = parse_iso_8601(stop)
    ) %>%
    left_join(get_project_id_and_name(toggl_token), by = c("pid" = "id")) %>%
    select(
      start,
      stop,
      pretty_duration,
      duration,
      project_name,
      description#,
      #pid,
      #wid,
      #everything()
    ) %>%
    slice(nrow(.):1)
  
  return(data)
}

wrangle_toggl_data <- function(api_token = toggl_token, since = ymd_hms("2022-01-17-01-00-00-UTC"), until = Sys.time()){
  # wrangles toggl data 

  data <- get_toggl_data(api_token, since, until) %>%
    clean_toggl_data()
  
  return(data)
}