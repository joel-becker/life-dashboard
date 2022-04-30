toggl_token <- "4cae764f9f916efe1042101c2ee8efc0"

app_packages <- c(
    "shiny",
    "ggplot2",
    "forcats", # factor reordering
    "ggallin", # pseudo-log scale
    "pracma", # exponential moving average
    "scales", # log scale breaks
    "ggcorrplot", # correlation plot
    "plyr", # for round_any()
    "rsconnect",
    "shinycssloaders", # shiny related
    "bslib", # minty theme
    "markdown",
    "tidyverse",
    "lubridate", # handling dates
    #"tidyquant", # plot moving averages
    "janitor", # clean variable names
    "zoo",
    "rlang", # quote variables
    "vars", # VARs
    "devtools" # extract_varirf fn
    )
# new.packages <- app_packages[!(app_packages %in% installed.packages()[, "Package"])]
# if(length(new.packages)) install.packages(new.packages)