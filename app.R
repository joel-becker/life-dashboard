#------------------------------------------------------------------------------#
# Basic app
# Author: Joel Becker

# Notes:
#
#------------------------------------------------------------------------------#


########################################################
######################## Set-up ########################
########################################################

# load libraries
library("shiny")
library("ggplot2")
library("forcats") # factor reordering
library("ggallin") # pseudo-log scale
library("pracma") # exponential moving average
library("scales") # log scale breaks
library("ggcorrplot") # correlation plot
library("plyr") # for round_any()
library("rsconnect")
library("shinycssloaders") # shiny related
library("bslib") # minty theme
library("markdown")
library("tidyverse")
library("lubridate") # handling dates
#library("slider") # slider functions
library("tidyquant") # plot moving averages
library("janitor") # clean variable names
library("zoo")
library("rlang") # quote variables
library("vars") # VARs
library(devtools) # extract_varirf fn
source_url(paste0(
  "https://raw.githubusercontent.com/anguyen1210/",
  "var-tools/master/R/extract_varirf.R"
  ))


# load helper files
# source("charts_helper.R")

# don't load functions in R folder
# TODO: maybe change this later to write chart
options(shiny.autoload.r = FALSE)

# load data
if (!("exercise_data.csv" %in% list.files("temp")) ||
    !("volume_data.csv" %in% list.files("temp")) ||
    !("energy_data.csv" %in% list.files("temp")) ||
    !("nutrition_data.csv" %in% list.files("temp")) ||
    !("mentalhealth_data.csv" %in% list.files("temp")) ||
    !("VAR_data.csv" %in% list.files("temp"))) {
  source("tables_exporter.R")
} else {
  exercise_data <- read_csv("temp/exercise_data.csv")
  volume_data <- read_csv("temp/volume_data.csv")
  energy_data <- read_csv("temp/energy_data.csv")
  nutrition_data <- read_csv("temp/nutrition_data.csv")
  mentalhealth_data <- read_csv("temp/mentalhealth_data.csv")
  VAR_data <- read_csv("temp/VAR_data.csv")
}

# button_color_css <- "
## DivCompClear, #FinderClear, #EnterTimes{
# /* Change the background color of the update button
# to blue. */
# background: DodgerBlue;
# /* Change the text size to 15 pixels. */
# font-size: 15px;
# }"

weighted_average <- function(xs, weights) {
  sum(xs*weights) / sum(weights)
}


rolling_weighted_average <- function(xs, date, target_date, epsilon = 0.0035) {
  time_diff <- as.numeric(abs(difftime(target_date, date, units = "day")))
  weights <- exp(-epsilon*time_diff)
  
  weighted_average(xs, weights)
}

generate_lags <- function(var, n=10){
  # TODO: get this working with multiple variables at once
  var <- enquo(var)
  
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("%s lag %02d", quo_text(var), indices))
  
}

semipseudolog_trans <- function(base) {
  scales::trans_new(
    name      = 'pseudo log10',
    transform = function(x) asinh(x/2)/log(base),
    inverse   = function(y) 2 * sinh(y * log(base)),
    domain    = c(-Inf,Inf))
}

#custom_date_axis <- function(data) {
#  interval_length <- interval(ymd(min(data$date)), ymd(max(data$date)))
#  interval_duration_months <- interval_length %/% months(1)
#  
#  # how long is between approximately 6 break points?
#  months_between_breaks <- ceiling(interval_duration_months / 6)
#  
#  first_date_month <- month(min(data$date))
#  first_date_year  <- year(min(data$date))
#  first_date       <- ymd(paste0(first_date_year, "-", first_date_month, "-01"))
#  last_date_month  <- month(max(data$date))
#  last_date_year   <- year(max(data$date))
#  last_date        <- ymd(paste0(last_date_year, "-", last_date_month, "-01"))
#  
#  breaks <- seq(
#    first_date,
#    last_date,
#    by = paste0(months_between_breaks, " month")
#    #length.out = 8
#  )
#  
#  interval_endseq_to_lastdate <- interval(
#    max(breaks), last_date
#    ) %/% months(1)
#  
#  breaks <- breaks + months(interval_endseq_to_lastdate)
#  
#  labels = c(
#    paste0(c(
#      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
#      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
#      " '21")
#  )
#  
#  return(breaks)
#}

ui <- fluidPage(
  # tags
  tags$head(includeHTML(("google-analytics.html"))),

  # theme
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  # page
  navbarPage(
    "Joel's life dashboard",
    
    ## exercise sub-menu
    navbarMenu(
      "Exercise",
      icon = icon("dumbbell"),
      # tags$style(button_color_css),
      
      ### exercise progression
      tabPanel(
        "Exercise progression",
        fluid = TRUE,
        # icon = icon("dumbbell"),
        # tags$style(button_color_css),
        sidebarLayout(
          sidebarPanel(
            titlePanel("Options"),
            selectInput(
              "exercise_name",
              label = "Choose exercises",
              exercise_data %>%
                add_count(exercise_name) %>%
                dplyr::group_by(exercise_name) %>%
                dplyr::mutate(last_exercise = max(date)) %>%
                ungroup() %>%
                filter(n >= 40) %>%
                #filter(n >= 40 & last_exercise > Sys.Date() - months(6)) %>%
                arrange(desc(n)) %>%
                pull(exercise_name) %>%
                unique(),
              multiple = TRUE
            ),
            radioButtons(
              "radiobuttons_expscale",
              label = "Scale type",
              choices = c("Exponential" = "Exponential", "Linear" = "Linear"),
              selected = "Exponential"
            )
          ),
          mainPanel(
            h1("Estimated one-rep-max by selected exercises"),
            plotOutput(
              "onerepmax_plot" # ,
              # width = 8
            ),
            includeMarkdown("markdown/onerepmax.md")
          )
        )
      ),
      tabPanel(
        "Volume",
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            titlePanel("Options"),
            numericInput(
              "rollavg_length_volume",
              label = "Rolling average window length",
              value = 0.05,
              min = 0.01,
              max = 1,
              step = 0.01
            ),
            radioButtons(
              "volume_metric",
              label = "Choose volume metric",
              choices = c(
                "Aggregate 1RM" = "volume",
                "Aggregated hypertrophy-adjusted 1RM" = "hypertrophy_adjusted_volume"
              ),
              selected = "volume"
            ),
            radioButtons(
              "radiobuttons_calories",
              label = "Include cardio calories",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = "No"
            )
          ),
          mainPanel(
            h1("Volume"),
            plotOutput(
              "volume_plot" # ,
              # width = 8
            ),
            includeMarkdown("markdown/volume.md")
          )
        )
      )
    ),
    ## correlations sub-menu
    navbarMenu(
      "Diet",
      icon = icon("utensils"),
      
      ### high-level energy
      tabPanel(
        "Energy",
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            titlePanel("Options"),
            selectInput(
              "energy_metric",
              label = "Choose energy metric",
              c(
                energy_data %>%
                  pull(metric) %>%
                  unique()
              ),
              selected = "Calorie deficit (absolute)"
            ),
            numericInput(
              "rollavg_length_energy",
              label = "Exponential mean hyperparameter value",
              value = 0.15,
              min = 0.01,
              max = 1,
              step = 0.01
            )
          ),
          mainPanel(
            h1("Energy"),
            plotOutput(
              "energy_plot" # ,
              # width = 8
            ),
            includeMarkdown("markdown/energy.md")
          )
        )
      ),
      tabPanel(
        "Nutrition",
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            titlePanel("Nutrition"),
            selectInput(
              "nutrition_metric",
              label = "Choose nutrition variable",
              c(
                nutrition_data %>%
                  pull(metric) %>%
                  unique()
              ),
              multiple = FALSE,
              selected = "Protein"
            ),
            numericInput(
              "rollavg_length_nutrition",
              label = "Exponential mean hyperparameter value",
              value = 0.15,
              min = 0.01,
              max = 1,
              step = 0.01
            )
          ),
          mainPanel(
            h1("Nutrition plot"),
            div(
              style = "position:relative",
              plotOutput(
                "nutrition_plot"
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Mental health",
      fluid = TRUE,
      icon = icon("grin-beam-sweat"),
      sidebarLayout(
        sidebarPanel(
          titlePanel("Options"),
          numericInput(
            "rollavg_length_mentalhealth",
            label = "Exponential moving average hyperparameter value",
            value = 0.2,
            min = 0.01,
            max = 1,
            step = 0.01
          )
        ),
        mainPanel(
          h1("Mental health"),
          div(
            style = "position:relative",
            plotOutput(
              "mentalhealth_plot"#,
              #hover = hoverOpts(
              #  "plot_hover",
              #  delay = 100,
              #  delayType = "debounce"
              #)
            )#,
            #uiOutput("hover_info")
          ),
          includeMarkdown("markdown/mentalhealth.md")
        )
      )
    ),
    ## correlations sub-menu
    navbarMenu(
      "Correlations",
      icon = icon("yin-yang"),
      
      ### correlations
      tabPanel(
        "Life correlations",
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            titlePanel("Test"),
            numericInput(
              "lifecorrelations_numberlags",
              label = "Number of lags of selected variables",
              value = 0,
              min = 0,
              max = 10,
              step = 1
            )
          ),
          mainPanel(
            h1("Correlation between life aspects"),
            div(
              style = "position:relative",
              plotOutput(
                "corr_plot"
              )
            )
          )
        )
      ),
      tabPanel(
        "Impulse response",
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            titlePanel("Impulse response function variables"),
            selectInput(
              "VAR_impulse",
              label = "Choose impulse variable",
              VAR_data %>% names(),
              multiple = FALSE,
              selected = "mental_health"
            ),
            selectInput(
              "VAR_response",
              label = "Choose response variable",
              VAR_data %>% names(),
              multiple = FALSE,
              selected = "volume"
            )
          ),
          mainPanel(
            h1("Impulse response function"),
            div(
              style = "position:relative",
              plotOutput(
                "VAR_plot"
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$onerepmax_plot <- renderPlot({
    #data <- exercise_data %>%
    #  group_by(exercise_name) %>%
    #  mutate(first_one_rep_max = first(one_rep_max)) %>%
    #  ungroup() %>%
    #  filter(one_rep_max >= (2/3) * first_one_rep_max) %>%
    #  select(date, exercise_name, one_rep_max, cummax_one_rep_max)
    exercise_names <- input$exercise_name
    radiobuttons_expscale <- input$radiobuttons_expscale
    
    if (length(exercise_names) == 0) {
      exercise_names <- c("Bench Press (Barbell)", "Chin Up")
    }
    
    filtered_data <- exercise_data %>%
      filter(exercise_name %in% exercise_names)
    
    plot <- ggplot(
      filtered_data,
      aes(x = date, y = one_rep_max, group = exercise_name, colour = exercise_name)
    ) +
      geom_line(aes(y = cummax_one_rep_max), size = 1.5) +
      geom_point(size = 3, alpha = 1 / 3) +
      # geom_line(stat="smooth", method = "lm", formula = y ~ poly(x, 2), se=FALSE, size=1.5, alpha=1/3) +
      
      ylim(0, NA) +
      ylab("Estimated one-rep-max (lbs)") +
      scale_color_manual(
        values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666"),
        aesthetics = c("colour", "fill")
      ) +
      #scale_x_date(
      #  breaks = as.Date(c(
      #    "2020-10-01",
      #    "2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01"
      #  )),
      #  labels = c("Oct '20", "Jan '21", "Apr '21", "Jul '21", "Oct '21"),
      #  limits = c(min(filtered_data$date), max(filtered_data$date)) # ,
      #  # title("Date")
      #) +
      scale_x_date(
        date_minor_breaks = "1 month",
        date_labels =  "%b %Y"
      ) +
      scale_y_continuous(
        breaks = seq(0, 400, round_any(max(filtered_data$one_rep_max) / 16, 8, f = ceiling))
      ) +
      # coord_trans(y = "exp") +
      
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 12),
        # axis.title.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(2, "cm"), # change legend key size
        legend.key.height = unit(2, "cm"), # change legend key height
        legend.key.width = unit(2, "cm"), # change legend key width
        legend.text = element_text(size = 14)
        # panel.grid.minor.y = element_blank()
      ) +
      guides(
        color = guide_legend(
          nrow = ceiling(length(exercise_names) / 2),
          byrow = TRUE
        )
      )
    
    if (length(exercise_names) == 1) {
      plot <- plot +
        theme(legend.position = "none")
    }
    
    if (radiobuttons_expscale == "Exponential") {
      plot <- plot +
        coord_trans(y = scales::exp_trans(1.008))
    }
    
    return(plot)
  })
  
  output$volume_plot <- renderPlot({
    data <- volume_data
    rollavg_length_volume <- input$rollavg_length_volume
    metric_name <- input$volume_metric
    include_cardio_calories <- input$radiobuttons_calories
    
    if (include_cardio_calories == "Yes") {
      data <- data %>%
        mutate(
          value = value + (total_energy_burned * 3)
          )
    }
    
    plot <- data %>%
      filter(
        metric %in% metric_name
      ) %>%
      mutate(
        exp_moving_avg = map_dbl(
          date,
          function(d) rolling_weighted_average(
            value,
            date,
            d,
            epsilon = rollavg_length_volume
          )
        )
      ) %>%
      ggplot(aes(x = date, y = value)) +
      geom_col(alpha = 1 / 3, fill = "#1b9e77") +
      geom_line(
        aes(x = date, y = exp_moving_avg),
        colour = "#7570b3",
        fill = NULL,
        size = 1
        ) +
      scale_fill_manual(
        values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
      ) +
      #scale_x_date(
      #  breaks = as.Date(c(
      #    "2020-10-01",
      #    "2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01"
      #  )),
      #  labels = c("Oct '20", "Jan '21", "Apr '21", "Jul '21", "Oct '21"),
      #  limits = c(min(data$date), max(data$date)),
      #  expand = c(0, 0)
      #  # title("Date")
      #) +
      scale_x_date(
        date_minor_breaks = "1 month",
        date_labels =  "%b %Y",
        expand = c(0,0)
        ) +
      scale_y_continuous(
        # trans=scales::pseudo_log_trans(base = 2),
        expand = c(0, 0)
      ) +
      
      # ylim(min(data$value) - 1, max(data$value) + 1) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 12),
        # axis.title.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 24),
        legend.title = element_blank()
        #panel.grid.minor.x = element_blank() # ,
        # panel.grid.minor.y = element_blank()
      )
    
    if (metric_name == "volume") {
      plot <- plot +
        labs(y = "Volume (in estimated one-rep-max lb units)")
    } else if (metric_name == "hypertrophy_adjusted_volume") {
      plot <- plot +
        labs(y = "Hypertrophy-adjusted volume")
    }
    
    return(plot)
  })
  
  output$energy_plot <- renderPlot({
    data <- energy_data
    metric_names <- input$energy_metric
    rollavg_length_energy <- input$rollavg_length_energy
    
    data <- data %>%
      filter(
        metric %in% metric_names
      ) %>%
      drop_na(value) %>%
      mutate(
        value = as.numeric(value),
        exp_moving_avg = map_dbl(
          date,
          function(d) rolling_weighted_average(
            value,
            date,
            d,
            epsilon = rollavg_length_energy
          )
        )
      ) %>%
      full_join(
        data.frame(
          date = seq(min(.$date), max(.$date), by = "days")
        ),
        by = "date"
      )
    
    data_without_NA <- data %>%
      filter(!is.na(value))
    
    plot <- data %>%
      ggplot(aes(x = date, y = value))
    
    if ("Calorie deficit (relative)" %in% metric_names) {
      plot <- data %>%
        ggplot(aes(x = date, y = value, fill = positive_value)) +
        geom_col(alpha = 1 / 3)
    } else if ("Calorie deficit (absolute)" %in% metric_names) {
      plot <- data %>%
        ggplot(aes(x = date, y = value, fill = positive_value)) +
        geom_col(alpha = 1 / 3)
    } else if ("Protein" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        theme(legend.position = "none")
    } else if ("Sugar" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        theme(legend.position = "none")
    } else if ("Calorie expenditure" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        theme(legend.position = "none")
    } else if ("Calorie intake" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        theme(legend.position = "none")
    }
    
    plot <- plot +
      geom_line(
        aes(
          y = exp_moving_avg,
          fill = NULL
        ),
        size = 1,
        colour = "#7570b3"
      ) +
      
      scale_fill_manual(
        values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
      ) +
      #scale_x_date(
      #  breaks = as.Date(c(
      #    paste0("2021-", c(paste0("0", 1:9), paste0(10:12)), "-01")
      #  )),
      #  labels = c(
      #    paste0(c(
      #      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      #      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      #      " '21")
      #  ),
      #  limits = c(min(data_without_NA$date), max(data_without_NA$date)),
      #  expand = c(0, 0)
      #  # title("Date")
      #) +
      scale_x_date(
        date_minor_breaks = "1 month",
        date_labels =  "%b %Y",
        expand = c(0,0)
      ) +
      
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14),
        # axis.title.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"), # change legend key size
        legend.key.height = unit(1, "cm"), # change legend key height
        legend.key.width = unit(1.5, "cm"), # change legend key width
        legend.text = element_text(size = 14)
      )
    
    if ("Calorie deficit (relative)" %in% metric_names) {
      plot <- plot +
        labs(y = "Percentage calorie deficit/surplus") +
        scale_y_continuous(
          labels = scales::percent,
          breaks = seq(-1, 1, 0.2),
          expand = c(0, 0)
        )
    } else if ("Calorie deficit (absolute)" %in% metric_names) {
      plot <- plot +
        labs(y = "Calorie deficit/surplus") +
        scale_y_continuous(
          breaks = seq(-1500, 1500, 500),
          expand = c(0, 0)
        )
    } else if ("Protein" %in% metric_names) {
      plot <- plot +
        labs(y = "Protein consumed (grams)") +
        scale_y_continuous(
          breaks = seq(0, 300, 50),
          expand = c(0, 0)
        )
    } else if ("Sugar" %in% metric_names) {
      plot <- plot +
        labs(y = "Sugar consumed (grams)") +
        scale_y_continuous(
          breaks = seq(0, 300, 50),
          expand = c(0, 0)
        )
    } else if ("Calorie expenditure" %in% metric_names) {
      plot <- plot +
        labs(y = "Calories burned") +
        scale_y_continuous(
          breaks = seq(0, 4500, 1000),
          expand = c(0, 0)
        )
    } else if ("Calorie intake" %in% metric_names) {
      plot <- plot +
        labs(y = "Calories consumed") +
        scale_y_continuous(
          breaks = seq(0, 4500, 1000),
          expand = c(0, 0)
        )
    }
    
    return(plot)
  })
  
  output$nutrition_plot <- renderPlot({
    data <- nutrition_data
    metric_names <- input$nutrition_metric
    rollavg_length_nutrition <- input$rollavg_length_nutrition
    
    data <- data %>%
      filter(
        metric %in% metric_names
      ) %>%
      drop_na(value) %>%
      mutate(
        value = as.numeric(value),
        exp_moving_avg = map_dbl(
          date,
          function(d) rolling_weighted_average(
            value,
            date,
            d,
            epsilon = rollavg_length_nutrition
          )
        )
      ) %>%
      full_join(
        data.frame(
          date = seq(min(.$date), max(.$date), by = "days")
        ),
        by = "date"
      )
    
    data_without_NA <- data %>%
      filter(!is.na(value))
    
    plot <- data %>%
      ggplot(aes(x = date, y = value))
    
    if ("Protein" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        theme(legend.position = "none")
    } else if ("Sugar" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        theme(legend.position = "none")
    } else if ("Fat" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        theme(legend.position = "none")
    } else if ("Carbohydrates" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        theme(legend.position = "none")
    } else if ("Water" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        theme(legend.position = "none")
    }
    
    plot <- plot +
      geom_line(
        aes(
          y = exp_moving_avg,
          fill = NULL
        ),
        size = 1,
        colour = "#7570b3"
      ) +
      
      scale_fill_manual(
        values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
      ) +
      scale_x_date(
        date_minor_breaks = "1 month",
        date_labels =  "%b %Y",
        expand = c(0,0)
      ) +
      
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14),
        # axis.title.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"), # change legend key size
        legend.key.height = unit(1, "cm"), # change legend key height
        legend.key.width = unit(1.5, "cm"), # change legend key width
        legend.text = element_text(size = 14)
      )
    
    if ("Protein" %in% metric_names) {
      plot <- plot +
        labs(y = "Protein consumed (grams)") +
        scale_y_continuous(
          breaks = seq(0, 300, 50),
          expand = c(0, 0)
        )
    } else if ("Sugar" %in% metric_names) {
      plot <- plot +
        labs(y = "Sugar consumed (grams)") +
        scale_y_continuous(
          breaks = seq(0, 300, 50),
          expand = c(0, 0)
        )
    } else if ("Fat" %in% metric_names) {
      plot <- plot +
        labs(y = "Fat consumed (grams)") +
        scale_y_continuous(
          breaks = seq(0, 300, 50),
          expand = c(0, 0)
        )
    } else if ("Carbohydrates" %in% metric_names) {
      plot <- plot +
        labs(y = "Carbohydrates consumed (grams)") +
        scale_y_continuous(
          breaks = seq(0, 800, 100),
          expand = c(0, 0)
        )
    } else if ("Water" %in% metric_names) {
      plot <- plot +
        labs(y = "Water consumed (ml)") +
        scale_y_continuous(
          breaks = seq(0, 6000, 1000),
          expand = c(0, 0)
        )
    }
    
    return(plot)
  })
  
  output$mentalhealth_plot <- renderPlot({
    data <- mentalhealth_data
    rollavg_length_mentalhealth <- input$rollavg_length_mentalhealth
    
    plot <- data %>%
      mutate(
        positive_value = fct_relevel(
          positive_value,
          "Net positive",
          "Net negative"
          ),
        exp_moving_avg = map_dbl(
          date,
          function(d) rolling_weighted_average(
            mental_health,
            date,
            d,
            epsilon = rollavg_length_mentalhealth
          )
        )
        ) %>%
      ggplot(aes(x = date, y = mental_health, fill = positive_value)) +
      geom_col(alpha = 1 / 3) +
      geom_line(
        aes(
          y = exp_moving_avg,
          fill = NULL
        ),
        size = 1,
        colour = "#7570b3",
        linetype = "solid"
      ) +
      labs(y = "Mental health score") +
      scale_fill_manual(
        values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
      ) +
      scale_x_date(
        date_minor_breaks = "1 month",
        date_labels =  "%b %Y",
        expand = c(0,0)
      ) +
      scale_y_continuous(
        lim = c(-100, 100),
        breaks = seq(-100, 100, 25),
        # breaks = c(-100, -80, -40, -20, -10, -5, 0, 5, 10, 20, 40, 80, 100),
        #trans = semipseudolog_trans(1.002),
        expand = c(0, 0)
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14),
        # axis.title.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"), # change legend key size
        legend.key.height = unit(1, "cm"), # change legend key height
        legend.key.width = unit(1.5, "cm"), # change legend key width
        legend.text = element_text(size = 14)
      )
    
    return(plot)
  })
  
  output$corr_plot <- renderPlot({
    # standardise variables to get partial correlations
    # https://zief0002.github.io/modeling/cor.html
    
    # then create correlation plots like this 
    # https://www.khstats.com/blog/corr-plots/corr-plots/
    
    # TODO: use this to reorder?
    # https://stackoverflow.com/questions/57861765/reorder-axis-labels-of-correlation-matrix-plot
    lifecorrelations_numberlags <- input$lifecorrelations_numberlags
    
    energy_data <- energy_data %>%
      pivot_wider(
        names_from = "metric",
        values_from = "value"
      )
    
    volume_data <- volume_data %>%
      pivot_wider(
        names_from = "metric",
        values_from = "value"
      )
    
    # gather data together
    data <- energy_data %>%
      full_join(mentalhealth_data, by = "date") %>%
      full_join(volume_data, by = "date")
    
    # plot correlation plot
    
    plot <- data %>%
      dplyr::select(
        irritability,
        anxiety,
        depressed,
        elevated,
        sleep,
        mental_health,
        volume,
        "Calorie expenditure",
        "Calorie intake",
        "Calorie deficit (absolute)"#,
        #Protein,
        #Sugar
      ) %>%
      mutate(
        !!!generate_lags(sleep, lifecorrelations_numberlags),
        !!!generate_lags(mental_health, lifecorrelations_numberlags),
        !!!generate_lags(volume, lifecorrelations_numberlags),
        !!!generate_lags("Calorie expenditure", lifecorrelations_numberlags),
        !!!generate_lags("Calorie intake", lifecorrelations_numberlags)
      ) %>%
      relocate(
        contains("sleep"),
        contains("mental_health"),
        contains("irritability"),
        contains("anxiety"),
        contains("depressed"),
        contains("elevated"),
        contains("volume"),
        contains("Calorie expenditure"),
        contains("Calorie intake"),
        contains("Calorie deficit (absolute)")#,
        #contains("Protein"),
        #contains("Sugar")
      ) %>%
      cor(use = "complete.obs") %>%
      ggcorrplot(
        .,
        method = "circle"#,
        #hc.order = TRUE,
        #type = "lower"#,
        #p.mat = p.mat
      )
    
    return(plot)
  })
  
  output$VAR_plot <- renderPlot({
    VAR_impulse <- input$VAR_impulse
    VAR_response <- input$VAR_response
    
    VAR <- VAR(VAR_data, type = "both")
    irf <- irf(
      VAR,
      impulse = VAR_impulse,
      response = VAR_response,
      n.ahead = 7,
      ortho = TRUE,
      cumulative = FALSE,
      boot = TRUE,
      ci = 0.8,
      runs = 100
    )
    irf <- extract_varirf(irf)
    
    VAR_impulse <- tolower(VAR_impulse)
    VAR_response <- tolower(VAR_response)
    y <- paste0("irf_", VAR_impulse, "_", VAR_response)
    ymin <- paste0("lower_", VAR_impulse, "_", VAR_response)
    ymax <- paste0("upper_", VAR_impulse, "_", VAR_response)
    
    plot <- irf %>% 
      ggplot(aes_string(
        x = "period",
        y = y,
        ymin = ymin,
        ymax = ymax
      )) +
      geom_hline(yintercept = 0, color="grey") +
      geom_ribbon(fill = "#1b9e77", alpha=0.2) +
      geom_line(color = "#1b9e77") +
      theme_minimal() +
      ggtitle(paste0("Orthogonal impulse response, ", VAR_impulse, " - ", VAR_response)) +
      ylab(paste0("Response: ", VAR_response)) +
      xlab("Period") +
      theme(plot.title = element_text(size = 11, hjust=0.5),
            axis.title.y = element_text(size=11))
    
    plot
  })
  
  #output$hover_info <- renderUI({
  #  hover <- input$plot_hover
  #  point <- nearPoints(
  #    mentalhealth_data,
  #    hover,
  #    threshold = 5,
  #    maxpoints = 1,
  #    addDist = TRUE
  #  )
  #  if (nrow(point) == 0) {
  #    return(NULL)
  #  }
  #  
  #  # calculate point position INSIDE the image as percent of total dimensions
  #  # from left (horizontal) and from top (vertical)
  #  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  #  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  #  
  #  # calculate distance from left and bottom side of the picture in pixels
  #  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  #  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  #  
  #  # create style property fot tooltip
  #  # background color is set so tooltip is a bit transparent
  #  # z-index is set so we are sure are tooltip will be on top
  #  style <- paste0(
  #    "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
  #    "left:", left_px + 2, "px; top:", top_px + 2, "px;"
  #  )
  #  
  #  # actual tooltip created as wellPanel
  #  wellPanel(
  #    style = style,
  #    p(HTML(paste0(
  #      "<b> Car: </b>", rownames(point), "<br/>",
  #      "<b> mpg: </b>", point$mpg, "<br/>",
  #      "<b> hp: </b>", point$hp, "<br/>",
  #      "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px
  #    )))
  #  )
  #})
}

shinyApp(ui, server)
