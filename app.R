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

# load helper files
# source("charts_helper.R")

# don't load functions in R folder
# TODO: maybe change this later to write chart
options(shiny.autoload.r = FALSE)

# load data
if (!("exercise_data.csv" %in% list.files("temp")) ||
    !("volume_data.csv" %in% list.files("temp")) ||
    !("diet_data.csv" %in% list.files("temp")) ||
    !("mentalhealth_data.csv" %in% list.files("temp"))) {
  source("tables_exporter.R")
} else {
  exercise_data <- read_csv("temp/exercise_data.csv")
  volume_data <- read_csv("temp/volume_data.csv")
  diet_data <- read_csv("temp/diet_data.csv")
  mentalhealth_data <- read_csv("temp/mentalhealth_data.csv")
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
    set_names(sprintf("%s_lag_%02d", quo_text(var), indices))
  
}

semipseudolog_trans <- function(base) {
  scales::trans_new(
    name      = 'pseudo log10',
    transform = function(x) asinh(x/2)/log(base),
    inverse   = function(y) 2 * sinh(y * log(base)),
    domain    = c(-Inf,Inf))
}



ui <- fluidPage(
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
                group_by(exercise_name) %>%
                mutate(last_exercise = last(date)) %>%
                ungroup() %>%
                filter(n >= 50 & last_exercise > Sys.Date() - months(6)) %>%
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
            includeMarkdown("onerepmax.md")
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
              "radiobuttons_calories",
              label = "Include cardio calories",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = "No"
            )
          ),
          mainPanel(
            h1("Rolling average volume"),
            plotOutput(
              "volume_plot" # ,
              # width = 8
            ),
            includeMarkdown("volume.md")
          )
        )
      )
    ),
    tabPanel(
      "Diet",
      fluid = TRUE,
      icon = icon("utensils"),
      sidebarLayout(
        sidebarPanel(
          titlePanel("Options"),
          selectInput(
            "diet_metric",
            label = "Choose diet metric",
            c(
              diet_data %>%
                pull(metric) %>%
                unique()
            ),
            selected = "EnergyDeficit"
          ),
          numericInput(
            "rollavg_length_diet",
            label = "Exponential mean hyperparameter value",
            value = 0.2,
            min = 0.01,
            max = 1,
            step = 0.01
          )
        ),
        mainPanel(
          h1("Diet metrics"),
          plotOutput(
            "diet_plot" # ,
            # width = 8
          ),
          includeMarkdown("diet.md")
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
          h1("Mental health metrics"),
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
          includeMarkdown("mentalhealth.md")
        )
      )
    ),
    tabPanel(
      "Life correlations",
      fluid = TRUE,
      icon = icon("yin-yang"),
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
          h1("Correlations"),
          div(
            style = "position:relative",
            plotOutput(
              "corr_plot"
            )
          )
        )
      )
    )
  )
)
server <- function(input, output) {
  output$onerepmax_plot <- renderPlot({
    data <- exercise_data %>%
      group_by(exercise_name) %>%
      mutate(first_one_rep_max = first(one_rep_max)) %>%
      ungroup() %>%
      filter(one_rep_max >= (2/3) * first_one_rep_max) %>%
      select(date, exercise_name, one_rep_max, cummax_one_rep_max)
    exercise_names <- input$exercise_name
    radiobuttons_expscale <- input$radiobuttons_expscale
    
    if (length(exercise_names) == 0) {
      exercise_names <- c("Bench Press (Barbell)", "Chin Up")
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
      
      ylim(0, NA) +
      ylab("Estimated one-rep-max (lbs)") +
      scale_color_manual(
        values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666"),
        aesthetics = c("colour", "fill")
      ) +
      scale_x_date(
        breaks = as.Date(c(
          "2020-10-01",
          "2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01"
        )),
        labels = c("Oct '20", "Jan '21", "Apr '21", "Jul '21", "Oct '21"),
        limits = c(min(filtered_data$date), max(filtered_data$date)) # ,
        # title("Date")
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
        legend.text = element_text(size = 14),
        panel.grid.minor.x = element_blank() # ,
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
    include_cardio_calories <- input$radiobuttons_calories
    
    if (include_cardio_calories == "Yes") {
      data <- data %>%
        mutate(volume = volume + (total_energy_burned * 3))
    }
    
    plot <- data %>%
      mutate(
        exp_moving_avg = map_dbl(
          date,
          function(d) rolling_weighted_average(
            volume,
            date,
            d,
            epsilon = rollavg_length_volume
          )
        )
      ) %>%
      ggplot(aes(x = date, y = volume)) +
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
      scale_x_date(
        breaks = as.Date(c(
          "2020-10-01",
          "2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01"
        )),
        labels = c("Oct '20", "Jan '21", "Apr '21", "Jul '21", "Oct '21"),
        limits = c(min(data$date), max(data$date)),
        expand = c(0, 0)
        # title("Date")
      ) +
      scale_y_continuous(
        # trans=scales::pseudo_log_trans(base = 2),
        expand = c(0, 0)
      ) +
      
      # ylim(min(data$value) - 1, max(data$value) + 1) +
      labs(y = "Volume (in estimated one-rep-max lb units)") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 12),
        # axis.title.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 24),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank() # ,
        # panel.grid.minor.y = element_blank()
      )
    
    return(plot)
  })
  
  output$diet_plot <- renderPlot({
    data <- diet_data
    metric_names <- input$diet_metric
    rollavg_length_diet <- input$rollavg_length_diet
    
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
            epsilon = rollavg_length_diet
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
    
    if ("EnergyDeficitPct" %in% metric_names) {
      plot <- data %>%
        ggplot(aes(x = date, y = value, fill = positive_value)) +
        geom_col(alpha = 1 / 3) +
        labs(y = "Proportional calorie deficit/surplus") +
        scale_y_continuous(
          #labels = scales::percent_format(accuracy = 1),
          labels = scales::percent,
          breaks = seq(-1, 1, 0.2),
          expand = c(0, 0)
        )
    } else if ("EnergyDeficit" %in% metric_names) {
      plot <- data %>%
        ggplot(aes(x = date, y = value, fill = positive_value)) +
        geom_col(alpha = 1 / 3) +
        labs(y = "Calorie deficit/surplus") +
        scale_y_continuous(
          breaks = seq(-1500, 1500, 500),
          expand = c(0, 0)
        )
    } else if ("DietaryProtein" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        labs(y = "Protein consumed") +
        theme(legend.position = "none") +
        scale_y_continuous(
          breaks = seq(0, 300, 50),
          expand = c(0, 0)
        )
    } else if ("DietarySugar" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        labs(y = "Sugar consumed") +
        theme(legend.position = "none") +
        scale_y_continuous(
          breaks = seq(0, 300, 50),
          expand = c(0, 0)
        )
    } else if ("EnergyBurned" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        labs(y = "Calories burned") +
        theme(legend.position = "none") +
        scale_y_continuous(
          breaks = seq(0, 4500, 1000),
          expand = c(0, 0)
        )
    } else if ("EnergyConsumed" %in% metric_names) {
      plot <- plot +
        geom_col(alpha = 1 / 3, fill = "#1b9e77") +
        labs(y = "Calories consumed") +
        theme(legend.position = "none") +
        scale_y_continuous(
          breaks = seq(0, 4500, 1000),
          expand = c(0, 0)
        )
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
        breaks = as.Date(c(
          paste0("2021-", c(paste0("0", 1:9), paste0(10:12)), "-01")
        )),
        labels = c(
          paste0(c(
            "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
            " '21")
        ),
        limits = c(min(data_without_NA$date), max(data_without_NA$date)),
        expand = c(0, 0)
        # title("Date")
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
        legend.text = element_text(size = 14),
        panel.grid.minor.x = element_blank()
      )
    
    return(plot)
  })
  
  output$mentalhealth_plot <- renderPlot({
    data <- mentalhealth_data
    # metric_names <- input$diet_metric
    rollavg_length_mentalhealth <- input$rollavg_length_mentalhealth
    
    # data <- data %>%
    #  filter(
    #    metric %in% metric_names &
    #      !is.na(value)
    #  )
    
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
        breaks = as.Date(c(
          paste0("2021-", c(paste0("0", 1:9), paste0(10:12)), "-01")
        )),
        labels = c(
          paste0(c(
            "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
            " '21")
        ),
        limits = c(min(data$date), max(data$date)),
        title("Date"),
        expand = c(0, 0)
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
        legend.text = element_text(size = 14),
        panel.grid.minor.x = element_blank()
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
    
    # gather data together
    data <- diet_data %>%
      pivot_wider(
        names_from = "metric",
        values_from = "value"
      ) %>%
      full_join(mentalhealth_data, by = "date") %>%
      full_join(volume_data, by = "date")
    
    # plot correlation plot
    
    plot <- data %>%
      select(
        irritability,
        anxiety,
        depressed,
        elevated,
        sleep,
        mental_health,
        volume,
        EnergyBurned,
        EnergyConsumed,
        EnergyDeficit,
        DietaryProtein,
        DietarySugar
      ) %>%
      mutate(
        !!!generate_lags(sleep, lifecorrelations_numberlags),
        !!!generate_lags(mental_health, lifecorrelations_numberlags),
        !!!generate_lags(volume, lifecorrelations_numberlags),
        !!!generate_lags(EnergyBurned, lifecorrelations_numberlags),
        !!!generate_lags(EnergyConsumed, lifecorrelations_numberlags)
      ) %>%
      relocate(
        contains("sleep"),
        contains("mental_health"),
        contains("irritability"),
        contains("anxiety"),
        contains("depressed"),
        contains("elevated"),
        contains("volume"),
        contains("EnergyBurned"),
        contains("EnergyConsumed"),
        contains("EnergyDeficit"),
        contains("DietaryProtein"),
        contains("DietarySugar")
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
