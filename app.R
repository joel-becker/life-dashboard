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
library("scales") # log scale breaks
library("plyr") # for round_any()
#library("shinythemes")
library("rsconnect")
library("shinycssloaders") # shiny related
library("bslib") # minty theme
library("markdown")
library("tidyverse")
library("janitor")
library("zoo")

# load helper files
#source("charts_helper.R")

# don't load functions in R folder
# TODO: maybe change this later to write chart
options(shiny.autoload.r=FALSE)

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

#button_color_css <- "
##DivCompClear, #FinderClear, #EnterTimes{
#/* Change the background color of the update button
#to blue. */
#background: DodgerBlue;
#/* Change the text size to 15 pixels. */
#font-size: 15px;
#}"

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
      #tags$style(button_color_css),
      
      ### exercise progression
      tabPanel(
        "Exercise progression",
        fluid = TRUE,
        #icon = icon("dumbbell"),
        #tags$style(button_color_css),
        sidebarLayout(
          sidebarPanel(
            titlePanel("Desired exercises"),
            selectInput(
              "exercise_name",
              label = "Choose exercises",
              exercise_data %>%
                add_count(exercise_name) %>%
                filter(n > 40) %>%
                pull(exercise_name) %>%
                unique(),
              multiple = TRUE
            )
          ),
          mainPanel(
            h1("Estimated one-rep-max by selected exercises"),
            h2("Interactive plot"),
            plotOutput(
              "onerepmax_plot"#,
              #width = 8
            ),
            h2("Explanation"),
            includeMarkdown("onerepmax.md")
          )
        )
      ),
      
      tabPanel(
        "Volume",
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            titlePanel("Rolling average volume"),
            numericInput(
              "rollmean_length",
              label = "Rolling average window length",
              value = 7,
              min = 1,
              max = 31,
              step = 1
            )
          ),
          mainPanel(
            h1("Rolling average volume"),
            h2("Interactive plot"),
            plotOutput(
              "volume_plot"#,
              #width = 8
            ),
            h2("Explanation"),
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
          titlePanel("Desired diet metric"),
          selectInput(
            "diet_metric",
            label = "Choose metric",
            c(
              diet_data %>%
                pull(metric) %>%
                unique()
            ),
            selected = "EnergyDeficit"
          ),
          numericInput(
            "rollmean_length_diet",
            label = "Rolling average window length",
            value = 7,
            min = 1,
            max = 31,
            step = 1
          )
        ),
        mainPanel(
          h1("Diet metrics"),
          h2("Interactive plot"),
          plotOutput(
            "diet_plot"#,
            #width = 8
          ),
          h2("Explanation"),
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
          titlePanel("Rolling average mental health"),
          numericInput(
            "rollmean_length_mentalhealth",
            label = "Rolling average window length",
            value = 7,
            min = 1,
            max = 31,
            step = 1
          )
        ),
        mainPanel(
          h1("Mental health metrics"),
          h2("Interactive plot"),
          plotOutput(
            "mentalhealth_plot"#,
            #width = 8
          )#,
          #h2("Explanation"),
          #includeMarkdown("diet.md")
        )
      )
    )
  )
)
server <- function(input, output) {
  output$onerepmax_plot <- renderPlot({
    data <- exercise_data
    exercise_names <- input$exercise_name
    
    if (length(exercise_names) == 0) {
      exercise_names <- arrange(dplyr::count(data, exercise_name), -n)$exercise_name[c(1, 2)]
    }
    
    filtered_data <- data %>%
      filter(exercise_name %in% exercise_names)
    
    plot <- ggplot(
      filtered_data,
      aes(x = date, y = one_rep_max, group = exercise_name, colour = exercise_name)
    ) +
      geom_line(aes(y = cummax_one_rep_max), size=1.5) +
      geom_point(size=3, alpha=1/3) +
      #geom_line(stat="smooth", method = "lm", formula = y ~ poly(x, 2), se=FALSE, size=1.5, alpha=1/3) +
      
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
        limits = c(min(data$date), max(data$date))#,
        #title("Date")
      ) +
      coord_trans(y = scales::exp_trans(1.008)) +
      scale_y_continuous(
        breaks = seq(0, 400, round_any(max(filtered_data$one_rep_max) / 16, 8, f = ceiling))
      ) +
      #coord_trans(y = "exp") +
      
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12),  
        #axis.title.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=24),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.key.size = unit(2, 'cm'), #change legend key size
        legend.key.height = unit(2, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.text = element_text(size=14),
        panel.grid.minor.x = element_blank()#,
        #panel.grid.minor.y = element_blank()
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
    
    return(plot)
  })
  
  output$volume_plot <- renderPlot({
    data <- volume_data
    rollmean_length <- input$rollmean_length
    
    plot <- data %>%
      ggplot(aes(x = date, y = volume)) +
      geom_col(alpha = 1/3, fill = "#1b9e77") +
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
      scale_x_date(
        breaks = as.Date(c(
          "2020-10-01",
          "2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01"
        )),
        labels = c("Oct '20", "Jan '21", "Apr '21", "Jul '21", "Oct '21"),
        limits = c(min(data$date), max(data$date)),
        expand = c(0, 0)
        #title("Date")
      ) +
      scale_y_continuous(
        #trans=scales::pseudo_log_trans(base = 2),
        expand = c(0, 0)
      ) +
      
      #ylim(min(data$value) - 1, max(data$value) + 1) +
      labs(y = "Volume (in estimated one-rep-max lb units)") +
      
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 12),  
        #axis.title.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=24),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank()#,
        #panel.grid.minor.y = element_blank()
      )
    
    return(plot)
  })
  
  output$diet_plot <- renderPlot({
    data <- diet_data
    metric_names <- input$diet_metric
    rollmean_length <- input$rollmean_length_diet
    
    data <- data %>%
      filter(
        metric %in% metric_names &
          !is.na(value)
      )
    
    plot <- data %>%
      ggplot(aes(x = date, y = value))
    
    if ("EnergyDeficitPct" %in% metric_names){
      plot <- data %>%
        ggplot(aes(x = date, y = value, fill = positive_value)) +
        geom_col(alpha = 1/3) +
        labs(y = "Proportional calorie deficit/surplus") +
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 1)
        )
    } else if ("EnergyDeficit" %in% metric_names){
      plot <- data %>%
        ggplot(aes(x = date, y = value, fill = positive_value)) +
        geom_col(alpha = 1/3) +
        labs(y = "Calorie deficit/surplus")
    } else if ("DietaryProtein" %in% metric_names){
      plot <- plot +
        geom_col(alpha = 1/3, fill = "#1b9e77") +
        labs(y = "Protein consumed") +
        theme(legend.position = "none")
    } else if ("DietarySugar" %in% metric_names){
      plot <- plot +
        geom_col(alpha = 1/3, fill = "#1b9e77") +
        labs(y = "Sugar consumed") +
        theme(legend.position = "none")
    } else if ("EnergyBurned" %in% metric_names){
      plot <- plot +
        geom_col(alpha = 1/3, fill = "#1b9e77") +
        labs(y = "Calories burned") +
        theme(legend.position = "none")
    } else if ("EnergyConsumed" %in% metric_names){
      plot <- plot +
        geom_col(alpha = 1/3, fill = "#1b9e77") +
        labs(y = "Calories consumed") +
        theme(legend.position = "none")
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
      scale_x_date(
        breaks = as.Date(c(
          "2020-10-01",
          "2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01"
        )),
        labels = c("Oct '20", "Jan '21", "Apr '21", "Jul '21", "Oct '21"),
        limits = c(min(data$date), max(data$date)),
        expand = c(0, 0)
        #title("Date")
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 14),  
        #axis.title.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=24),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.key.size = unit(1.5, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.text = element_text(size=14),
        panel.grid.minor.x = element_blank()
        )
    
    return(plot)
  })
  
  output$mentalhealth_plot <- renderPlot({
    data <- mentalhealth_data
    #metric_names <- input$diet_metric
    rollmean_length <- input$rollmean_length_mentalhealth
    
    #data <- data %>%
    #  filter(
    #    metric %in% metric_names &
    #      !is.na(value)
    #  )
    
    plot <- data %>%
      mutate(positive_value = fct_relevel(positive_value, "Net positive", "Net negative")) %>%
      ggplot(aes(x = date, y = mental_health, fill = positive_value)) +
      geom_col(alpha = 1/3) +
      geom_line(
        aes(
          y = rollmean(mental_health, rollmean_length, na.pad = TRUE),
          fill = NULL
        ),
        size = 1,
        colour = "#7570b3"
      ) +
      labs(y = "Mental health score") +
      
      scale_fill_manual(
        values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
      ) +
      scale_x_date(
        breaks = as.Date(c(
          "2020-10-01",
          "2021-01-01", "2021-05-01", "2021-07-01", "2021-09-01", "2021-11-01"
        )),
        labels = c("Oct '20", "Jan '21", "May '21", "Jul '21", "Sep '21", "Nov '21"),
        limits = c(min(data$date), max(data$date)),
        title("Date"),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        lim = c(-100, 100),
        breaks = seq(-100, 100, 25),
        #breaks = c(-100, -80, -40, -20, -10, -5, 0, 5, 10, 20, 40, 80, 100),
        #trans = pseudolog10_trans,
        expand = c(0, 0)
      ) +
      
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 14),  
        #axis.title.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=24),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.key.size = unit(1.5, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.text = element_text(size=14),
        panel.grid.minor.x = element_blank()
      )
    
    return(plot)
  })
  
}

shinyApp(ui, server)
