# Load Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(zoo)
library(ggplot2)
library(lubridate)
library(plotly)
library(shinycssloaders)
library(shinyBS)
library(bslib)
library(scales)
library(tools)
library(shinyWidgets)
library(markdown)

# Tabs to load
flag_tab_1rm <- TRUE
flag_tab_volume <- TRUE
flag_tab_mental_health <- TRUE
flag_goals <- TRUE

# Source plot functions, UI
source("app_functions/plot_1RM.R")
source("app_functions/plot_heat_map.R")
source("app_functions/plot_volume_over_time.R")
source("app_functions/plot_mental_health.R")
source("app_functions/plot_goals_weightlifting.R")
source("app_functions/plot_goals_non_weightlifting.R")
source("app_functions/utilities_UI.R")

# Reload upon saving
options(shiny.autoreload = TRUE)

# Load data
weightlifting_data <- read_csv("data/weightlifting.csv") %>% 
  mutate(date = as.Date(date))
classification_data <- read_csv("data/exercise_classifications.csv")
mentalhealth_data <- read_csv("data/mental_health.csv")
volume_data <- read_csv("data/volume.csv")
non_weightlifting_data <- read_csv("data/non_weightlifting_exercise.csv")

# Function to merge classification data with weightlifting data
merge_classification <- function(weightlifting_data, classification_path) {
  classification_data <- read_csv(classification_path)
  weightlifting_data <- left_join(weightlifting_data, classification_data, by = "exercise_name")
  return(weightlifting_data)
}

# Reactive expression for merged data
merged_data <- merge_classification(weightlifting_data, "data/exercise_classifications.csv")

# Define a mapping of variable names to display names
category_labels <- c(
  "Exercise Name" = "exercise_name",
  "Anterior/Posterior" = "Anterior_Posterior",
  "Push/Pull/Legs" = "Push_Pull_Legs"
)


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Joel Becker's Life Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      if(flag_tab_1rm) menuItem("1RM Plot", tabName = "onerepmax", icon = icon("chart-line")),
      if(flag_tab_volume) menuItem("Volume Heatmap", tabName = "volume", icon = icon("th")),
      if(flag_tab_mental_health) menuItem("Mental Health", tabName = "mental_health", icon = icon("smile")),
      if(flag_goals) menuItem("Goals", tabName = "goals", icon = icon("bullseye"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab for 1RM Plot
      tabItem(tabName = "onerepmax",
        fluidPage(
          tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")), # Custom CSS
          #tags$img(src = 'logo.png', height = 60, width = 60), # Logo placeholder
          fluidRow(
            box(
              title = "Estimated One-Rep-Max by Selected Exercises", width = 8, solidHeader = TRUE, status = "primary",
              plotlyOutput("onerepmax_plot") %>% withSpinner()
            ),
            box(
                title = "Options", 
                width = 4, 
                solidHeader = FALSE, 
                status = "info",
                closable = TRUE,
                collapsible = TRUE,
                pickerInput(
                  inputId = "exercise_name",
                  label = "Choose exercises",
                  choices = weightlifting_data %>%
                    add_count(exercise_name) %>%
                    dplyr::group_by(exercise_name) %>%
                    dplyr::mutate(
                      last_exercise = max(date),
                      days_since_last = as.numeric(Sys.Date() - last_exercise),
                      sorting_metric = n / (60 + days_since_last)
                      ) %>%
                    ungroup() %>%
                    filter(n >= 50 & days_since_last <= 1000) %>%
                    arrange(desc(sorting_metric)) %>%
                    pull(exercise_name) %>%
                    unique(),
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    liveSearch = TRUE
                  )
                ),
                bsTooltip("exercise_name", "Select one or multiple exercises.", "right"),
                bsCollapse(
                  id = "other_options",
                  open = FALSE,
                  bsCollapsePanel("Other Options",
                    radioButtons(
                      "line_type",
                      label = "Line Type",
                      choices = c("Rolling 90-day Maximum" = "rolling", "Cumulative Maximum" = "cummax"),
                      selected = "rolling"
                    ),
                    checkboxInput("highlight_best_lifts", "Highlight Best Lifts from Past 30 Days", FALSE),
                    checkboxInput("show_single_rep", "Differentiate 1-rep sets", FALSE)
                  )
                )
            ),
            box(
                title = "Description", 
                width = 4, 
                solidHeader = FALSE, 
                status = "info",
                closable = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                includeMarkdown("markdown_descriptions/1RM.md")
            )
          ),
          tags$footer(
            HTML(
              paste0(
                "Visit ",
                a("my website", href = "https://joel-becker.com", target = "_blank"),
                " or the ",
                a("code for this project", href = "https://github.com/joel-becker/life-dashboard", target = "_blank"),
                "."
              )
            )
          )
        )
      ),
      # Tab for Volume Heatmap
      tabItem(tabName = "volume",
        fluidRow(
          box(
            title = "Volume Heatmap", width = 8, solidHeader = TRUE, status = "primary",
            plotOutput("volume_heatmap") %>% withSpinner()
          ),
          box(
            title = "Options", width = 4,
            solidHeader = FALSE, 
            status = "info", 
            closable = TRUE, 
            collapsible = TRUE,
            div(
              style = "display: block; position: relative;",
              pickerInput(
                "heatmap_metric",
                label = create_label_with_tooltip("Metric", "heatmap_metric-info"),
                choices = c("Frequency", "Volume"),
                selected = "Frequency", # Set default value
                multiple = FALSE,
                options = pickerOptions(
                  actionsBox = FALSE,
                  liveSearch = FALSE
                )
              ),
              bsTooltip(id = "heatmap_metric-info", title = "The metric being summed.", placement = "right", trigger = "hover")
            ),
            pickerInput(
              "category", 
              label = "Exercise Categorization",
              choices = category_labels, 
              selected = "Push_Pull_Legs",
              multiple = FALSE,
              options = pickerOptions(
                actionsBox = FALSE,
                liveSearch = FALSE
              )
            ),
            pickerInput(
              "time_bin", 
              label = create_label_with_tooltip("Time Bin Size", "heatmap_time-bin-info"), 
              c("2 months", "4 months"),
              selected = "2 months",
              multiple = FALSE,
              options = pickerOptions(
                actionsBox = FALSE,
                liveSearch = FALSE
              )
            ),
            bsTooltip(id = "heatmap_time-bin-info", title = "The length of time that bins correspond to.", placement = "right", trigger = "hover")
          ),
          box(
              title = "Description", 
              width = 4, 
              solidHeader = FALSE, 
              status = "info",
              closable = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("markdown_descriptions/volume_heatmap.md")
          )
        ),
        fluidRow(
          box(
            title = "Volume Over Time", width = 8, solidHeader = TRUE, status = "primary",
            plotOutput("volume_over_time") %>% withSpinner()
          ),
          box(
            title = "Options", width = 4,
            solidHeader = FALSE, 
            status = "info", 
            closable = TRUE, 
            collapsible = TRUE,
            checkboxInput("volume_over_time_exclude_zeros", "Exclude zeros", FALSE),
            bsTooltip(
              id = "volume_over_time_exclude_zeros-info", 
              title = "Affects whether average volume is estimated on a per day (including zeros) or per workout (excluding zeros) basis.", 
              placement = "right", 
              trigger = "hover"
              ),
            noUiSliderInput(
              inputId = "volume_over_time_rollavg_length",
              label = "Exponential Moving Average Parameter",
              min = 0, 
              max = 0.3,
              value = c(0.1),
              tooltips = TRUE,
              step = 0.001
            ),
          ),
          box(
              title = "Description", 
              width = 4, 
              solidHeader = FALSE, 
              status = "info",
              closable = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("markdown_descriptions/volume_over_time.md")
          )
        ),
        tags$footer(
          HTML(
            paste0(
              "Visit ",
              a("my website", href = "https://joel-becker.com", target = "_blank"),
              " or the ",
              a("code for this project", href = "https://github.com/joel-becker/life-dashboard", target = "_blank"),
              "."
            )
          )
        )
      ),
      # Tab for Mental Health
      tabItem(tabName = "mental_health",
        fluidRow(
          box(
            title = "Mental Health Plot", width =8, solidHeader = TRUE, status = "primary",
            plotlyOutput("mental_health_plot") %>% withSpinner()
          ),
          box(
            title = "Options", 
            width = 4, 
            solidHeader = FALSE, 
            status = "info", 
            closable = TRUE, 
            collapsible = TRUE,
            pickerInput(
              inputId = "mental_health_metric",
              label = "Choose metrics",
              choices = c(
                "Mental Health" = "mental_health", 
                "Subjective Well-Being" = "subjective_well_being", 
                "Life Satisfaction" = "life_satisfaction", 
                "Work Satisfaction" = "work_satisfaction"
                ),
              selected = "mental_health",
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE
              )
            ),
            noUiSliderInput(
              inputId = "rollavg_length_mental_health",
              label = "Exponential Moving Average Parameter",
              min = 0, 
              max = 0.3,
              value = c(0.1),
              tooltips = TRUE,
              step = 0.001
            ),
            #checkboxInput(
            # "include_partial_data", 
            # "Include scores based on partial information", 
            # FALSE
            # ),
          ),
          box(
            title = "Description", 
            width = 4, 
            solidHeader = FALSE, 
            status = "info", 
            closable = TRUE, 
            collapsible = TRUE,
            collapsed = TRUE,
            includeMarkdown("markdown_descriptions/mental_health.md")
          )
        ),
        tags$footer(
          HTML(
            paste0(
              "Visit ",
              a("my website", href = "https://joel-becker.com", target = "_blank"),
              " or the ",
              a("code for this project", href = "https://github.com/joel-becker/life-dashboard", target = "_blank"),
              "."
            )
          )
        )
      ),
      # Tab for Goals
      tabItem(tabName = "goals",
        fluidRow(
          box(
              title = "Description", 
              width = 12, 
              solidHeader = FALSE, 
              status = "info",
              closable = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("markdown_descriptions/goals.md")
          ),
          box(
            title = "Weightlifting Goals", width = 6, solidHeader = TRUE, status = "primary",
            plotOutput("goals_weightlifting") %>% withSpinner()
          ),
          box(
            title = "Non-Weightlifting Exercise Goals", width = 6, solidHeader = TRUE, status = "primary",
            plotOutput("goals_non_weightlifting") %>% withSpinner()
          ),
        ),
        tags$footer(
          HTML(
            paste0(
              "Visit ",
              a("my website", href = "https://joel-becker.com", target = "_blank"),
              " or the ",
              a("code for this project", href = "https://github.com/joel-becker/life-dashboard", target = "_blank"),
              "."
            )
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # Define the reactive expression for the merged data
  merged_data <- reactive({
    merge_classification(weightlifting_data, "data/exercise_classifications.csv")
  })

  # Reactive expression for prepared data for heatmap
  data_volume_heatmap <- reactive({
    prepare_heatmap_data(merged_data(), input$heatmap_metric, input$category, input$time_bin)
  })
  
  # Original 1RM plot output
  output$onerepmax_plot <- renderPlotly({
    plot_one_rep_max(
      exercise_names = input$exercise_name,
      line_type = input$line_type,
      show_single_rep = input$show_single_rep,
      highlight_best_lifts = input$highlight_best_lifts,
      weightlifting_data = weightlifting_data
    )
  })

  # Output for heatmap plot
  output$volume_heatmap <- renderPlot({
    req(input$category)  # Ensure that the input$category is provided
    plot_heatmap(
      data_volume_heatmap(), input$heatmap_metric, input$category, input$time_bin
    )  # Call the reactive variable as a function
  })

  # Output for volume over time plot
  output$volume_over_time <- renderPlot({
    plot_ggplot <- plot_volume_over_time(
      volume_data, input$volume_over_time_rollavg_length, input$volume_over_time_exclude_zeros
    )

    plot_ggplot
  })

  # Output for well-being plot
  output$mental_health_plot <- renderPlotly({
    mental_health_data <- reactive({ mentalhealth_data })
    plot_ggplot <- plot_mental_health(
      mental_health_data(), input$rollavg_length_mental_health, 
      input$mental_health_metric#, input$include_partial_data
      )
    
    # Convert ggplot to plotly
    plot_plotly <- ggplotly(plot_ggplot, tooltip = "text")

    # Return the customized plotly object
    plot_plotly
  })

  # Output for goals plot
  output$goals_weightlifting <- renderPlot({
    plot_goals_weightlifting <- plot_goals_weightlifting(
      weightlifting_data
    )

    plot_goals_weightlifting
  })

  output$goals_non_weightlifting <- renderPlot({
    plot_goals_non_weightlifting <- plot_goals_non_weightlifting(
      non_weightlifting_data
    )

    plot_goals_non_weightlifting
  })
}

# Run app
shinyApp(ui = ui, server = server)
