# Set the working directory to the red_blue folder here
setwd("/Users/nasiha/red_blue/red_blue")
main_dir<- getwd()

# 1. Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

librarian::shelf(tidyverse, tsibble, lubridate, glue, TimTeaFan/dplyover, zoo, TTR, fs, gt, openxlsx, 
                 snakecase, rlang, fredr, BrookingsInstitution/ggbrookings, ipumsr, here, haven, broom)

results<-read_rds(glue("{main_dir}/charts/coefficients_year_interaction.rds"))

library(plotly)

# 2. Create a UI for user input
ui <- fluidPage(
  titlePanel("Regression Coefficients"),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcomeInput", "Select an outcome:", 
                  choices = unique(results$outcome_option)),
      selectInput("controls_lab", "Select Controls", choices = c("_2020+_2021+_2022+yes_2020+yes_2021+yes_2022")),
      radioButtons("type", "Select type", choices = unique(results$type))
    ),
    mainPanel(
      plotOutput("coefPlot"),
      verbatimTextOutput("modelSummary")
    )
  )
)

# 3. Create a server that processes user input and displays the output
server <- function(input, output) {
  output$coefPlot <- renderPlot({
    data <- results %>% filter(outcome_option == input$outcomeInput & type == input$type & controls_lab == input$controls_lab)
    
    # Filter out NA terms
    data <- data %>% filter(!is.na(term))
    
    # Manual renaming for coefficient labels
    coef_names <- c("(Intercept)" = "Intercept",
                    "share_blue2020_2020" = "Biden share*2020 dummy",
                    "share_blue2020_2021" = "Biden share*2021 dummy",
                    "share_blue2020_2022" = "Biden share*2022 dummy",
                    "yes_2020" = "2020 dummy",
                    "yes_2021" = "2021 dummy",
                    "yes_2022" = "2022 dummy")
    
    data$term <- factor(data$term, levels = names(coef_names))
    names(data$term) <- coef_names
    
    # Extract the plot title from the first row of the data
    plot_title <- paste("Regression Coefficients for", data$outcome_lab[1])
    
    ggplot(data, aes(x = term, y = estimate, color = p.value < 0.1)) +
      geom_point(size = 4) +
      geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
      labs(title = plot_title, x = "Term", y = "Estimate") +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"), 
                         labels = c("Significant", "Not significant")) +
      scale_x_discrete(labels = coef_names) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 14, hjust = 0.5),
            legend.position = "bottom")
  })
  
  output$modelSummary <- renderPrint({
    data <- results %>% filter(outcome_option == input$outcomeInput & type == input$type & controls_lab == input$controls_lab)
    rsq <- unique(data$rsquared)
    cat(paste0("R-Squared: ", rsq))
  })
}

# 4. Create the shiny app by calling shinyApp(ui = ui, server = server)
shinyApp(ui = ui, server = server)
