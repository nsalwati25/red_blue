# Set the working directory to the red_blue folder here
setwd("/Users/nasiha/red_blue/red_blue")
main_dir<- getwd()

# 1. Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Load the 'results' dataset
results<-read_rds(glue("{main_dir}/charts/coefficients_year_interaction.rds"))

# 1. Function to exclude common prefix
exclude_common_prefix <- function(labels) {
  prefix <- Reduce(function(x, y) {
    i <- 1
    while(i <= nchar(x) && i <= nchar(y) && substr(x, i, i) == substr(y, i, i)) {
      i <- i + 1
    }
    substr(x, 1, i - 1)
  }, labels)
  
  sapply(labels, function(label) substr(label, nchar(prefix) + 1, nchar(label)))
}

# 2. Create a UI for user input
ui <- fluidPage(
  titlePanel("Regression Coefficients"),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcomeOptionCategory", "Select an outcome option category:", 
                  choices = c( "Employment-to-population ratio", 
                               "Labor force participation rate", 
                               "Unemployment rate", 
                               "Opportunity Insights - Spending", 
                               "PCE", 
                               "PCE - Durables", 
                               "PCE - Non durables",
                               "PCE - Household services", 
                               "PCE - NPISH", 
                               "Share of headline,PCE", 
                               "Share of headline,PCE - Durables", 
                               "Share of headline,PCE - Non durables", 
                               "Share of headline,PCE - Household services", 
                               "Share of headline,PCE - NPISH")),
      selectInput("controls_lab", "Select Controls", choices = c("_2020+_2021+_2022+yes_2020+yes_2021+yes_2022")),
      radioButtons("type", "Select type", choices = unique(results$type))
    ),
    mainPanel(
      plotOutput("coefPlot_2020", width = "100%", height = "600px"),
      plotOutput("coefPlot_2021", width = "100%", height = "600px"),
      plotOutput("coefPlot_2022", width = "100%", height = "600px"),
      verbatimTextOutput("modelSummary")
    )
  )
)

# 3. Create a server that processes user input and displays the output
server <- function(input, output) {
  
  # Filter the data based on user inputs
  data <- reactive({
    results %>% 
      filter(outcome_option_category == input$outcomeOptionCategory & 
               type == input$type & 
               controls_lab == input$controls_lab)
  })
  
  # Plotting function
  plot_func <- function(term, title) {
    data_to_plot <- data() %>% filter(term == !!term)
    
    data_to_plot$outcome_option <- exclude_common_prefix(data_to_plot$outcome_option)
    
    ggplot(data_to_plot, aes(x = outcome_option, y = estimate, color = p.value < 0.1)) +
      geom_point(size = 4) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      labs(title = title, x = "Outcome Option", y = "Estimate") +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"), 
                         labels = c("Significant", "Not significant")) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 14, hjust = 0.5),
            legend.position = "bottom")
  }
  
  # Create separate plots for each year
  # Create separate plots for each year
  output$coefPlot_2020 <- renderPlot({ plot_func("share_blue2020_2020", "Coefficient on Biden share*2020 dummy") })
  output$coefPlot_2021 <- renderPlot({ plot_func("share_blue2020_2021", "Coefficient on Biden share*2021 dummy") })
  output$coefPlot_2022 <- renderPlot({ plot_func("share_blue2020_2022", "Coefficient on Biden share*2022 dummy") })
  
  output$modelSummary <- renderPrint({
    data <- data()
    rsq <- unique(data$rsquared)
    cat(paste0("R-Squared: ", rsq))
  })
}

# 4. Create the shiny app by calling shinyApp(ui = ui, server = server)
shinyApp(ui = ui, server = server)