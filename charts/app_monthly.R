
librarian::shelf(tidyverse, tsibble, lubridate, glue, TimTeaFan/dplyover, zoo, TTR, fs, gt, openxlsx, 
                 snakecase, rlang, fredr, BrookingsInstitution/ggbrookings, ipumsr, here, haven, broom)
library(shiny)

coefficients_grouped<-read_rds("/charts/coefficients_monthly.rds")

# UI
library(ggplot2)
library(plotly)

ui <- fluidPage(
  selectInput("outcome_option", "Select Outcome", choices = unique(coefficients_grouped$outcome_option)),
  selectInput("controls_lab", "Select Controls", choices = unique(coefficients_grouped$controls_lab)),
  radioButtons("type", "Select type", choices = unique(coefficients_grouped$type)),
  
  
  mainPanel(
    plotlyOutput("myplot"),
    DT::dataTableOutput("data_table")
    
  )
)

server <- function(input, output) {
  output$myplot <- renderPlotly({
    # Subset the data for the selected ID
    df <- coefficients_grouped[coefficients_grouped$outcome_option == input$outcome_option, ]
    df <- df[df$controls_lab == input$controls_lab, ]
    df <- df[df$type == input$type, ]
    
    # Create the ggplot for the selected ID
    df$sig <- df$p.value < 0.1
    
    plot <- ggplot(df, aes(x = newtime, y = estimate, text = paste("R-squared: ", round(rsquared, 3), "<br>Coefficient ", unique(df$estimate)))) +
      geom_point(aes(color = sig), size = 3) +
      scale_color_manual(values = c("grey", "red")) +
      geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error, color = sig), width = 0.2, color = "grey80") +
      geom_line(aes(group = 1), color = "black", size = 0.5) +  # add this line to connect the dots
      labs(title = paste0(unique(df$outcome_lab)), x = "Time", y = "Coefficient on Biden Share") +
      scale_x_continuous(breaks = seq(min(df$newtime), max(df$newtime), by = 1)) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(colour = "black", size = 14), # Increase font size of axis labels
            axis.title = element_text(colour = "black", size = 16, face = "bold"), # Increase font size of axis titles
            plot.title = element_text(size = 20, face = "bold")) # Increase font size of plot title
    

    # Add vertical line at newtime == 2020.083
    plot <- plot + geom_vline(xintercept = 2020.083, linetype = "dashed", color = "grey")
    
    # Convert the ggplot object to a plotly object
    p <- ggplotly(plot, tooltip = "text")
    
    
    # Return the plotly object
    return(p)
  })
  
  output$data_table <- DT::renderDataTable({
    df <- coefficients_grouped[coefficients_grouped$outcome_option == input$outcome_option, ]
    df <- df[df$controls_lab == input$controls_lab, ]
    df <- df[df$type == input$type, ]
    df<- df[, c("newtime", "estimate", "outcome_lab", "controls_lab", "std.error", "p.value", "nobs")]
    DT::datatable(print(df), options = list(pageLength = 5, lengthChange = FALSE))
  })
}



shinyApp(ui, server)


