library(shiny)
library(DT)

shinyServer(function(input, output, session) {
  
  output$results_table <- renderDT({
    data.frame(Message = "Click 'Run Simulation' to generate results.")
  })
  
  output$oc_table <- renderDT({
    data.frame(Message = "Results will appear here.")
  })
  
})