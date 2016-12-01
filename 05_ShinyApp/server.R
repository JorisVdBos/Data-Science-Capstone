#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  prediction <- reactive({
    giveNumberOfPossibilities <<- setNumberOfPossibilities()
    predict(freqModel, input$inputModel)
  })
  
  setNumberOfPossibilities <- reactive({
    input$giveNoPos
  })
  
  output$testText <- renderUI({
    HTML(paste0("Prediction of ", setNumberOfPossibilities(), 
                " possible next words to input \"", input$inputModel, 
                "\" are: <br/><br/>",
                paste(prediction()$value, collapse = "<br/>"))
    )
    
  })
  
})
  