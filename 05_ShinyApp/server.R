shinyServer(function(input, output) {
  
  # Reactive components
  prediction <- reactive({
    giveNumberOfPossibilities <<- input$giveNoPos
    
    filter <- character()
    if(input$filterNumber)
      filter <- c(filter, "#")
    if(input$filterEndline)
      filter <- c(filter, "\n")
    if(input$filterComma)
      filter <- c(filter, ",")
    doNotPredict <<- filter
    
    predict(freqModel, input$inputModel)
  })
  
  
  # Output text
  output$testText <- renderUI({
    HTML(paste0("Prediction of ", input$giveNoPos, 
                " possible next words to input \"", input$inputModel, 
                "\" are: <br/><br/>",
                paste(prediction()$value, collapse = "<br/>")))
  })
  
})
