shinyServer(function(input, output) {
  
  # Reactive components
  prediction <- reactive({
    giveNumberOfPossibilities <<- input$giveNoPos
    predict(freqModel, input$inputModel)
  })
  
  
  # Output text
  output$testText <- renderUI({
    HTML(paste0("Prediction of ", input$giveNoPos, 
                " possible next words to input \"", input$inputModel, 
                "\" are: <br/><br/>",
                paste(prediction()$value, collapse = "<br/>"))
    )
  })
  
})
