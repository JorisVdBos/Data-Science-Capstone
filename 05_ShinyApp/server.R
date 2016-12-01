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
