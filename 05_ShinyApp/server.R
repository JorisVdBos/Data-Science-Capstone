shinyServer(function(input, output) {
  
  # Reactive components
  predictionFreq <- reactive({
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
  
  predictionKN <- reactive({
    giveNumberOfPossibilities <<- input$giveNoPos

    filter <- character()
    if(input$filterNumber)
      filter <- c(filter, "#")
    if(input$filterEndline)
      filter <- c(filter, "\n")
    if(input$filterComma)
      filter <- c(filter, ",")
    doNotPredict <<- filter

    predict(KNModel, input$inputModel)
  })
  
  
  # Output text
  output$freqModelTest <- renderUI({
    HTML(paste0("Prediction of the 3-gram model:<br/><br/>",
                paste(predictionFreq()$value, collapse = "<br/>")))
  })
  
  output$KNModelTest <- renderUI({
    HTML(paste0("Prediction of the Kneser-Ney model:<br/><br/>",
                paste(predictionKN()$value, collapse = "<br/>")))
  })
  
})
