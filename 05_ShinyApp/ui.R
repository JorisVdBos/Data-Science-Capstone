##############
# Header
##############

dbHeader <- dashboardHeader(title = applicationTitle,
                            titleWidth = titleWidth)
##############
# sidebar
##############

sidebar <- dashboardSidebar(
  width = sidebarWidth,
  sidebarMenu(
    menuItem(text     = "Handson", 
             tabName  = "model1", 
             icon     = icon("play")),
    menuItem(text     = "More info", 
             tabName  = "moreInfo", 
             icon     = icon("question"))
  )
)

#############
# Body
#############

body <- dashboardBody(
  
  # Add shinyJS to allow mini-sidebar
  shinyjs::useShinyjs(),
  
  # Include custom css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE-2.0.6/AdminLTE.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE-2.0.6/_all-skins.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "shinydashboard-0.5.1/shinydashboard.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "model1",
            h1(model1Title),
            fluidPage(
              sliderInput("giveNoPos",
                          "Give top words:",
                          min = 1,
                          max = 20,
                          value = 3),
              br(),
              textInput("inputModel", 
                        label = NULL,
                        placeholder = "Put text here!"),
              br(),
              br(),
              "Model predictions:",
              textOutput("testText")
            )
    ),
    tabItem(tabName = "moreInfo", 
            h1(moreInfoTitle),
            fluidPage(
              fluidRow(HTML(aboutText))
            )
    )
  )
)

###################################
# Merge pieces to complete ui
###################################

ui <- dashboardPage(skin    = "blue",
                    header  = dbHeader,
                    sidebar = sidebar,
                    body    = body)