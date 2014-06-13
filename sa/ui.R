
# Define UI
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Sensitivity Analysis",windowTitle="Ben Porter - Shiny App"),
  
  sidebarPanel(
    
    radioButtons("scenario", "Select Scenario",
                 list("Scenario 1: 24 Months" = "scen1",
                      "Scenario 2: 72 Months" = "scen2",
                      "Scenario 3: 50 Years"  = "scen3"),
                 selected="Scenario 1: 24 Months"),
    
    br(),
    
    sliderInput("tenants", "Number of Tenants:", 
                min = 1, max = 700, value = 482, step= 1),  
    
    sliderInput("dpw", "Trains Per Week:", 
                min = 1, max = 7, value = 3, step= 1),
    
    sliderInput("train.length", "Average Train Length (in miles):", 
                min = 0, max = 1, value = 0.25, step= 0.01),
    
    
    sliderInput("train.speed", "Average Train Speed (MPH):", 
                min = 0, max = 25, value = 10, step = 0.5),
    
    br(),
    
    checkboxInput("charmeckavg", "Add Char Meck Avg Response Line", TRUE)
    
  ),
  
  mainPanel(
    
    plotOutput("probplot",width="60%"),
    br(),
    textOutput("probtext"),
    br(),
    downloadButton("downloadSummary", "Download Current Scenario"),
    br(),br(),
    tableOutput("datasummary")
    #textOutput("whatami") #used for debugging
  )
  
  
))