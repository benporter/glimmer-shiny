library(shiny)
# Define UI
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Housing Distance",windowTitle="Ben Porter - Shiny App"),
  
  sidebarPanel(
    
    textInput("work_address", "Work Address", value = "Trade and Tryon Charlotte, NC"),
    
    br(),
    
    textInput("home_address", "Home Address", value = "730 Hawthorne Ln Charlotte, NC"),
    
    checkboxInput("avoidhwy", "Avoid Highways", FALSE),
    
    actionButton("goButton", "Go!")
    
  ),
  
  mainPanel(
    tabsetPanel(
    tabPanel("Main",textOutput("disttext"),
                    textOutput("durationtext"),
                    textOutput("origin_addr_text"),
                    textOutput("dest_addr_text")),
    tabPanel("Debug",textOutput("debug"))
   )
  )
  
  
))