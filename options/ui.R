
# Define UI
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Option Price Exploration",windowTitle="Ben Porter - Options"),
  
  sidebarPanel(
  
  textInput("ticker", "Enter Ticker Symbor", value = "JPM"),
  sliderInput("expiryYear", "Expiration Year:", 
              min = 2012, max = 2013, value = 2012, step= 1), 
  sliderInput("expiryMonth", "Expiration Month:", 
              min = 1, max = 12, value = 11, step= 1)
  
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data Frame",tableOutput("callsTable")),
      tabPanel("Expiration Date",textOutput("expiryText"))
            
    )
  )

))