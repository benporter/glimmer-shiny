# Define UI
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Non-Profit Mailings Optimization",windowTitle="Non-Profit Mailings Optimization - Ben Porter"),
  
  sidebarPanel(
    h4("Define Assumptions"),
    sliderInput("rrate", "Response Rate", 
                min = 0.01, max = 1, value = 0.05, step = 0.01),
    numericInput("mail_uc", "Average Cost per Mailing",
                 min = 0, value = 0.55, step = 0.01),
    numericInput("n_mailings", "Number of Mailings",
                 min = 0,value = 10000, step = 100),
    numericInput("avg_donation", "Average Gift Amount, in dollars",
                 min = 0,value = 10, step = 1)
  
  ),
  
  mainPanel(
    tabsetPanel(tabPanel("Overview",
                         textOutput("overviewText")
                        ),
                tabPanel("Donations",
                         textOutput("donationText")
                         ),
                tabPanel("Net Gain/Loss",
                         textOutput("donationSummary"),
                         textOutput("campaignCostSummary"),
                         textOutput("netSummary"),
                         plotOutput("breakevenplot"),
                         plotOutput("lineplot")
                         ),
                tabPanel("Deeper Analysis",
                         textOutput("predModelText")
                         ),
                tabPanel("Data",
                         dataTableOutput("df_melt")
                         )
                )
    )


))