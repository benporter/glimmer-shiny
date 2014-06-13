
# Define UI
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Charlotte R Users Group Survey Results",windowTitle="Charlotte R Users Group"),
  
  sidebarPanel(
    checkboxInput("numberlabelcolor", "Number Label White", TRUE),
    HTML("<hr size=10 width=100% align=\"left\" noshade>"),
    radioButtons("barcolor", "Select Bar Color",
                 list("Dark Blue" = "darkblue",
                      "Blue" = "blue",
                      "Light Blue" = "lightblue",
                      "Red" = "red"),
                 selected="Blue"),
    HTML("<hr size=10 width=100% align=\"left\" noshade>"),
    sliderInput("xsize", "Size of Category Labels", 
                min = 8, max = 25, value = 15, step = 1)
  ),

        
 mainPanel(
   tabsetPanel(
     tabPanel("Q1",
              plotOutput("firstplot")),
     tabPanel("Q2",
              plotOutput("secondplot")),
     tabPanel("Q3",
              plotOutput("thirdplot")),
     tabPanel("Q4",
              plotOutput("forthplot")),  
     tabPanel("Q5",
              plotOutput("fifthplot")), 
     tabPanel("Q6",
              plotOutput("sixthplot")),  
     tabPanel("Q7",
              plotOutput("seventhplot")),   
     tabPanel("Q8",
              plotOutput("eigthplot")), 
     tabPanel("Q9",
              plotOutput("ninthplot")), 
     tabPanel("Q10",
              plotOutput("tenthplot")), 
     tabPanel("Q11",
              plotOutput("eleventhplot")),  
     tabPanel("Q12",
              plotOutput("twelfthplot")),  
     tabPanel("Data",
              dataTableOutput("df_melt"))
    )
 )
  
))
    