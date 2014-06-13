
#library(shiny)

# Define UI
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Word Cloud Generator",windowTitle="Ben Porter - Wordcloud Generator"),
  
  sidebarPanel(
    h4("Select Data Input Method"),
    radioButtons("inputmethod", "Select Data Source",
                 list("Predefined Speeches" = "normal",
                      "Upload Custom (.txt only)" = "userdefined"),
                 selected="Predefined Speeches"),
    
    conditionalPanel(
      condition = "input.inputmethod == 'normal'",
      selectInput("dataset", "Choose a File:",
                  choices = factor(c(list.files('~/ShinyApps/wordcloud/data/', pattern=".txt",recursive = TRUE))))  
    ),
    
    
    conditionalPanel(
      condition = "input.inputmethod == 'userdefined'",
      fileInput('file1', 'Upload your own .txt file',
                accept=c('text/csv', 'text/comma-separated-values,text/plain'))
      
    ),
    
    HTML("<hr size=10 width=100% align=\"left\" noshade>"),  
    h4("Transformations"),
    checkboxInput("lowercase", "Convert to Lowercase", TRUE),
    checkboxInput("stripwhite", "Strip White Space", TRUE),
    checkboxInput("removepunc", "Remove Punctuation", TRUE),
    checkboxInput("stopwords", "Remove Standard English Stopwords", TRUE),
    #checkboxInput("stem", "Stem Words (e.g. like, likes, likely becomes lik)", FALSE),
    
    br(),
    textInput("customstop", "Enter Custom Words to Remove", value = "separate, words, by, comma"),
    HTML("<hr size=10 width=100% align=\"left\" noshade>"),
    downloadButton("downloadFreq", "Download Word Frequencies")
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Wordcloud",plotOutput("wordcloudplot")),
      tabPanel("Word Frequency",tableOutput("freqtable")),
      tabPanel("Input Text",textOutput("inputtext"))
      #tabPanel("debug",textOutput("caption1")),
      #tabPanel("debug",tableOutput("caption1")),
      #tabPanel("debug2",textOutput("customstopwords"))
      
    ))
))