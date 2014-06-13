
library(shiny)
#library(XLConnect)       # to read in .xlxs files
library(ggplot2)         # to create plots using ggplot 
library(sqldf)           # for SQL


shinyServer(function(input, output) {
  
  ###################################
  # Read in raw survey results data
  ###################################
  
  #setwd("/home/benporter/ShinyApps/crug")
  
  #file.name <- "Charlotte R Users Group Survey (Responses).xlsx"
  #sheet.name <- "Form Responses"
  #survey.data <- readWorksheet(loadWorkbook(file.name),sheet=sheet.name)
  
  file.name <- "Charlotte R Users Group Survey (Responses) - latest.csv"
  #survey.data <- read.csv(file.name)
  
  survey.data <- read.table(file=file.name, header=TRUE, sep="\t")
  
  ################
  # Format Data
  ################
  
  # Remove personal data
  num.col <- ncol(survey.data)
  num.col.keep <- num.col-4
  survey.data <- survey.data[,1:num.col.keep]
  
  # take of first column, the timestamo column
  survey.data <- survey.data[,2:ncol(survey.data)]  
  
  # For radio button style questions
  createSubset <- function(col.choice) {
        
    df <- survey.data
    df$count <- 1
    question <- colnames(df)[col.choice]
    df <- df[,c(question,"count")]
    title.formatted <- paste(gsub("\\."," ",question),"?",sep="")
    
    colnames(df) <- c("response","count")
    
    df.summary <- sqldf("select response, 
                                sum(count) as count
                         from df
                         group by response")
    return(df.summary)
  }
  
  # For multiselect questions
  createSubset2 <- function(col.choice) {
    
    df <- survey.data
    df$count <- 1
    question <- colnames(df)[col.choice]
    df <- df[,c(question,"count")]
    
    #df <- survey.data
    #question <- colnames(df)[col.choice]
    #df <- df[,c(question)]
    
    df.new <- data.frame("response"=NULL)
    for (i in 1:nrow(df)) {
      
      # Step 1: parse values and get list of responses
      
      df[,c(question)] <- as.character(df[,c(question)])
      
      #grepl is a grep logical, returns true or false
      if(grepl(",",df[i,1])) {
        values <- strsplit(df[i,1], ",")
      } else {
        values <- df[i,1]
      }  
      
      # Step 2: Put into single column
      list.length <- length(values[[1]])
      
      for (j in 1:list.length) {
        list.item <- values[[1]][j]
        
        # trim leading and trailing spaces using regex
        list.item <- as.character(gsub("^\\s+|\\s+$", "", list.item))
        
        df.new <- rbind(df.new,data.frame("response"=list.item))
      }  
    }
    
    df.new$count <- 1
    dfnew <- df.new
    
    df.new2 <- sqldf("select response,
                        sum(count) as count
                 from dfnew
                 group by response")  
    
    return(df.new2)  
    
  } # end of createSubset2 function
  
  df.list <- list(createSubset(1))
  for (i in 2:num.col.keep) {
    df.list <- c(df.list,list(createSubset(i)))
  }
  
  
  # multiselect questions
  #msq <- c(3,4,5,7,8,9,10)
  df.list2 <- list(createSubset2(3))
  for (i in 2:num.col.keep) {
  #for (i in msq) {
    df.list2 <- c(df.list2,list(createSubset2(i)))
  } 
  
  getTitle <- function(col.choice) {
    
    df <- survey.data
    question <- colnames(df)[col.choice]
    title.formatted <- paste(gsub("\\."," ",question),"?",sep="")
    return(title.formatted)

  }
    
  createStandardPlot <- function(df,title.chart) {
    
    #g <- ggplot(df,aes(y=count)) + geom_bar(aes(x=response),fill="darkblue",stat="identity")
    g <- ggplot(df,aes(y=count)) + geom_bar(aes(x=response),fill=input$barcolor,stat="identity")
    if(input$numberlabelcolor == TRUE) { 
      g <- g + geom_text(aes(y = count/2,x=response,label=count),colour="white") 
    } else {
      g <- g + geom_text(aes(y = count/2,x=response,label=count),colour="black") 
    }
    g <- g + coord_flip()
    g <- g + ggtitle(title.chart)
    g <- g +  xlab("") + ylab("Response Count")
    #g <- g + theme(axis.text.y  = element_text(size=14,colour="black"))
    g <- g + theme(axis.text.y  = element_text(size=input$xsize,colour="black"))
    g
    
  }
  

  #####################################
  # First Question
  #####################################  

  output$firstplot <- renderPlot({
    
    question.number <- 1
    
    df <- df.list[[question.number]]
    t <- getTitle(question.number)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Second Question
  #####################################  
  
  output$secondplot <- renderPlot({
    
    question.number <- 2
    
    df <- df.list[[question.number]]
    t <- getTitle(question.number)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Third Question
  #####################################  
  
  output$thirdplot <- renderPlot({
    
    question.number <- 3
    #NOTE: df.list2
    df <- df.list2[[question.number]]
    #t <- getTitle(question.number)
    t <- getTitle(3)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Forth Question
  #####################################  
  
  output$forthplot <- renderPlot({
    
    question.number <- 4
    #NOTE: df.list2
    df <- df.list2[[question.number]]
    t <- getTitle(4)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Fifth Question
  #####################################  
  
  output$fifthplot <- renderPlot({
    
    question.number <- 5
    
    df <- df.list[[question.number]]
    t <- getTitle(question.number)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Sixth Question
  #####################################  
  
  output$sixthplot <- renderPlot({
    
    question.number <- 6
    #NOTE: df.list2
    df <- df.list2[[question.number]]
    t <- getTitle(6)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Seventh Question
  #####################################  
  
  output$seventhplot <- renderPlot({
    
    question.number <- 7
    
    df <- df.list[[question.number]]
    t <- getTitle(question.number)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Eigth Question
  #####################################  
  
  output$eigthplot <- renderPlot({
    
    question.number <- 8
    #NOTE: df.list2
    df <- df.list2[[question.number]]
    t <- getTitle(8)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Ninth Question
  #####################################  
  
  output$ninthplot <- renderPlot({
    
    question.number <- 9
    
    df <- df.list[[question.number]]
    t <- getTitle(question.number)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Tenth Question
  #####################################  
  
  output$tenthplot <- renderPlot({
    
    question.number <- 10
    
    df <- df.list[[question.number]]
    t <- getTitle(question.number)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Eleventh Question
  #####################################  
  
  output$eleventhplot <- renderPlot({
    
    question.number <- 11
    
    df <- df.list[[question.number]]
    t <- getTitle(question.number)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
  #####################################
  # Twelfth Question
  #####################################  
  
  output$twelfthplot <- renderPlot({
    
    question.number <- 12
    
    df <- df.list2[[question.number]]
    t <- getTitle(question.number)
    g <- createStandardPlot(df=df,title.chart=t)
    print(g)
    
  })
  
      
  ############################
  # Data Tab: Data Widget
  ############################
  
  output$df_melt = renderDataTable({
    # for the data table widget
    d <- survey.data
    #d <- subset(d,variable != "cutoff")
    #d <- d[!(colnames(d) %in% c("variable"))]
    d
  })
  
})