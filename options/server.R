library(shiny)

#####################################
# Define Non-Reactive Functions
#####################################


readYahooOptions <- function(Symbols, Exp, ...){   parse.expiry <- function(x) { 
  if(is.null(x)) 
    return(NULL)
  
  if(inherits(x, "Date") || inherits(x, "POSIXt"))       return(format(x, "%Y-%m"))
  
  if (nchar(x) == 5L) {
    
    x <- sprintf(substring(x, 4, 5), match(substring(x,
                                                     1, 3),
                                           month.abb), fmt = "20%s-%02i")
  } 
  else if (nchar(x) == 6L) {
    
    x <- paste(substring(x, 1, 4), substring(x, 5, 6),
               sep = "-")
    
  } 
  return(x) 
}
                                                   clean.opt.table <- function(tableIn) { 
                                                     tableOut <- sapply(tableIn[,-2], function(x) as.numeric(gsub(",","",x)))     
                                                     rownames(tableOut) <- tableIn[,2] 
                                                     tableOut 
                                                   }
                                                   
                                                   if(missing(Exp)) {
                                                     optURL <- paste(paste("http://finance.yahoo.com/q/op?s",Symbols,sep="="),"Options",sep="+") }  else { 
                                                       optURL <- paste(paste("http://finance.yahoo.com/q/op?s=",Symbols,"&m=",parse.expiry(Exp),sep=""),"Options",sep="+")
                                                     }
                                                   if(!missing(Exp) && is.null(Exp)) { 
                                                     optPage <- readLines(optURL) 
                                                     optPage <- optPage[grep("View By Expiration", optPage)]
                                                     
                                                     allExp <- gregexpr("m=", optPage)[[1]][-1] + 2
                                                     allExp <- substring(optPage, allExp, allExp + 6)
                                                     allExp <- allExp[seq_len(length(allExp)-1)] # Last one seems useless ?
                                                     return(structure(lapply(allExp, readYahooOptions, Symbols=Symbols), .Names=format(as.yearmon(allExp))))   }
                                                   stopifnot(require("XML"))
                                                   
                                                   optURL <- readHTMLTable(optURL)
                                                   
                                                   # Not smart to hard code these but it's a 'good-enough' hack for now   # Also, what is table 9 on this page?
                                                   
                                                   list(calls = clean.opt.table(optURL[[10]]),
                                                        
                                                        puts = clean.opt.table(optURL[[14]]),
                                                        symbol = Symbols)
                                                   
                                                   
                                                   
                                                   
} #end of readYahooOptions


shinyServer(function(input, output) {
  
  expiration <- reactive({
    year  <- input$expiryYear
    month <- input$expiryMonth
    
    if (month < 10 ) {
      formattedDate <- paste(year,"-0",month,sep="")
    } else {
      formattedDate <- paste(year,"-",month,sep="")
    }
    
    return(formattedDate)
    
    })
  
  
  callOptionDF <- reactive({
  
  options <- readYahooOptions(input$ticker,Exp=expiration())
  calls.df <-  as.data.frame(options$calls)
  #colnames(calls.df) <- c("Strike","Last","Change","Bid","Ask","Volume","Open_Interest")
  #calls.df$Log_Volume <- log(calls.df$Volume)
  #calls.df$Log_Open_Interest <- log(calls.df$Open_Interest)
  
  return(calls.df)
  
  })
  
  #############################
  # Output
  #############################
  
  #generate frequency table
  output$callsTable <- renderTable({ 
    
    dt <- callOptionDF()
    #dt <- subset(dt,freq>2)
    #rownames(dt) <- NULL
    #colnames(dt) <- c("Word","Frequency")
    dt
  })
  
  output$expiryText <- renderText({
    e <- expiration()
    e
  }) 
  
  
})