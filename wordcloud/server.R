
library(shiny)
library(tm)
library(wordcloud)
#library(Snowball) #for stemming

shinyServer(function(input, output) {
  
  output$caption1 <- renderTable( {
    
    if (is.null(input$file1)) { return(NULL) }
    inFile <- input$file1
    inFile
    
  })
  
  output$customstopwords <- renderText( {
    print(paste("Current value of user defined custom stop words: ",input$customstop,sep=""))
  })
  
  
  folder.chosen <- reactive({ 
    if (input$inputmethod=="normal") 
    { return(substr(input$dataset, 1, regexpr("\\/",input$dataset)[1])) }
    else { return(NULL) } 
  })
  
  # Return the requested dataset
  datasetInput <- reactive({
    
    filepath<-paste('~/ShinyApps/wordcloud/data/',folder.chosen(),sep="")
    current.corpus <- Corpus(DirSource(filepath), readerControl = list(language = "eng"))
    return(current.corpus)
    
  })
    
  datasetInputCustomUpload <- reactive({
    
    if(isolate(input$inputmethod)=="userdefined") {
      inFile <- input$file1
      if (is.null(inFile)) { return(NULL) } else {
        upload.path <- substr(inFile$datapath,1,nchar(inFile$datapath)-1)
        current.corpus <- Corpus(DirSource(upload.path), readerControl = list(language = "eng"))
        return(current.corpus)
      }
    } else { return(NULL) }  
  })    
  
  
  #transform.corpus <- reactive(function() {
  corpus.choice <- reactive(function() {
    
    if(input$inputmethod=="normal") {
      if(is.null(datasetInput())) return(NULL)
      b <- datasetInput() 
    } else {
      if(input$inputmethod=="userdefined") {
        inFile <- input$file1
        if(is.null(isolate(datasetInputCustomUpload()))) return(NULL)
        b <- isolate(datasetInputCustomUpload())
      }
    }
  })
  
  transform.corpus <- reactive(function() {
    
    b <- corpus.choice()
    
    if(input$lowercase) {
      b <- tm_map(b, tolower) }
    
    if(input$stripwhite) {
      b <- tm_map(b, stripWhitespace) } 
    
    if(input$removepunc) {
      b <- tm_map(b, removePunctuation) }
    
    b <- tm_map(b, removeWords, as.character(unlist(strsplit(input$customstop,",")),mode=list)) # remove user defined words
    
    if(input$stopwords) { 
      b <- tm_map(b, removeWords, stopwords("english")) }
    
    #if(input$stem) { 
    #  b <- tm_map(b, stemDocument) }
    
    tdm <- TermDocumentMatrix(b)
    m1 <- as.matrix(tdm)
    v1 <- sort(rowSums(m1),decreasing=TRUE)
    return(data.frame(word = names(v1),freq=v1))
    
  })
  
  
  #generate wordcloud
  output$wordcloudplot <- renderPlot({ 
    
    dt <- transform.corpus()
    wordcloud(dt$word,dt$freq)
    
  })
  
  #generate frequency table
  output$freqtable <- renderTable({ 
    
    dt <- transform.corpus()
    dt <- subset(dt,freq>2)
    rownames(dt) <- NULL
    colnames(dt) <- c("Word","Frequency")
    dt
  })
  
  output$inputtext <- renderText( {
    b <- corpus.choice()
    b[[1]]
  })                           
  
  formatDownload <- reactive({
    dt <- transform.corpus()
    rownames(dt) <- NULL
    colnames(dt) <- c("Word","Frequency")
    dt       
  })
  
  output$downloadFreq <- downloadHandler(
    filename = function() { paste("Word Cloud Frequencies", '.csv', sep='') },
    content = function(file) {
      write.csv(formatDownload(), file)
    }
  )
  
  
})