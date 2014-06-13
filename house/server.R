library(rjson)#To read json files
library(RCurl)#To get the result of an url
library(shiny)
#library("reshape2") #needed for melt function
#library("ggplot2")

shinyServer(function(input, output) {
  
  work_address_plus <-  reactive({
    
    input$goButton
    
    dollabillz <- isolate({ gsub(" ","+",input$work_address) })
    return(dollabillz)
  })
  
  home_address_plus <-  reactive({
    
    input$goButton
    
    hizzy <- isolate({ gsub(" ","+",input$home_address) })
    return(hizzy)
  })
  
  json_get <- reactive({
    
    input$goButton
    
    avoidhwy <- isolate(input$avoidhwy)
    
    if (avoidhwy=="TRUE") {
      
      #includes "avoid=highways" option
      url <- paste("http://maps.googleapis.com/maps/api/distancematrix/json?origins=",isolate(work_address_plus()),
               "&destinations=",isolate(home_address_plus()),
               "&sensor=false",
               "&avoid=highways", 
               "&units=imperial",sep="")
      } else {
      url <- paste("http://maps.googleapis.com/maps/api/distancematrix/json?origins=",isolate(work_address_plus()),
                     "&destinations=",isolate(home_address_plus()),
                     "&sensor=false",
                     "&units=imperial",sep="")
      }
    
    json_data <- fromJSON(paste(readLines(url), collapse=""))
    
    #return the entire json and use other functions to parse the wanted elements
    return(json_data)
  })  
  
  output$disttext <- renderText({
    input$goButton
    d <- json_get()$rows[[1]]$elements[[1]]$distance$text
    print(paste("Distance: ",d,sep=""))
  })
  
  output$durationtext <- renderText({
    input$goButton
    d <- json_get()$rows[[1]]$elements[[1]]$duration$text
    print(paste("Duration: ",d,sep=""))
  })
  
  output$dest_addr_text <- renderText({
    input$goButton
    d <- json_get()$destination_addresses
    h <- input$home_address
    print(paste(h, " resolved to ",d,sep=""))
  })
  
  output$origin_addr_text <- renderText({
    input$goButton
    d <- json_get()$origin_addresses
    w <- input$work_address
    print(paste(w," resolved to ",d,sep=""))
  })
  
  
  output$debug <- renderText( {
    debugme <- json_get()
    debugme
  })  
  
})
