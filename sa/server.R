library("reshape2") #needed for melt function
library("ggplot2")

shinyServer(function(input, output) {
  
  pop <- data.frame(males.25.29.in.2010 = 311499,
                    females.25.29.in.2010 = 315537 )
  
  pop$males.28.in.2010   <- pop$males.25.29.in.2010 * 1/5
  pop$females.28.in.2010 <- pop$females.25.29.in.2010 * 1/5
  pop$males.28.in.2011   <- pop$males.28.in.2010 * 1.014 #pop growth rates
  pop$males.28.in.2012   <- pop$males.28.in.2011 * 1.014 #pop growth rates
  pop$females.28.in.2011 <- pop$females.28.in.2010 * 1.014 #pop growth rates
  pop$females.28.in.2012 <- pop$females.28.in.2011 * 1.014 #pop growth rates
  
  pop$total <- pop$males.28.in.2012 + pop$females.28.in.2012 
  
  # new data - only 28 year olds who were transported to the hospital
  emer.calls.28.res.calls <- 430 + 385 + 344  
  
  # annualized prob of someone needing a call
  emer.calls.perperson <- emer.calls.28.res.calls / pop$total
  # probability that they need the call in 1 particular week
  emer.calls.perperson.perweek <- 1 - ((1 - emer.calls.perperson ) ^ (1 / 52))
  emer.calls.perperson.perweek
  #detach(emer.calls.28.res)
  
  
  prob.of.call <- reactive(function() {
    tenants <- input$tenants
    prob.of.call <- 1 - ( (1 - emer.calls.perperson.perweek ) ^ tenants)
    prob.of.call
    
  })  
  
  # time to pass is in hours, ie 1.25 = 1 hour and 15 mins
  Time.To.Pass.In.Min <- reactive(function() {
    t <- (input$train.length / input$train.speed ) * 60 
    t
  })
  
  ## Probabilty that Train will be in the way
  Time.Blocked.Per.Week <- reactive(function() {
    t <- input$dpw * 2 * Time.To.Pass.In.Min() 
    t
  })
  
  
  # prob that a train is in the way, per week.  mins per week = 60 mins * 24 hours * 7 days
  Prob.Blocked.Per.Week  <- reactive(function() {
    t <- Time.Blocked.Per.Week() / ( 60 * 24 * 7)
    t
  })
  
  conflict.prob.year <- reactive(function() {
    
    # per week
    conflict.prob <- Prob.Blocked.Per.Week() * prob.of.call()
    
    # per 1 year
    conflict.prob.year <- 1 - ( 1 - conflict.prob) ^ 52
    conflict.prob.year
  })  
  
  scenario.years <- reactive({ 
    #"Scenario 1: 24 Months" = "scen1"
    #"Scenario 2: 72 Months" = "scen2"
    #"Scenario 3: 50 Years"  = "scen3"
    
    if      (input$scenario=="scen1")
    { return(2) }
    else if (input$scenario=="scen2")
    { return(5) } 
    else if (input$scenario=="scen3")
    { return(50) } 
    else {return(NULL)}
  })
  
  train.prob <- reactive({
    p <- 1 - ( 1 - conflict.prob.year()) ^ scenario.years()
    p  
  })  
  
  MaxDelay <- reactive({
    d <- Time.To.Pass.In.Min() / 2
    d  
  })  
  
  AvgDelay <- reactive({
    d <- MaxDelay() 
    d  
  })  
  
  inputdata <- reactive({
    
    df <- data.frame(
      GoogleMapTime = c(6,5,0.766666667),
      AvgDelay = rep(AvgDelay(),3),
      MaxDelay = rep(MaxDelay(),3),
      row.names = c("Engine 11 via 33rd","Engine 11 via 36th","Engine 7")
    )
    
    return(df)
    
  })
  
  graph.title <- reactive({
    gt <- paste(input$train.length," miles",sep="")
    gt
  })  
  
  m.input.data <- reactive({
    
    input.data <- inputdata()
    input.data$category <- row.names(input.data)
    m.input.data <- melt(input.data, id.vars = "category")  
    return(m.input.data)
    
  }) 
  
  output$probplot <- renderPlot({ 
    
    #mc.box.offset <- 4.9
    
    p <- ggplot(m.input.data(), aes(category, value, fill=variable),stat="indentity") + geom_bar() + scale_x_discrete(limits=c("Engine 7","Engine 11 via 36th","Engine 11 via 33rd"))
    p <- p + xlab("Engine and Route") + ylab("Travel Time, Minutes")
    
    p <- p + scale_fill_manual(values = c("MaxDelay" = "#FFA500","AvgDelay" = "#CD8500","GoogleMapTime" = "#00688B"),
                               name="",
                               breaks=c("MaxDelay", "AvgDelay", "GoogleMapTime"),
                               labels=c("Maximum Delay", "Average Delay " ,"Average Travel Time (Google Maps)"))
    #p <- p + opts(legend.position = "bottom",legend.direction = "vertical")
    p <- p + opts(legend.position = "right",legend.direction = "vertical")
    #p <- p + geom_text(aes(1, mc.box.offset, label="Meck County Avg"),colour='black',size=5) 
    
    # add or remove the Meck County Avg Response time line
    if(input$charmeckavg==TRUE) {
      p <- p + geom_hline(aes(yintercept=4.566666667))
      p <- p + geom_text(aes(1,  4.9, label="Meck County Avg"),colour='gray40',size=4) 
    }
    
    p <- p + ggtitle(paste("Response Times to Site\nTrain Length: ",graph.title(),sep=""))
    print(p)
    
  })
  
  
  datasummary <- reactive({
    
    if      (input$scenario=="scen1")
    { scen.selected <- "Scenario 1: 24 Months"}
    else if (input$scenario=="scen2")
    { scen.selected <- "Scenario 2: 72 Months"} 
    else if (input$scenario=="scen3")
    { scen.selected <- "Scenario 3: 50 Years"} 
    else {scen.selected <- "debug me"}
    
    df <- data.frame(
      Value = c(scen.selected,
                input$tenants,
                input$dpw,
                input$train.length,
                input$train.speed,
                round(Time.To.Pass.In.Min(),2),
                round(AvgDelay(),2), 
                round(MaxDelay() * 2,2),
                round(train.prob(),3)
      )
    )
    
    rownames(df)= c("Scenario",
                    "Tenants",
                    "Trains Per Week",
                    "Train Length",
                    "TrainSpeed",
                    "Train Time to Pass (in min)",
                    "Average Delay (if any occurs)",
                    "Max Delay (if any occurs)",
                    "Probability During Scenario Time Frame"
    )
    
    df
  })
  
  output$datasummary <- renderTable({
    ds <- datasummary()
    ds
  })
  
  output$downloadSummary <- downloadHandler(
    filename = function() { paste("Scenario", '.csv', sep='') },
    content = function(file) {
      write.csv(datasummary(), file)
    }
  )
  
  
  output$probtext <- renderText( {
    p <- round(train.prob(),3)
    print(paste("Probability that a delay will occur during the scenario time frame: ",p,sep=""))
  })
  
  #use this to put a variable in to see its value
  output$whatami <- renderText( {
    print(paste("Current value: ",graph.title(),sep=""))
  })
  
  
})
