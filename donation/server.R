
library(shiny)
library(ggplot2)
library(reshape)

shinyServer(function(input, output) {
  
  ##########################
  # Define Static Variables
  ##########################

  #color vars
  profit_color <- "#2DC266"
  loss_color <-   "#E34444"
  
  ##########################
  # Data Transformations
  ##########################
  
  responses <- reactive({ 
    # number of responses
    # number of mailings * response rate (people who actually donated something)
    return(round(input$n_mailings * input$rrate)) 
  })
  
  cost_per_response <- reactive({
    # cost for each donation actually recieved
    # (unit cost * number of mailings) / number of responses
    return((input$mail_uc * input$n_mailings) / responses()) 
  })
  
  campaign_cost <- reactive({
    # total cost of the mailing campaign
    return(input$mail_uc * input$n_mailings) 
  })  
  
  donated <- reactive({
    # Vector of individual donations
    # assume exponential distribution, rexp()
    set.seed(316) # as in John 3:16
    return(round(rexp(responses(),rate=1/input$avg_donation),2)) 
  })
  
  total_donation <- reactive({
    # sum of all individual donations
    return(sum(donated()))
  })
  
  donation_df <- reactive({
    # creates dataframe (df) of donations
    # columns:
    #    donor:  index for each donation
    #    donations:  dollar amount for each donation
    #    color:  color for charts, green for donations, red for costs
    #    cutoff:  cost of each response
    
    cutoff <- cost_per_response()
    df <- donated()
    n <- responses()
    
    donated_sorted <- sort(df,decreasing=TRUE)
    donation_df <- data.frame("donor"=1:n,
                              "donations" = donated_sorted)
    donation_df$color <- ifelse(donation_df$donations>cutoff,
                                profit_color,loss_color)
    donation_df$cutoff <- rep(cost_per_response(),n)
    ######################donation_df$age <- donations 
    return(donation_df) 
  })
  
  donation_df_melt <- reactive({
    # melted version of donation_df, and removes color column
    df <- donation_df()
    df <- df[!(colnames(df) %in% c("color"))]
    melt_df <- melt(df,id=c("donor"))
    return(melt_df) 
  })
    
  d_profit <- reactive({
    #subseted version of donation_df, only where donation exceeded cost per response
    cutoff <- cost_per_response()
    df <- donation_df()
    return(subset(df,donations > cutoff)) 
  })
  
  d_loss   <- reactive({
    #subseted version of donation_df, only where donation was less than cost per response
    cutoff <- cost_per_response()
    df <- donation_df()
    return(subset(df,donations <= cutoff))
  })
  
#####################################################################
# Output Definitions
#####################################################################
  
  ##########################
  # Tab 1: Overview
  ##########################
  
  output$overviewText <- renderText({
    t <- "Problem Statement: <add problem statement>"
    t
  })  
  
  ############################
  # Tab 2: Donations Analysis
  ############################
  
  output$donationText <- renderText({
    t <- "<add analysis of randomly generated donations>\n\nPerhaps a boxplot, then ordered chart"
    t
  })  
  
  ############################
  # Tab 3: Net Gain/Loss
  ############################
  
  output$breakevenplot <- renderPlot({
    #generate breakeven plot
    d_profit <- d_profit()
    d_loss <- d_loss()
    
    p <- ggplot(donation_df(),aes(x=donor))
    p <- p + xlab("Doner Index Number, (sorted)") + ylab("Dollars")
    p <- p + geom_ribbon(data=d_profit(),
                         aes(ymin=cutoff,
                             ymax=donations,
                             fill=color
                             )
                         ) 
    p <- p + geom_ribbon(data=d_loss(),
                         aes(ymin=donations,
                             ymax=cutoff,
                             fill=color
                             )
                         )
    p <- p + scale_fill_manual(values=c(profit_color,loss_color))
    #p <- p + scale_fill_manual(values=c("#2DC266","#E34444"))
    p <- p + theme(legend.position = "none") 
    print(p)    
  })
  
  output$lineplot <- renderPlot({
    # alternate version of the breakeven plot
    df <- donation_df_melt()
    
    h <- ggplot(df,aes(x = donor, y = value, fill=variable,group=variable)) 
    h <- h + geom_area(alpha=0.40,position = 'identity') + scale_fill_manual(values = c('green','red'))
    h <- h + geom_line(aes(ymax=value), position="identity")
    h <- h + xlab("Doner Index Number, (sorted)") + ylab("Dollars")
    h <- h + theme(legend.position = "none")
    print(h)
  })
  
  output$donationSummary <- renderText({
    t <- paste("Total Donation Amount: $",total_donation(),sep="")
    t
  })
  
  output$campaignCostSummary <- renderText({
    t <- paste("Total Cost for Mailing Campaign: $",campaign_cost(),sep="")
    t
  })
  
  output$netSummary <- renderText({
    net_amount <- total_donation() - campaign_cost()
    if(net_amount > 0) {
      t <- paste("Net Gain: $",net_amount,sep="")
    } else {
      t <- paste("Net Loss: ",net_amount,sep="")
    }
    return(t)
  })
  
  ############################
  # Tab 4: Deeper Analysis
  ############################
  
  output$predModelText <- renderText({
    t <- "Can we use predictive modeling to only send mailings to 
    the top 50% of people who are most likely to respond?"
    t
  })
  
  ############################
  # Tab 5: Data Widget
  ############################
  
  output$df_melt = renderDataTable({
    # for the data table widget
    d <- donation_df_melt()
    d <- subset(d,variable != "cutoff")
    d <- d[!(colnames(d) %in% c("variable"))]
    d
  })  
  
})