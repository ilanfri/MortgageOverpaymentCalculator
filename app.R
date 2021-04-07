library(shiny)
library(ggplot2)
library(reshape2) #melt()

repaymentCalc = function(principal, interestrate, term){
  i = 0.01*1/12*interestrate
  n = 12*term
  monthlyrepayment = principal*i*(1+i)^n/((1+i)^n-1)
  totalrepayment = n*monthlyrepayment
  return(list(monthlytotal=monthlyrepayment, finaltotal=totalrepayment))
}

repaymentCalcDetailed = function(principal, interestrate, term, monthlypayment){
  
  monthlyinterest = 0.01*interestrate/12
  months = seq(12*term)
  principalrepayments = c()
  
  interestpayments = c()
  remainingprincipals = c()
  remainingprincipal = principal
  
  for(i in months){
    interestpayment = remainingprincipal*monthlyinterest
    principalrepayment = monthlypayment - interestpayment
    principalrepayments = c(principalrepayments, principalrepayment)
    remainingprincipal = principal - sum(principalrepayments)
    if(remainingprincipal < 0){
      principalrepayments = principalrepayments[-length(principalrepayments)]
      break
    }
    interestpayments = c(interestpayments, interestpayment)
    remainingprincipals = c(remainingprincipals, remainingprincipal)
  }
  
  #print(sum(principalrepayments))
  #stopifnot(abs(sum(principalrepayments) - principal) < 10.0)
  
  return(list(interest=interestpayments, principal=principalrepayments, remainingprincipal=remainingprincipals))
}


################################################################

ui = fluidPage(

  title = "MortgageOverpaymentCalculator",
  
  sidebarPanel(
    numericInput("principal", "Mortgage value (GBP):", 450000, min = 100000, max = 10000000),
    numericInput("interestrate", "Interest rate (%):", 4, min = 0.05, max = 20),
    numericInput("duration", "Duration (years):", 25, min = 0.5, max = 40),
    
    sliderInput("repayment", "Monthly repayment:", min = 500, max = 5000, value = 2375.27, step = 0.01),
    
    textOutput(outputId = "instructions"),
    
    verbatimTextOutput(outputId = "author")
    
  ),
  mainPanel(
    #plotOutput(outputId = "desiredpstcd")
    #leafletOutput("desiredpstcd"),
    
    #textOutput("interest"),
    htmlOutput("interest"),
    plotOutput('plot1'),
    textOutput(outputId = "explanation"),
    
    verbatimTextOutput(outputId = "sharinginstructions")
  )
    #textOutput(outputId = "testing")
)

server = function(input, output, session){
  
  observe({
    #minpaymentchange = repaymentCalc(input$principal, input$interestrate, input$duration)
    
    repay = repaymentCalc(input$principal, input$interestrate, input$duration)

    updateSliderInput(session, "repayment", value = repay$monthlytotal,
                      min = round(repay$monthlytotal,2), max = round(repay$monthlytotal + 0.1*input$principal/12,2), step = 0.01)
  })
  
  output$plot1 <- renderPlot({
    #palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    #          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    #par(mar = c(5.1, 4.1, 0, 1))
    #plot(selectedData(),
    #     col = clusters()$cluster,
    #     pch = 20, cex = 3)
    #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
    repaydetailed = repaymentCalcDetailed(input$principal, input$interestrate, input$duration, input$repayment)
    
    df = data.frame(i=seq_along(repaydetailed$interest), principal=repaydetailed$principal, interest=repaydetailed$interest, remaining=repaydetailed$remainingprincipal)
    stackdf = melt(df[c("i","principal","interest")], id.vars = "i")
    stackdf$variable = relevel(stackdf$variable, "interest")
    #plot(seq(length(repaydetailed$remainingprincipal)), repaydetailed$remainingprincipal)

    stackdf = merge(stackdf, df[c("i","remaining")], by="i")
    
    ggplot(stackdf, aes(x=i)) + geom_bar(aes(y=value, fill=variable), position="stack", stat="identity") +
      theme(legend.title = element_blank()) + theme(legend.position="bottom") +
      xlim(0, 12*input$duration) + xlab("Months") + ylab("Monthly repayment (GBP)")
      #geom_line(aes(y=remaining)) + scale_y_continuous(name = "Monthly repayment (GBP)", sec.axis = sec_axis(~., name="Principal remaining"))
    # + 
  })
  
  output$interest <- renderText({ 
    repaydetailed = repaymentCalcDetailed(input$principal, input$interestrate, input$duration, input$repayment)
    #paste("Total interest paid: ", round(sum(repaydetailed$interest),0))
    
    nmonths = length(repaydetailed$interest)
    if(repaydetailed$remainingprincipal[length(repaydetailed$remainingprincipal)] > 1.0){
      nmonths = nmonths + 1
    }
    #nmonths = nmonths + 1
    nyears = floor(nmonths/12)
    nmonthsadditional = nmonths %% 12
    
    paste0("<b><center>Total interest paid: £", format(round(as.numeric(sum(repaydetailed$interest)), 0), nsmall=0, big.mark=" "), 
          "<br>Time taken to pay off: ", nyears, " years, ", nmonthsadditional, " months", "</b></center>")
  })
  
  #output$caveat = renderPrint({cat("(Values too small will enclose no Tube station and therefore give an error.)")})
  
  #output$caveat = renderPrint({cat("(Geographical middle point is used so travel times for the two parties may differ significantly, and suggested middle point may be a bit odd. Fun times. \n Route calculation may take a little while.)")})
  
  output$explanation = renderPrint({cat("Colours show what fraction of monthly payment goes towards capital repayment and towards interest. Total interest paid and time to pay off mortgage shown above graph. Intended for exploratory purposes only, not to be used as financial advice. No liability of any kind accepted. Consult with your mortgage provider for accurate figures.")})
  
  output$instructions = renderPrint({cat("Minimum repayment shown is default repayment as given by online mortgage calculators. Maximum slider value corresponds to common 10% annual limit on overpayment. Move slider to the right to see how overpayment impacts total interest paid and time taken to pay off mortgage.")})
  
  output$author = renderPrint({cat("Ilan Fridman Rojas ©, 2021")})
  
  output$sharinginstructions = renderPrint({cat("To save app-like shortcut to this tool:\n iOS: Share button > Add to Home Screen\n Android: Three dots button > Add to Home Screen")})

}

shinyApp(ui=ui, server=server)