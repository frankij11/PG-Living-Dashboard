library(shiny)
library(shinydashboard)


deal_calc_ui = function(){
  
  
  page2 = tabItem(tabName="deal_calc", 
                  h1(strong("Deal Calaculator")),
                box(width = 8,  
                  fluidRow(
                    box(title="Deal Summary",width=12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                          valueBoxOutput("valbox_best_offer"),
                          valueBoxOutput("valbox_profit"),
                          valueBoxOutput("valbox_cash")
 
                          
                        ),

                    box(title="Financing Breakdown",width=12, solidHeader = TRUE, collapsible=TRUE, status="warning",
                        valueBoxOutput("valbox_loan_amount"),
                        valueBoxOutput("valbox_cash_closing"),
                        valueBoxOutput("valbox_holding_costs")
                        )
                  ),
                  fluidRow(
                    box(width=12,
                        column(12, align="center",
                               
                               column(6, sliderInput("sldr_sales_price", "After Repair Value (ARV)", min = 1000, max=1000000, value=300000, step=5000)),
                               column(6,sliderInput("sldr_profit", "Profit", min=0, max=200000,value=25000, step=1000)),
                               column(6,sliderInput("sldr_cash_at_closing", "Cash at Closing", min=0, max=1,value=.12)),
                               column(6, sliderInput("sldr_rehab", "Rehab Costs", min=0, max=200000,value=65000, step=1000)),
                               column(6, sliderInput("sldr_project_days", "Project Days",min= 0, max=365, value=180)),
                               column(6, sliderInput("sldr_interest_rate", "Interest Rate", min = 0, max=.25, value=.08)),
                               column(6, sliderInput("sldr_selling_cost", "Selling Costs", min = 0, max=.20, value=.08)),
                               column(6, sliderInput("sldr_closing_cost", "Buying Costs",min= 0, max=.20,value= .06))
                               
                        )
                    )
                  )
  )
  )
  return(page2)
  
}


deal_calc2_ui = function(){
page =column(width=12,align="left",
  #uiOutput("num_best_offer"),
  valueBoxOutput("num_best_offer", width = 12),
  #numericInput("num_best_offer",NULL, value=NULL ),
  column(12, h4(strong("Inputs"))),
  numericInput("num_sales_price","ARV", min = 1000, max=1000000, value=300000, step=5000),
  numericInput("num_profit", "Profit $", min=0, max=200000,value=25000, step=1000),
  numericInput("num_cash_at_closing", "Cash %", min=0, max=1,value=.12),
  numericInput("num_rehab", "Rehab $", min=0, max=200000,value=65000, step=1000),
  numericInput("num_project_days", "Days",min= 0, max=365, value=180),
  numericInput("num_interest_rate","APY %", min = 0, max=.25, value=.08),
  numericInput("num_selling_cost", "Sell %", min = 0, max=.20, value=.08),
  numericInput("num_closing_cost","Buy %",min= 0, max=.20,value= .06)
)

return(page)
}

deal_calc_serv = function(input, output, session){

  #best_offer =function(SalesPrice, Rehab, ProjectDays = 180, Profit=None, CashOnCashReturn = 0.5, NetSale = 0.92, ClosingPercent = 0.06, InterestRate = 0.08, CashAtClosingPercent = 0.12){
  #side bar
  best2 = reactive({
    best_offer(SalesPrice = input$num_sales_price,
               Rehab = input$num_rehab,
               ProjectDays = input$num_project_days,
               Profit = input$num_profit,
               NetSale = 1 - input$num_selling_cost,
               ClosingPercent = input$num_closing_cost,
               InterestRate = input$num_interest_rate,
               CashAtClosingPercent = input$num_cash_at_closing)
    
    
  })
  
  #output$num_best_offer <- renderUI({numericInput("ui_best_offer","Best Offer", best() )})
  output$num_best_offer <- renderValueBox({valueBox("Best Offer",format(round(best2()), big.mark = ","))})
#    input$num_sales_price
#  input$num_profit
  
  # observeEvent(input$num_sales_price,{updateNumericInput(session, "num_best_offer", value = format(big.mark=",",round(best2() ) ) )} )
  # observeEvent(input$num_profit,{updateNumericInput(session, "num_best_offer", value = format(big.mark=",",round(best2() ) ) )} )
  # observeEvent(input$num_project_days,{updateNumericInput(session, "num_best_offer", value = format(big.mark=",",round(best2() ) ) )} )
  # observeEvent(input$num_rehab,{updateNumericInput(session, "num_best_offer", value = format(big.mark=",",round(best2() ) ) )} )
  # observeEvent(input$num_selling_cost,{updateNumericInput(session, "num_best_offer", value = format(big.mark=",",round(best2() ) ) )} )
  # observeEvent(input$num_closing_cost,{updateNumericInput(session, "num_best_offer", value = format(big.mark=",",round(best2() ) ) )} )
  # observeEvent(input$num_interest_rate,{updateNumericInput(session, "num_best_offer", value = format(big.mark=",",round(best2() ) ) )} )
  # observeEvent(input$num_cash_at_closing,{updateNumericInput(session, "num_best_offer", value = format(big.mark=",",round(best2() ) ) )} )

  
  best = reactive({
    
    best_offer(SalesPrice = input$sldr_sales_price,
               Rehab = input$sldr_rehab,
               ProjectDays = input$sldr_project_days,
               Profit = input$sldr_profit,
               NetSale = 1 - input$sldr_selling_cost,
               ClosingPercent = input$sldr_closing_cost,
               InterestRate = input$sldr_interest_rate,
               CashAtClosingPercent = input$sldr_cash_at_closing)
  })
  
  cash_closing = reactive({
    
    input$sldr_cash_at_closing * (input$sldr_rehab+best() )
    
  })
  
  financed = reactive({input$sldr_rehab + best() - cash_closing()})
  
  holding = reactive({financed()* input$sldr_interest_rate/365 * input$sldr_project_days})
  
  cash = reactive({cash_closing() + holding()})
  
  prof_percent = reactive({(input$sldr_profit / cash()*100)})
  
  
  
  output$valbox_best_offer <- renderValueBox({
    valueBox(
      #format(big.mark = ",",best(),digits=0),
      format(big.mark = ",", round(best())),
      "Best Offer",
      color="blue")
  })
  
  output$valbox_profit <- renderValueBox({
    valueBox(
      paste0(format(round(prof_percent(), 2), digits=2, nsmall = 2), "%" ) ,
      "Profit (% of Cash Need)",
      color="green")
  })
  
  output$valbox_loan_amount<- renderValueBox({
    valueBox(
      format(big.mark = ",",round(financed())),
      "Loan Amount",
      color="red")
  })         
  
  output$valbox_cash <- renderValueBox({
    valueBox(
      format( round(cash()),big.mark = ","),
      "Cash Needed",
      color="blue")
  })     
  
  
  
  
  output$valbox_cash_closing <- renderValueBox({
    valueBox(
      format(round(cash_closing()),big.mark=","),
      "Cash at Closing",
      color="red")
  })         
  
  output$valbox_holding_costs  <-renderValueBox({
    valueBox(
      format(round(holding()),big.mark=",") ,
      "Interest Costs",
      color="red")
  })          
}

