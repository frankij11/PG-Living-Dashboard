library(shiny)
library(shinydashboard)

best_offer = function(){
  
  page = tabItem(tabName="best_offer", 
          h2(strong("Yearly Spending Trends")),   
          box(plotOutput("distPlot"), collapsible = TRUE, width = 12),
          box(dataTableOutput("cost_by_year"), collapsible = TRUE, width = 12)
          
  )
  
  page2 = tabItem(tabName="best_offer", 
                  h2(strong("Deal Calaculator")),
                  sliderInput("best_offer", "Best Offer", 1000, 10000000, 300000),
                  sliderInput("SalesPrice", "Sale Price", 1000, 10000000, 300000),
                  
  )
  
  return(page)

  }

#best_offer =function(SalesPrice, Rehab, ProjectDays = 180, Profit=None, CashOnCashReturn = 0.5, NetSale = 0.92, ClosingPercent = 0.06, InterestRate = 0.08, CashAtClosingPercent = 0.12){
