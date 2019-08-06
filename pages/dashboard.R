library(shiny)
library(shinydashboard)


dashboard_ui = function(){
  
page = tabItem(tabName="dashboard",
        shinydashboard::tabBox (title="Current Property Dashboard",width=12, side = "left",        
        tabPanel("Summary Costs",
                 fluidRow(
                  h2(strong("Top Level Data View")),
                  box(
                    dataTableOutput("cost_by_property"),
                    
                    title = "Total Cost by Property", 
                    solidHeader=TRUE,
                    collapsible = TRUE,
                    width = 12)
                  # Show a plot of the generated distribution
                  
                )
            ),
        
        tabPanel("Annual Spending Trends", 
          fluidRow(
             box(plotOutput("distPlot"), collapsible = TRUE, width = 12),
             box(dataTableOutput("cost_by_year"), collapsible = TRUE, width = 12)
          )
        )
        )
      )
return(page)

}


dashboard_serv = function(input, output, session, filt_sum, year_sum){
  year_sum_display = year_sum
  year_sum_display$Labor = paste0("$", format(round(year_sum_display$Labor), big.mark = ","))
  year_sum_display$Material = paste0("$", format(round(year_sum_display$Material), big.mark = ","))
  year_sum_display$Total = paste0("$", format(round(year_sum_display$Total), big.mark = ","))
  
  #dash page
  output$cost_by_property <- DT::renderDataTable({filt_sum})

  #Annual Page
  output$cost_by_year <- renderDataTable({year_sum_display})
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    barplot(year_sum$Total, 
            main= "PG Living Rehab Costs", 
            xlab = "Year",
            ylab= "$M" ,
            names.arg = year_sum$Year, 
            col = rgb(137/255,207/255,240/255),
            yaxt="n")
    axis(2, axTicks(2), paste0("$", format(axTicks(2)/1000000, big.mark=",", scientific = F)), las=1) 
  })
  
  
  
}