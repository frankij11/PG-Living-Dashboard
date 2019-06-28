#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readxl)
library(dplyr)
#library(sclaes)
library(ggplot2)


rehab_costs <- read_excel("rehab_costs.xlsx", 
                          sheet = "Data", col_types = c("text", 
                                                        "date", "text", "text", "numeric", 
                                                        "numeric", "text", "text", "text", 
                                                        "text", "numeric"))

rehab_costs$Property = toupper(rehab_costs$Property)
rehab_costs$Year = format(rehab_costs$Date, "%Y")
rehab_costs$Month = format(rehab_costs$Date, "%m")


basic_sum = summarise(group_by(rehab_costs, Property),Total = sum(Amount),  Start_Date = min(Date), End_Date=max(Date) )
basic_sum$Duration = round(difftime(basic_sum$End_Date, basic_sum$Start_Date, units="weeks"),2)
#basic_sum$Total = scales::dollar(basic_sum$Total)

filt_sum <- filter(basic_sum, End_Date > "2018-01-01")
filt_sum <- filt_sum[order(filt_sum$End_Date,decreasing=TRUE),]
filt_sum$Total = paste0("$", format(round(filt_sum$Total), big.mark = ","))

filt_sum[order("End_Date")]

year_sum = summarise(group_by(rehab_costs, Year), Total = sum(Amount))
year_sum = filter(year_sum, Year > 2010)
barplot(year_sum$Total, names.arg = year_sum$Year)









library(shiny)
library(shinydashboard)
library(DT)


# Define UI for application that draws a histogram
ui <- dashboardPage(
   
   # Application title
   dashboardHeader(title="PG Living Dashboard"),
   
   # Sidebar with a slider input for number of bins 
   dashboardSidebar(
      sidebarMenu(
         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
         menuItem("Spending Trends", tabName = "widgets", icon = icon("th"), 
                  menuSubItem("Annual Update", tabName = "annual")),
         menuItem("Potential Properties", icon = icon("th"), 
                  menuItem("Find Comparables", tabName= "find_comps"),
                  menuItem("Auction Properties", tabName = "auction"),
                  menuItem("Deal Calculator", tabName = "deal_calc")
         )
      )
   ),
   
   dashboardBody(
      tabItems(
         tabItem(tabName="dashboard",
                 fluidRow(
                    # valueBox(100, "stat 1" ,icon("ion ion-stats-bars")),
                    # valueBox(2, "stat 2" ,icon("ion ion-stats-bars"), color="green"),
                 ),
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
         tabItem(tabName="annual", 
                 h2(strong("Yearly Spending Trends")),   
                 box(plotOutput("distPlot"), collapsible = TRUE, width = 12),
                 box(dataTableOutput("cost_by_year"), collapsible = TRUE, width = 12)
                 
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$cost_by_property <- DT::renderDataTable({filt_sum})
   
   output$cost_by_year <- renderDataTable({year_sum})

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

# Run the application 
shinyApp(ui = ui, server = server)

