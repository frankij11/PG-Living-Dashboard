#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("rei.R")
auction_props <- auction_hw()
my_offer = best_offer(290, 55, 25)
print(my_offer)

source("pages/deal_calc.R")
best_offer = best_offer() #h2("best offer") #best_offer()

source("data_analysis.R")
filt_sum <- get_filt_sum()
year_sum <- get_year_sum()

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
                 
         ),
         
         tabItem(tabName="auction",
                 fluidRow(
                    # valueBox(100, "stat 1" ,icon("ion ion-stats-bars")),
                    # valueBox(2, "stat 2" ,icon("ion ion-stats-bars"), color="green"),
                 ),
                 fluidRow(
                    h1(strong("Auction Properties")),
                    box(
                       dataTableOutput("auction_frame"),
                       
                       title = "List of Auction Properties", 
                       solidHeader=TRUE,
                       collapsible = TRUE,
                       width = 12)
                    # Show a plot of the generated distribution
                    
                 )
         )
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$cost_by_property <- DT::renderDataTable({filt_sum})
   
   year_sum_display = year_sum
   year_sum_display$Labor = paste0("$", format(round(year_sum_display$Labor), big.mark = ","))
   year_sum_display$Material = paste0("$", format(round(year_sum_display$Material), big.mark = ","))
   year_sum_display$Total = paste0("$", format(round(year_sum_display$Total), big.mark = ","))
   
   output$cost_by_year <- renderDataTable({year_sum_display})

   output$auction_frame <- renderDataTable({auction_props})
   
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

