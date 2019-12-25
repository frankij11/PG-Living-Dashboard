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

source("pages/dashboard.R")
source("pages/deal_calc.R")
source("pages/comps.R")
#best_offer = best_offer() #h2("best offer") #best_offer()

source("data_analysis.R")
filt_sum <- get_filt_sum()
year_sum <- get_year_sum()

library(shiny)
library(shinydashboard)
library(DT)


# Define UI for application that draws a histogram
ui <- dashboardPage(
   # Application title
   dashboardHeader(title = "PG Living Dashboard"),
   
   # Sidebar with a slider input for number of bins
   dashboardSidebar(
      sidebarMenu(
         menuItem(
            "Dashboard",
            tabName = "dashboard",
            icon = icon("dashboard")
         ),
         menuItem(
            "Current Properties",
            tabName = "current_props",
            icon = icon("home")
         ),
         menuItem(
            "Potential Properties",
            tabName = "potential_props",
            icon = icon("dollar")
         ),
         menuItem("Deal Calculator", tabName = "deal_calc"),
         menuItem("Quick Calculator",
                  deal_calc2_ui())
      )
   ),
   
   dashboardBody(tabItems(
      dashboard_ui(),
      
      deal_calc_ui(),
      
      
      tabItem(tabName = "potential_props",
              tabBox(width=12,
                 tabPanel("comps",
                            fluidRow( comp_ui())
                          ),
                 tabPanel(
                    "Auction",
                          fluidRow(
                             box(
                                dataTableOutput("auction_frame"),
                                
                                title = "List of Auction Properties",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 12
                              )
                          # Show a plot of the generated distribution
                          
                        )
                  )
              )
        )
     )
   )
      
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
   deal_calc_serv(input, output, session)
   dashboard_serv(input, output, session, filt_sum, year_sum)
   comp_serv(input, output, session)
   output$auction_frame <- renderDataTable({
      auction_props
   })
   
}

# Run the application
shinyApp(ui = ui, server = server)
