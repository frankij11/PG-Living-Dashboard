library(shiny)
library(shinydashboard)
library(ggmap)
library(ggplot2)
library(dplyr)

source("sdat.R")
source("rei.R")


API_KEY = 'AIzaSyBBTXVoEURxl8IGpTF0vfOp96LJgpKdSZo'
register_google(API_KEY)



get_address = function(address){
  address=geocode(address, "all")  
  return(address)
}

list_addresses = function(address){
  add_choice = list()
  for(i in 1:length(address$results)){
    add_choice = append(address$results[[i]]$formatted_address, add_choice)
  }
  
  return(add_choice)
}


comp_ui = function(){
  
  page =  box(title="Find Comparable Houses", status="primary", width=12,
              fluidRow(
                column(4,textInput("txt_address", "Search Address", "1303 Alberta Dr")),
                column(2, actionButton("btn_search","Find Address" ))
              ),
              fluidRow(
                column(4,selectInput("sel_address", "Choose Address", list(), multiple = FALSE),
                       column(2, actionButton("btn_comp","Find Comps" ))
                )
                
              ),

              fluidRow(dataTableOutput("dt_comp_summary")),
              fluidRow(plotOutput("plt_comps")),
              fluidRow(dataTableOutput("dt_comps"))
              
  )
  return(page)
  
}


comp_serv = function(input, output, session){
  
  #address =  reactive({get_address(input$txt_address)})
  #addresses = reactive({list_addresses(address)})
  
  
  #output$dt_comps = renderDataTable({mtcars})
  #input$btn_search
  observeEvent(input$txt_address, {
    try({
      address =  get_address(input$txt_address)
    addresses = list_addresses(address)
    updateSelectInput(session, "sel_address", "Choose Address", choices=addresses )
    })
  })
  
  #btn_comp
  observeEvent(input$sel_address, {
    try({address =  get_address(input$sel_address)
    #print(address$results[[1]])
    
    lat = address$results[[1]]$geometry$location$lat
    lon = address$results[[1]]$geometry$location$lng
    print(lat)
    print(lon)
    w = where_comps(lat, lon, .5)
    print(w)
    df = sdat_query(where=w)
    head(df)
    df = filter(df, land_use =="Residential (R)" )
    df = filter(df, date2> "2018-01-01")
    df$price = as.numeric(df$price)
    df$living_area = as.numeric(df$living_area)
    df$year_built = as.numeric(df$year_built)
    sum_table = do.call(cbind, lapply(df[c("price", "living_area", "year_built")], summary))
    #output$plt_comps = renderPlot(ggplot(df, aes(living_area, price)+geom_point() ))
    output$dt_comp_summary = renderDataTable({as.data.frame(unclass(summary(df[c("price", "living_area")])))})
    output$plt_comps = renderPlot(plot(df$living_area, df$price))
    
    output$dt_comps = renderDataTable({df})
    })
    
  })
  
  
  
}




