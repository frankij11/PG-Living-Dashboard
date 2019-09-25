library(shiny)
library(shinydashboard)
library(ggmap)
library(ggplot2)
library(dplyr)
library(DT)
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
                column(6,textInput("txt_address", "Search Address", "1303 Alberta Dr"))
              ),
              fluidRow(
                column(6,numericInput("num_dist", "Search Radius", 0.5))
              ),
              fluidRow(
                column(6,selectInput("sel_address", "Choose Address", list(), multiple = FALSE)
                )
                
              ),

              fluidRow(dataTableOutput("dt_prop_summary")),
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
  observeEvent(c(input$sel_address, input$num_dist), {
    try({address =  get_address(input$sel_address)
    #print(address$results[[1]])
    
    lat = address$results[[1]]$geometry$location$lat
    lon = address$results[[1]]$geometry$location$lng
    print(lat)
    print(lon)
    w = where_comps(lat, lon, input$num_dist)
    print(w)
    df = sdat_query(where=w)
    head(df)
    df = filter(df, land_use =="Residential (R)" )
    df = filter(df, date2> "2018-01-01")
    df$price = as.numeric(df$price)
    df$living_area = as.numeric(df$living_area)
    df$year_built = as.numeric(df$year_built)
    df$bld_code = as.factor(df$bld_code)
    sum_table = do.call(cbind, lapply(df[c("price", "living_area", "year_built")], summary))
    #output$plt_comps = renderPlot(ggplot(df, aes(living_area, price)+geom_point() ))
    
    
    #prop_meta_where = where_comps(lat, lon, .001)
    #prop_meta_where = gsub("> 50000 | > 0", " >= 0 ", prop_meta_where)
    #df_prop <- sdat_query(where = prop_meta_where)
    address_short_name = paste(address$results[[1]]$address_components[[1]]$long_name,address$results[[1]]$address_components[[2]]$long_name )
    df_prop <- sdat_query(where = where_meta(c(address_short_name)))
    df_prop$price = as.numeric(df_prop$price)
    df_prop$living_area = as.numeric(df_prop$living_area)
    df_prop$year_built = as.numeric(df_prop$year_built)
    
    #Render Data
    output$dt_comp_summary = renderDataTable({as.data.frame(unclass(summary(df[c("price", "living_area")])))})
    output$plt_comps = renderPlot({
      
      plot(df$living_area, df$price, col = df$bld_code)
      legend("topright", legend = unique(df$bld_code),col=1:length(df$bld_code),pch=1)
      abline(v=df_prop$living_area[[1]], col = "blue", lty=2)
      abline(h=median(df$price, col = "blue", lty=2))
      abline(lm(df$price ~ df$living_area), col = "black", lty=2)
      
      })
    
    
    
    output$dt_comps = renderDataTable({
      
      datatable(df,options = list(scrollX =T))
      })
    output$dt_prop_summary = renderDataTable({
      datatable(df_prop, options = list(scrollX =T))
    })
        
    
    })
    
  })

}




