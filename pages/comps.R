library(shiny)
library(shinydashboard)
library(ggmap)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(DT)
library(leaflet)
source("sdat.R")
source("rei.R")


#models <- readRDS("../analysis/models_small.rds")

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
                column(6,
                  fluidRow(textInput("txt_address", "Search Address", "1303 Alberta Dr District Heights"), 
                           actionButton("btn_search", "search", icon=icon("search"))),
                  fluidRow(numericInput("num_dist", "Search Radius", 0.5)),
                  fluidRow(selectInput("sel_address", "Choose Address", list(), multiple = FALSE))
                    
                  )),
              fluidRow(dataTableOutput("dt_prop_summary")),
              fluidRow(dataTableOutput("dt_comp_summary")),
              fluidRow(plotOutput("plt_comps")),
              fluidRow(leafletOutput('map_props')),
              fluidRow(dataTableOutput("dt_comps"))
              
  )
  return(page)
  
}


comp_serv = function(input, output, session){

  #create reactive variable
  values <- reactiveValues(address_short_name = NULL, lat = NULL, lon = NULL)
  
  #create map
  output$map_props = renderLeaflet({
    map <- leaflet() %>% 
      addTiles() %>%
      setView(lat = 38.9784,lng=-76.4922, zoom=12)
  })
  
  #add property to map
  observe({
    try({
      df_prop <- data.frame(df_prop())
      if(!is.null(df_prop)){
        leafletProxy("map_props") %>%
          clearGroup("prop") %>%
          setView(lat = df_prop$lat,lng=df_prop$lon, zoom=16) %>%
          addAwesomeMarkers(group= "prop",
                            lat=as.numeric(df_prop$lat),
                            lng=as.numeric(df_prop$lon),
                            icon=awesomeIcons(markerColor = "green"),
                            popup=df_prop$address[[1]]
          ) 
      }
    })
  })
    
  #add renovated comps to map
  observe({
    try({
      comps <- data.frame(comps())
      comps <- filter(comps, renovated ==T)
      if(!is.null(comps)){
        
        leafletProxy("map_props") %>%
          clearGroup("comps") %>%
          addMarkers(group="comps",
                     lat=comps$lat %>% as.numeric(),
                     lng = comps$lon  %>% as.numeric(),
                     popup = paste(sep="<br/>",
                                   paste0("<b>",comps$address,"<b>"),
                                   paste("Price:",format(comps$price, big.mark = ",")),
                                   paste("Sqft:", format(comps$living_area, big.mark=",")))
          )
      }
    })        
  })

  #add non renovated comps to map
  observe({
    try({
      comps <- data.frame(comps())
      comps <- filter(comps, renovated ==F)
      if(!is.null(comps)){
        
        leafletProxy("map_props") %>%
          clearGroup("comps") %>%
          addMarkers(group="comps",
                     lat=comps$lat %>% as.numeric(),
                     lng = comps$lon  %>% as.numeric(), 
                     icon=awesomeIcons(markerColor = "red"),
                     popup = paste(sep="<br/>",
                                   paste0("<b>",comps$address,"<b>"),
                                   paste("Price:",format(comps$price, big.mark = ",")),
                                   paste("Sqft:", format(comps$living_area, big.mark=",")))
          )
      }
    })        
  })
  
    
  #plot comps
  output$plt_comps = renderPlot({
    show_grid <- T
    comps <- data.frame(comps())
    g <- ggplot(comps, aes(x=living_area, y=price / 1000, color = stories)) + 
      geom_point() +
      geom_smooth(method=lm) +
      geom_vline(xintercept = df_prop()$living_area[[1]], linetype="dashed", color="blue")+
      labs(title=input$sel_address,subtitle="Comparable Houses", x = "Sqft", y = "Price $K")
    
    if(show_grid){g<- g + facet_wrap(comps$renovated)}
    
    g
    # plot(df$living_area, df$price, col = df$bld_code)
    # legend("topright", legend = unique(df$bld_code),col=1:length(df$bld_code),pch=1)
    # abline(v=df_prop$living_area[[1]], col = "blue", lty=2)
    # abline(h=median(df$price, col = "blue", lty=2))
    # abline(lm(df$price ~ df$living_area), col = "black", lty=2)
    
  })
  
  
  #find new property
  observeEvent(input$btn_search, {
    try({
      
      google_address <- get_address(input$txt_address)
      addresses = list_addresses(google_address)
      
      #values$addresses <- google_address
      values$address_short_name <-  paste(google_address$results[[1]]$address_components[[1]]$long_name,
                                          google_address$results[[1]]$address_components[[2]]$long_name )
      print(values$address_short_name)
      values$lat <- google_address$results[[1]]$geometry$location$lat
      values$lon <- google_address$results[[1]]$geometry$location$lng
      
      
      updateSelectInput(session, "sel_address", "Choose Address", choices=addresses )
    })
  })
  
  
  #address <- reactive({get_address(input$sel_address)})
  df_prop <- reactive({
    print("df_prop reactive")
    print(values$address_short_name)
    df <- sdat_query(where = where_meta(values$address_short_name))
#    if(is.null(df) | nrow(df) ==0){
#      df <- sdat_query(where= where_comps(lat=values$lat, lon = values$lon, miles = .01,0))}
    df
  })
  
  output$dt_prop_summary = renderDataTable({
    df_prop <- data.frame(df_prop())
    datatable(df_prop, options=list(dom="t", scrollX=T))
    #datatable(df_prop %>% t(), options = list(dom="t",scrollY=T))
    #c("address","land_use", "stories","basement", "neighborhood", "living_area", "acres", "year_built", "tax_assessment", "Estimate")
  })
  
  
  comps <- reactive({
    print("comps reactive")
    reno = F
    df = sdat_query(where=where_comps(lat=df_prop()$lat[[1]], lon=df_prop()$lon[[1]], miles=input$num_dist))
    df = filter(df, land_use == df_prop()$land_use[[1]])
    if(reno){df <- filter(df, renovated == T)}
    df
  })
  
  output$dt_comps = renderDataTable({
    print("rendering comps")
    #print(comps()[1,1:10])
    datatable(comps(),options = list(scrollX =T))
  })
  

  output$dt_comp_summary = renderDataTable({
    datatable(
      comps() %>%
        filter(renovated ==T) %>%
        select(price, living_area) %>% 
        gather() %>% 
        group_by(key) %>% 
        summarise(
          min= min(value) %>% round() %>% format(big.mark=","),
          median= median(value) %>% round() %>% format(big.mark=","),
          mean= mean(value) %>% round() %>% format(big.mark=","),
          max = max(value) %>% round() %>% format(big.mark=",")
        ) %>%
        arrange(-desc(key)) %>%
        t()
      , options = list(dom="t"))
  })
  
}




