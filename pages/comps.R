library(shiny)
library(shinydashboard)
library(ggmap)
library(plotly)
#library(ggplot2)
#library(gridExtra)
#library(grid)
library(dplyr)
library(DT)
library(crosstalk)
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
  
  page =box(
    title = "Find Comparable Houses",
    status = "primary",
   width = 12,

        fluidRow(
          textInput("txt_address","Search Address","1303 Alberta Dr District Heights") %>% column(width = 3),
          numericInput("num_dist", "Search Radius", 0.5) %>% column(width =3),
          numericInput("num_year", "Search Year",2019, step=1, min=0, max=2020) %>% column(width =3)
        ),
        fluidRow(
          actionButton("btn_search", "search", icon = icon("search")) %>% column(width = 3),
          checkboxInput("chk_reno", "Show Renovations Only?", value =T) %>% column(width = 3)
          
        ),

        box(width=12, title="Predicted Price", collapsible = T, collapsed = F,
            
            fluidRow(
              textOutput("txt_address"),
              DTOutput("dt_predict", width = "50%")
            )
          
        ),
    fluidRow(
      dataTableOutput("dt_prop_summary") %>% column(width=12)
    ),
    fluidRow(
            leafletOutput('map_props') %>% column(width=6),
            plotlyOutput("plt_comps") %>% column(width=6)
            ),
    fluidRow(dataTableOutput("dt_comps"))
    
  )
  return(page)
  
}


comp_serv = function(input, output, session){
  

  
  #create reactive variable
  values <- reactiveValues(address_short_name = NULL, lat = NULL, lon = NULL, meta = NULL, all_comps = NULL)

  output$dt_predict <- renderDT({
    meta <-df_prop()
    comps <- comps()
    mods <- sdat_models(meta, comps)
    dt <- sdat_predict(mods, meta)
    dt$Estimate <- round(dt$Estimate / 1000)
    updateNumericInput(session,
                       inputId = "num_sales_price",
                       label = "ARV",
                       min = 1000,
                       max = 1000000,
                       value = dt$Estimate[2:nrow(dt)] %>% mean() * 1000 %>% round(digits = -3),
                       step = 5000
    ) 
    dt
  })  
    
  #create map
  output$map_props = renderLeaflet({
    map <- leaflet() %>% 
      addTiles() %>%
      setView(lat = 38.9784,lng=-76.4922, zoom=12)
  })
  
  #add renovated comps to map
  observe({
    try({
      comps <- data.frame(comps())
      comps_reno <- filter(comps, renovated == T)
      if(!is.null(comps)){
        
        leafletProxy("map_props") %>%
          clearGroup("comps_reno") %>%
          addAwesomeMarkers(group="comps_reno",
                     lat=comps_reno$lat %>% as.numeric(),
                     lng = comps_reno$lon  %>% as.numeric(),
                     icon=awesomeIcons(markerColor = "green"),
                     label = comps_reno$price / 1000 %>% round(0),
                     popup = paste(sep="<br/>",
                                   paste0("<b>",comps_reno$address,"<b>"),
                                   paste("Price:",format(comps_reno$price, big.mark = ",")),
                                   paste("Sqft:", format(comps_reno$living_area, big.mark=",")))
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
          clearGroup("comps_no_reno") %>%
        addAwesomeMarkers(group="comps_no_reno",
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
  
  #add property to map
  observe({
    try({
      df_prop <- data.frame(df_prop())
      if(!is.null(df_prop)){
        leafletProxy("map_props") %>%
          clearGroup("prop") %>%
          setView(lat = df_prop$lat,lng=df_prop$lon, zoom=14) %>%
          addAwesomeMarkers(group= "prop",
                            lat=as.numeric(df_prop$lat),
                            lng=as.numeric(df_prop$lon),
                            icon=awesomeIcons(markerColor = "black"),
                            popup=df_prop$address[[1]]
          ) 
      }
    })
  })
  
  
  #plot comps
  output$plt_comps = renderPlotly({
    #show_grid <- F
    
    comps <- data.frame(comps())
    if(nrow(comps)>0){
    #comps$ren_stories <- paste(comps$renovated, comps$stories)
    p <- plot_ly(data = filter(comps, renovated ==TRUE) 
                 ,x= ~living_area
                 ,y= ~price / 1000
                 ,text= ~paste(address, 
                               '<br>Sold Date:', ymd(date),
                               '<br>Year Built:', year_built)
                 ,type ='scatter'
                 ,mode='markers'
                 ,color = I('green')
                 ,name = "Renovated"
                 ) %>%
      add_trace(data=filter(comps, renovated ==FALSE)
                ,x= ~living_area
                ,y= ~price / 1000
                ,text= ~paste(address, 
                              '<br>Sold Date:', ymd(date),
                              '<br>Year Built:', year_built)
                ,color = I('red')
                ,name = "Not Renovated"
                ) %>%
      add_segments(x = df_prop()$living_area[[1]], xend = df_prop()$living_area[[1]], y = round(min(comps$price)/1000 - 100, -2) , yend = round(max(comps$price)/1000 +100, -2), color=I('black'), name = "Property Sqft")
      #geom_point() +
      #geom_smooth(method=lm) +
      #geom_vline(xintercept = df_prop()$living_area[[1]], linetype="dashed", color="blue")+
      #labs(title=input$sel_address,subtitle="Comparable Houses", x = "Sqft", y = "Price $K")
      #labs(title=values$full_address,subtitle="Comparable Houses", x = "Sqft", y = "Price $K")
    
    }
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
      values$meta <- sdat_query(where = where_meta(values$address_short_name))
      values$full_address <- google_address$results[[1]]$formatted_address
      values$all_comps <- sdat_query(where=where_comps(lat=df_prop()$lat[[1]], lon=df_prop()$lon[[1]], miles=input$num_dist, year=2014))
      #values$predict.lm <- model.lm(values$all_comps)
      #values$predict.rf <- model.rf(values$all_comps)
      
      #updateSelectInput(session, "sel_address", "Choose Address", choices=addresses )
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
    datatable(df_prop %>% select(address,city,year_built,stories,living_area,acre), options=list(dom="t", scrollX=T))
    #datatable(df_prop %>% t(), options = list(dom="t",scrollY=T))
    #c("address","land_use", "stories","basement", "neighborhood", "living_area", "acres", "year_built", "tax_assessment", "Estimate")
  })
  
  
  comps <- reactive({
    print("comps reactive")
    #reno = F
    df = sdat_query(where=where_comps(lat=df_prop()$lat[[1]], lon=df_prop()$lon[[1]], miles=input$num_dist))
    df = filter(df, land_use == df_prop()$land_use[[1]])
    if(input$chk_reno){df <- filter(df, renovated == T)}
    df
  })
  
  output$dt_comps = renderDataTable({
    print("rendering comps")
    #print(comps()[1,1:10])
    datatable(comps(),options = list(scrollX =T, scrollY=T))
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




