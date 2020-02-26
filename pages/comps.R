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
library(geosphere)
source("sdat.R")
source("rei.R")

#models <- readRDS("../analysis/models_small.rds")

API_KEY = 'AIzaSyBBTXVoEURxl8IGpTF0vfOp96LJgpKdSZo'
register_google(API_KEY)



get_address = function(address){
  #api = "https://www.mapquestapi.com/geocoding/v1/address?key=6yJFVCl3qGnH50FqzSjI6g70DGuCdbE2&inFormat=kvp&outFormat=csv&location="
  api="http://dev.virtualearth.net/REST/v1/Locations?key=LWs50LxKdRrlIrSX8aOy~Rd2e8OyxLUDoRlhrE8rTzw~AkN9Tth4vAlfye46YPPx9rMDxqDOitzuqOidLyDiUz14cfShrn8D1_fIklsw-FRv&query="
  location=address
  full_url = paste0(api, location, "+MD+USA")
  full_url = URLencode(full_url)
  page = GET(full_url)
  results = content(page)["resourceSets"][["resourceSets"]][[1]]
  frame = data.frame(address_short_name = content(page)["resourceSets"][["resourceSets"]][[1]][["resources"]][[1]][["address"]][["addressLine"]],
  address_long_name = content(page)["resourceSets"][["resourceSets"]][[1]][["resources"]][[1]][["address"]][["formattedAddress"]],
  lat = content(page)["resourceSets"][["resourceSets"]][[1]][["resources"]][[1]][["point"]][["coordinates"]][[1]],
  lon = content(page)["resourceSets"][["resourceSets"]][[1]][["resources"]][[1]][["point"]][["coordinates"]][[2]] )
    
  return(frame)
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
          textInput("txt_address","Search Address","1303 Alberta Dr") %>% column(width = 3),
          numericInput("num_dist", "Search Radius", 0.5) %>% column(width =3),
          numericInput("num_year", "Search Year",2019, step=1, min=0, max=2020) %>% column(width =3)
        ),
        fluidRow(
          actionButton("btn_search", "search", icon = icon("search")) %>% column(width = 3),
          checkboxInput("chk_reno", "Show Flipped Houses Only?", value =F) %>% column(width = 3)
          
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
  values <- reactiveValues(address_short_name = NULL, lat = NULL, lon = NULL, prop_meta = sdat_query(where=where_meta("1303 Alberta Dr")), all_comps = NULL)

  output$dt_predict <- renderDT({
    meta <-df_prop()
    comps <- comps()
    mods <- sdat_models(meta, comps)
    dt <- sdat_predict(mods, meta)

    dt$Estimate <- round(dt$Estimate / 1000)
    try({
    updateNumericInput(session,
                       inputId = "num_sales_price",
                       label = "ARV",
                       min = 1000,
                       max = 1000000,
                       value = dt$Estimate[2:nrow(dt)] %>% mean() * 1000 %>% round(digits = -3),
                       step = 5000
    ) 
    })
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
                     label = paste(comps_reno$price / 1000 %>% round(0)," , ", comps_reno$living_area) ,
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
                     label = paste(comps$price / 1000 %>% round(0)," , ", comps$living_area) ,
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
                            label = paste(df_prop$price / 1000 %>% round(0)," , ", df_prop$living_area) ,
                     popup = paste(sep="<br/>",
                                   paste0("<b>",df_prop$address,"<b>"),
                                   paste("Price:",format(df_prop$price, big.mark = ",")),
                                   paste("Sqft:", format(df_prop$living_area, big.mark=",")))

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
    p <- plot_ly(data = filter(comps, renovated ==TRUE, basement==T) 
                 ,x= ~living_area
                 ,y= ~price / 1000
                 ,text= ~paste(address, 
                               '<br>Sold Date:', ymd(date),
                               '<br>Year Built:', year_built)
                 ,type ='scatter'
                 ,mode='markers'
                 ,color = I('green')
                 ,name = "Flipped"
                 ) %>%
      add_trace(data=filter(comps, renovated ==FALSE, basement==T)
                ,x= ~living_area
                ,y= ~price / 1000
                ,text= ~paste(address, 
                              '<br>Sold Date:', ymd(date),
                              '<br>Year Built:', year_built)
                ,color = I('red')
                ,name = "Not Flipped"
                ) %>%
      add_trace(data=filter(comps, basement==F)
          ,x= ~living_area
          ,y= ~price / 1000
          ,text= ~paste(address, 
                        '<br>Sold Date:', ymd(date),
                        '<br>Year Built:', year_built)
          ,color = I('black')
          ,name = "No Basement"
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
      df <- sdat_query(where = where_meta(input$txt_address))
      if (nrow(df)<1){
        showNotification("Trying to find address on Bing", type="message")
        search_address <- get_address(input$txt_address)
        values$address_short_name <-  search_address$address_short_name[[1]]
        values$lat <- search_address$lat[[1]]
        values$lon <- search_address$lng[[1]]

        df <- sdat_query(where = where_meta(search_address$address_short_name[[1]]))
        }
      if(is.null(df) | nrow(df) ==0){
        showNotification("Could not find match. Try another address", type="error")
        df <- sdat_query(where= where_comps(lat=values$lat, lon = values$lon, miles = .01,0))
        }
      
      values$prop_meta <- df

#      values$full_address <- google_address$formatted_address_long_name[[1]]
      #values$all_comps <- sdat_query(where=where_comps(lat=df_prop()$lat[[1]], lon=df_prop()$lon[[1]], miles=input$num_dist, year=2014))
      
      #updateSelectInput(session, "sel_address", "Choose Address", choices=addresses )
    })
  })
  
  
  #address <- reactive({get_address(input$sel_address)})
  df_prop <- reactive({
    print("df_prop reactive")
    #print(values$address_short_name)
#    df <- sdat_query(where = where_meta(input$txt_address))
#    if (nrow(df)<1){
#      search_address <- get_address(input$txt_address)
#      df <- sdat_query(where = where_meta(search_address$address_short_name[[1]]))}
#    if(is.null(df) | nrow(df) ==0){
#      df <- sdat_query(where= where_comps(lat=values$lat, lon = values$lon, miles = .01,0))}
#    df

    values$prop_meta
  })
  
  output$dt_prop_summary = renderDataTable({
    df_prop <- data.frame(df_prop())
    datatable(df_prop %>% select(address,city,tax_assessment,owner_type,year_built,stories,basement,living_area,acre, price,date), options=list(dom="t", scrollX=T))
    #datatable(df_prop %>% t(), options = list(dom="t",scrollY=T))
    #c("address","land_use", "stories","basement", "neighborhood", "living_area", "acres", "year_built", "tax_assessment", "Estimate")
  })
  
  
  comps <- reactive({
    print("comps reactive")
    #reno = F
    df = sdat_query(where=where_comps(lat=df_prop()$lat[[1]], lon=df_prop()$lon[[1]], miles=input$num_dist))
    df = filter(df, land_use == df_prop()$land_use[[1]])
    df$distance <- sapply(1:nrow(df), function(i){
      dist= distm(c(df$lon[[i]],df$lat[[i]]), c(df_prop()$lon[[1]], df_prop()$lat[[1]]), fun = distHaversine) * 0.621371 / 1000
      dist = round(dist, 2)
    })
    if(input$chk_reno){df <- filter(df, renovated == T)}
    
    df
  })
  
  output$dt_comps = renderDataTable({
    print("rendering comps")
    #print(comps()[1,1:10])
    datatable(
      
      filter(comps(), 
              basement==df_prop()$basement[[1]],
              living_area >= df_prop()$living_area[[1]] - 200,
              living_area <= df_prop()$living_area[[1]] + 200
      ) %>%
      #mutate(delta_sqft = living_area - df_prop()$living_area[[1]] ,
      #       delta_year_built = as.numeric(year_built) - as.numeric(df_prop()$year_built[[1]]),
      #       delta_acre = acre - df_prop()$acre[[1]],
      #       delta_tax = tax_assessment - df_prop()$tax_assessment[[1]]
      #) %>%
      arrange(desc(price)) %>%
      select(address,price, distance, year_built, living_area, basement,stories, acre )
      
      ,options = list(scrollX =T, scrollY=T))
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




