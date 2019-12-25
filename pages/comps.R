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
                  fluidRow(textInput("txt_address", "Search Address", "1303 Alberta Dr District Heights")),
                  fluidRow(numericInput("num_dist", "Search Radius", 0.5)),
                  fluidRow(selectInput("sel_address", "Choose Address", list(), multiple = FALSE))
                    
                  )),
              fluidRow(dataTableOutput("dt_prop_summary")),
              fluidRow(dataTableOutput("dt_comp_summary")),
              fluidRow(plotOutput("plt_comps")),
              fluidRow(leafletOutput('map_comps')),
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
    w = paste(w,
              "AND",
              "sales_segment_1_transfer_date_yyyy_mm_dd_mdp_field_tradate_sdat_field_89 > '2012%'",
              
              "LIMIT 50000")
    #print(w)
    df = sdat_query(where=w)
    head(df)
    df = filter(df, land_use =="Residential (R)" )
    df = filter(df, date2> "2018-01-01", price2 > 10000, living_area >0)
    df$price = as.numeric(df$price)
    df$living_area = as.numeric(df$living_area)
    df$year_built = as.numeric(df$year_built)
    df$bld_code = as.factor(df$bld_code)
    sum_table = do.call(cbind, lapply(df[c("price", "living_area", "year_built")], summary))
    #output$plt_comps = renderPlot(ggplot(df, aes(living_area, price)+geom_point() ))
    mod <- lm(price~living_area + bld_code, data =df)
    df$Estimate <- predict(mod, df)
    #prop_meta_where = where_comps(lat, lon, .001)
    #prop_meta_where = gsub("> 50000 | > 0", " >= 0 ", prop_meta_where)
    #df_prop <- sdat_query(where = prop_meta_where)
    address_short_name = paste(address$results[[1]]$address_components[[1]]$long_name,address$results[[1]]$address_components[[2]]$long_name )
    df_prop <- sdat_query(where = where_meta(c(address_short_name)))
    df_prop$price = as.numeric(df_prop$price)
    df_prop$living_area = as.numeric(df_prop$living_area)
    df_prop$year_built = as.numeric(df_prop$year_built)
    df_prop$Estimate <- predict(mod, df_prop)
#add all models
    #   df_prop <- cbind(df_prop, data.frame(predict(models, df_prop)))
    print(predict(mod,df_prop))
    #Render Data
    output$dt_comp_summary = renderDataTable({
      datatable(
        df %>% 
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
    
    
    output$plt_comps = renderPlot({
      show_grid <- F
      g <- ggplot(df, aes(x=living_area, y=price / 1000, color = bld_code)) + 
        geom_point() +
        geom_smooth(method=lm) +
        geom_vline(xintercept = df_prop$living_area[[1]], linetype="dashed", color="blue")+
        labs(title=input$sel_address,subtitle="Comparable Houses", x = "Sqft", y = "Price $K")
     
     if(show_grid){g<- g + facet_grid(df$bld_code)}

      g
      # plot(df$living_area, df$price, col = df$bld_code)
      # legend("topright", legend = unique(df$bld_code),col=1:length(df$bld_code),pch=1)
      # abline(v=df_prop$living_area[[1]], col = "blue", lty=2)
      # abline(h=median(df$price, col = "blue", lty=2))
      # abline(lm(df$price ~ df$living_area), col = "black", lty=2)
      
      })
    
    output$map_comps = renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addAwesomeMarkers(lat=df_prop$lat,
                   lng=df_prop$lon,
                   icon=awesomeIcons(markerColor = "green"),
                   popup=df_prop$address[[1]]
                   ) %>%
        addMarkers(lat=df$lat,
                   lng = df$lon, 
                   popup = paste(sep="<br/>",
                                 paste0("<b>",df$address,"<b>"),
                                 paste("Price:",format(df$price, big.mark = ",")),
                                 paste("Sqft:", format(df$living_area, big.mark=",")))
                  )
    })
    
    output$dt_comps = renderDataTable({
      
      datatable(df,options = list(scrollX =T))
      })
    output$dt_prop_summary = renderDataTable({
      datatable(df_prop[, c("address","land_use", "bld_code", "neighborhood", "living_area", "year_built", "tax_assessment", "Estimate") ]%>% t(), options = list(dom="t",scrollX=T))
    })
        
    
    })
    
  })

}




