library(httr)
#library(RSocrata)
library(tidyverse)
library(lubridate)
#slibrary(stringr)
library(caret)

api_endpoint = "https://opendata.maryland.gov/resource/ed4q-f8tm.json?"
api_csv = "https://opendata.maryland.gov/resource/ed4q-f8tm.csv?"

url_md = "https://opendata.maryland.gov/"
app_id = "fuixaI8OmedLW38WcI00DnoCn"

select_statement = "SELECT 
    mdp_street_address_mdp_field_address as address,
    mdp_street_address_zip_code_mdp_field_zipcode as zipcode,
    mdp_street_address_city_mdp_field_city as city,
    subdivision_code_mdp_field_subdivsn_sdat_field_37 as neighborhood,
    record_key_owner_occupancy_code_mdp_field_ooi_sdat_field_6 as owner_type,
    sales_segment_1_consideration_mdp_field_considr1_sdat_field_90 as price,
    sales_segment_2_consideration_sdat_field_110 as price2,
    sales_segment_1_transfer_date_yyyy_mm_dd_mdp_field_tradate_sdat_field_89 as date,
    sales_segment_2_transfer_date_yyyy_mm_dd_sdat_field_109 as date2,

    sales_segment_1_grantor_name_mdp_field_grntnam1_sdat_field_80 as seller,
    c_a_m_a_system_data_year_built_yyyy_mdp_field_yearblt_sdat_field_235 as year_built,
    c_a_m_a_system_data_dwelling_grade_code_and_description_mdp_field_strugrad_strudesc_sdat_field_230 as grade,
    current_assessment_year_total_assessment_sdat_field_172 as tax_assessment,
    sales_segment_3_how_conveyed_ind_sdat_field_127 as sales_type_orig,
    c_a_m_a_system_data_structure_area_sq_ft_mdp_field_sqftstrc_sdat_field_241 as living_area,
    c_a_m_a_system_data_land_area_mdp_field_landarea_sdat_field_242 as land_area,
    c_a_m_a_system_data_land_unit_of_measure_mdp_field_luom_sdat_field_243 as land_area_measure,
    land_use_code_mdp_field_lu_desclu_sdat_field_50 as land_use,
    additional_c_a_m_a_data_dwelling_construction_code_mdp_field_strucnst_sdat_field_263 as constr_code,
    additional_c_a_m_a_data_building_style_code_and_description_mdp_field_strustyl_descstyl_sdat_field_264 as bld_code_detail,
    additional_c_a_m_a_data_dwelling_type_mdp_field_strubldg_sdat_field_265 as bld_code,
    latitude_longitude as location,
    mdp_latitude_mdp_field_digycord_converted_to_wgs84 as lat,
    mdp_longitude_mdp_field_digxcord_converted_to_wgs84 as lon
"

# additional_c_a_m_a_data_building_style_code_and_description_mdp_field_strustyl_descstyl_sdat_field_1 as style_code,

select_statement = gsub("[\r\n]", "",select_statement)
select_statement = gsub("  ", "",select_statement)


where_comps = function(lat, lon, miles = 1, year = 2019){ 
  miles = miles/0.000621371 #convert to meters
  w = sprintf(" WHERE 
        
        sales_segment_1_transfer_date_yyyy_mm_dd_mdp_field_tradate_sdat_field_89 >= %s  AND 
        sales_segment_1_consideration_mdp_field_considr1_sdat_field_90 > 10000 AND 
        within_circle(latitude_longitude, %f, %f, %f)", paste0("'", year, "%'"),lat, lon, miles)
  w = gsub("[\r\n]", "",w)
  w = gsub("  ", "",w)
  return(w)
  # (sales_segment_1_consideration_mdp_field_considr1_sdat_field_90 - sales_segment_2_consideration_sdat_field_110) > 50000 AND 
  # sales_segment_2_consideration_sdat_field_110 > 0 AND 

}

clean_addresses <- function(addresses){
  props_str = toupper(addresses)
  props_str = str_replace_all(props_str,"STREET", "ST")
  props_str = str_replace_all(props_str,"AVENUE", "AVE")
  props_str = str_replace_all(props_str,"DRIVE", "DR")
  props_str = str_replace_all(props_str,"LN", "LANE")
  props_str = str_replace_all(props_str,"TERRACE", "TER")
  props_str = str_replace_all(props_str,"COURT", "CT")
  props_str = str_replace_all(props_str," RD", " ROAD")
  props_str = str_replace_all(props_str,"TRAIL", "TRL")
  props_str = str_replace_all(props_str,"PLACE", "PL")
  props_str = str_replace_all(props_str,"BOULEVARD", "BLVD")
  
  return(props_str)
}

where_meta = function(props){
  props = paste0("'", sapply(props, trimws), "'")
  props_str = paste0(props, collapse = ",")
  props_str = clean_addresses(props_str)
  
  
  
  
  w = sprintf(" WHERE mdp_street_address_mdp_field_address in (%s) ", props_str)
  w = gsub("[\r\n]", "",w)
  w = gsub("  ", "",w)
  return(w)
}

sdat_query = function(api=api_csv, select=select_statement, where,limit = 50000,return_page=0){
  query = paste0("$query= ", select, where, " LIMIT ", limit)
  full_url = paste0(api, query)
  full_url = URLencode(full_url)
  page = GET(full_url)
  if(return_page>0){return(page)}
  if(status_code(page)==200){
    frame = suppressMessages(content(page))
    
    #clean data
    try({
      frame$neighborhood <- frame$neighborhood %>% as.factor()
      frame <- filter(frame, living_area >500)
      frame$price_delta <- (frame$price - frame$price2)
      frame$date_delta <- (ymd(frame$date) - ymd(frame$date2))/ 365
      frame$renovated <- frame$price_delta > 50000 & frame$date_delta <=1 & frame$price2 >10000
      frame$basement <- grepl("with basement", tolower(frame$bld_code_detail))
      frame$stories <- sub(".*STRY *(.*?) *Story.*", "\\1", frame$bld_code_detail)
      frame$acre <- ""
      frame$acre[frame$land_area_measure =="S"] <- frame$land_area[frame$land_area_measure =="S"] / 43560
      frame$acre[frame$land_area_measure =="A"] <- frame$land_area[frame$land_area_measure =="A"]
      
    })
    return(frame)
  }else{
    return(NULL)
  }
  
}

sdat_q=function(api=api_csv, q){
  query= paste0("$q=", q)
  full_url = paste0(api, query)
  full_url = URLencode(full_url)
  page = GET(full_url)
  if(status_code(page)==200){
    frame = content(page)
    return(frame)
  }else{
    return(NULL)
  }
  
}
sdat_get_meta <- function(df, col = "address"){
  results <- NULL
  for(i in 1:nrow(df)){
    tmp_result <- NULL
    tmp_result <- sdat_query(where=where_meta(df[,col][i]))
    if(!is.null(tmp_result)){
      #tmp_result$search_address <- df[,col][i] 
      if(is.null(results)){
        results <- tmp_result
      }else{
        results <- rbind(results, tmp_result)
    }
  }
  }
  df$search_address <- clean_addresses(df[,col])
  df <- left_join(df, results, by=c("search_address" = "address"))
  return(df)
}

sdat_get_many_properties = function(addresses){
  n = length(addresses)
  result = NULL
  beg = 0
  end = 0
  while(end<n){
    beg= beg+1
    end = min(beg +9 , n)
    print(paste("beginning", beg))
    print(paste("END", end))
    tmp_result = sdat_query(where=where_meta(addresses[beg:end]))
    if(is.null(result)){
      result = tmp_result
    }else{
      result = rbind(result, tmp_result)
    }
    tmp_result = NULL   
    beg = end
  }
  return(result)
}

model.lm <- function(df, reno = T, dist = 1){
  if(reno){df <- filter(df, renovated==T)}
  
  lm(price~living_area + stories + basement , data = df)
}

model.rf <- function(df){
  #control <- trainControl(method='boot', 
  #                        number=10, 
  #                       repeats=3)
  train(price~living_area + tax_assessment + owner_type + stories + renovated + year_built, data =df, method="rf")
}

#   if(values()$all_comps %>% !is.null()){



sdat_models <- function(meta = NULL, comps = NULL, address = NULL){

if(is.null(meta)){
  meta = sdat_query(where=where_meta(address))
  if(nrow(meta<1)){return(NULL)}
}
comps <- filter(comps, land_use =="Residential (R)" | land_use =="Town House (TH)" , price <1500000 )
  
comps_reno <- filter(comps, renovated ==T)
mods <- list()
try({
mods$avg <- lm(price~basement, data = filter(comps, living_area > meta$living_area[[1]] -200,living_area > meta$living_area[[1]] +200) )
mods$all <- lm(price~living_area:basement:renovated, data=comps)
#mods$slr <- lm(price~living_area, data = comps_reno)
mods$lm <- lm(price~living_area:basement, data = comps_reno)
mods$lm2 <- lm(price~living_area:basement:stories, data = comps_reno)
mods$lm3 <- lm(price~living_area:basement:stories:land_use:renovated, data = comps)
mods$lm4 <- lm(price~living_area:basement:stories:neighborhood:land_use:renovated, data = comps)
#mods$rf <- train(price~living_area + basement + stories + renovated +land_use + tax_assessment, 
#                 data = comps,
#                 method="rf",
#                 ntree=50, 
#                 control = trainControl(number=1))

})
meta$renovated == T
#mods$predict <- predict(mods, meta) %>% data.frame()
return(mods)
}

sdat_predict <- function(models, meta, reno=T){
  meta$renovated <- reno
  df <- data.frame(Model = sapply(1:length(models), function(i) models[i] %>% names() %>% toupper())
                   #,Equation = sapply(1:length(models), function(i) models[[i]]$call)
                   )
  df$Estimate <- sapply(1:length(models), 
                        function(i) {
                          r <- NULL
                          try({r <- predict(models[[i]], meta)[[1]]})
                          return(r)
                        })
  
                  
  return(df)
}

library(httr)
#library(RSocrata)
library(tidyverse)
library(lubridate)
#slibrary(stringr)
library(caret)

api_endpoint = "https://opendata.maryland.gov/resource/ed4q-f8tm.json?"
api_csv = "https://opendata.maryland.gov/resource/ed4q-f8tm.csv?"

url_md = "https://opendata.maryland.gov/"
app_id = "fuixaI8OmedLW38WcI00DnoCn"

select_statement = "SELECT 
    mdp_street_address_mdp_field_address as address,
    mdp_street_address_zip_code_mdp_field_zipcode as zipcode,
    mdp_street_address_city_mdp_field_city as city,
    subdivision_code_mdp_field_subdivsn_sdat_field_37 as neighborhood,
    record_key_owner_occupancy_code_mdp_field_ooi_sdat_field_6 as owner_type,
    sales_segment_1_consideration_mdp_field_considr1_sdat_field_90 as price,
    sales_segment_2_consideration_sdat_field_110 as price2,
    sales_segment_1_transfer_date_yyyy_mm_dd_mdp_field_tradate_sdat_field_89 as date,
    sales_segment_2_transfer_date_yyyy_mm_dd_sdat_field_109 as date2,

    sales_segment_1_grantor_name_mdp_field_grntnam1_sdat_field_80 as seller,
    c_a_m_a_system_data_year_built_yyyy_mdp_field_yearblt_sdat_field_235 as year_built,
    c_a_m_a_system_data_dwelling_grade_code_and_description_mdp_field_strugrad_strudesc_sdat_field_230 as grade,
    current_assessment_year_total_assessment_sdat_field_172 as tax_assessment,
    sales_segment_3_how_conveyed_ind_sdat_field_127 as sales_type_orig,
    c_a_m_a_system_data_structure_area_sq_ft_mdp_field_sqftstrc_sdat_field_241 as living_area,
    c_a_m_a_system_data_land_area_mdp_field_landarea_sdat_field_242 as land_area,
    c_a_m_a_system_data_land_unit_of_measure_mdp_field_luom_sdat_field_243 as land_area_measure,
    land_use_code_mdp_field_lu_desclu_sdat_field_50 as land_use,
    additional_c_a_m_a_data_dwelling_construction_code_mdp_field_strucnst_sdat_field_263 as constr_code,
    additional_c_a_m_a_data_building_style_code_and_description_mdp_field_strustyl_descstyl_sdat_field_264 as bld_code_detail,
    additional_c_a_m_a_data_dwelling_type_mdp_field_strubldg_sdat_field_265 as bld_code,
    latitude_longitude as location,
    mdp_latitude_mdp_field_digycord_converted_to_wgs84 as lat,
    mdp_longitude_mdp_field_digxcord_converted_to_wgs84 as lon
"

# additional_c_a_m_a_data_building_style_code_and_description_mdp_field_strustyl_descstyl_sdat_field_1 as style_code,

select_statement = gsub("[\r\n]", "",select_statement)
select_statement = gsub("  ", "",select_statement)


where_comps = function(lat, lon, miles = 1, year = 2019){ 
  miles = miles/0.000621371 #convert to meters
  w = sprintf(" WHERE 
        
        sales_segment_1_transfer_date_yyyy_mm_dd_mdp_field_tradate_sdat_field_89 >= %s  AND 
        sales_segment_1_consideration_mdp_field_considr1_sdat_field_90 > 10000 AND 
        within_circle(latitude_longitude, %f, %f, %f)", paste0("'", year, "%'"),lat, lon, miles)
  w = gsub("[\r\n]", "",w)
  w = gsub("  ", "",w)
  return(w)
  # (sales_segment_1_consideration_mdp_field_considr1_sdat_field_90 - sales_segment_2_consideration_sdat_field_110) > 50000 AND 
  # sales_segment_2_consideration_sdat_field_110 > 0 AND 

}

clean_addresses <- function(addresses){
  props_str = toupper(addresses)
  props_str = str_replace_all(props_str,"STREET", "ST")
  props_str = str_replace_all(props_str,"AVENUE", "AVE")
  props_str = str_replace_all(props_str,"DRIVE", "DR")
  props_str = str_replace_all(props_str,"LN", "LANE")
  props_str = str_replace_all(props_str,"TERRACE", "TER")
  props_str = str_replace_all(props_str,"COURT", "CT")
  props_str = str_replace_all(props_str," RD", " ROAD")
  props_str = str_replace_all(props_str,"TRAIL", "TRL")
  props_str = str_replace_all(props_str,"PLACE", "PL")
  props_str = str_replace_all(props_str,"BOULEVARD", "BLVD")
  
  return(props_str)
}

where_meta = function(props){
  props = paste0("'", sapply(props, trimws), "'")
  props_str = paste0(props, collapse = ",")
  props_str = clean_addresses(props_str)
  
  
  
  
  w = sprintf(" WHERE mdp_street_address_mdp_field_address in (%s) ", props_str)
  w = gsub("[\r\n]", "",w)
  w = gsub("  ", "",w)
  return(w)
}

sdat_query = function(api=api_csv, select=select_statement, where,limit = 50000,return_page=0){
  query = paste0("$query= ", select, where, " LIMIT ", limit)
  full_url = paste0(api, query)
  full_url = URLencode(full_url)
  page = GET(full_url)
  if(return_page>0){return(page)}
  if(status_code(page)==200){
    frame = suppressMessages(content(page))
    
    #clean data
    try({
      frame$neighborhood <- frame$neighborhood %>% as.factor()
      frame <- filter(frame, living_area >500)
      frame$price_delta <- (frame$price - frame$price2)
      frame$date_delta <- (ymd(frame$date) - ymd(frame$date2))/ 365
      frame$renovated <- frame$price_delta > 50000 & frame$date_delta <=1 & frame$price2 >10000
      frame$basement <- grepl("with basement", tolower(frame$bld_code_detail))
      frame$stories <- sub(".*STRY *(.*?) *Story.*", "\\1", frame$bld_code_detail)
      frame$acre <- ""
      frame$acre[frame$land_area_measure =="S"] <- frame$land_area[frame$land_area_measure =="S"] / 43560
      frame$acre[frame$land_area_measure =="A"] <- frame$land_area[frame$land_area_measure =="A"]
      
    })
    return(frame)
  }else{
    return(NULL)
  }
  
}

sdat_q=function(api=api_csv, q){
  query= paste0("$q=", q)
  full_url = paste0(api, query)
  full_url = URLencode(full_url)
  page = GET(full_url)
  if(status_code(page)==200){
    frame = content(page)
    return(frame)
  }else{
    return(NULL)
  }
  
}
sdat_get_meta <- function(df, col = "address"){
  results <- NULL
  for(i in 1:nrow(df)){
    tmp_result <- NULL
    tmp_result <- sdat_query(where=where_meta(df[,col][i]))
    if(!is.null(tmp_result)){
      #tmp_result$search_address <- df[,col][i] 
      if(is.null(results)){
        results <- tmp_result
      }else{
        results <- rbind(results, tmp_result)
    }
  }
  }
  df$search_address <- clean_addresses(df[,col])
  df <- left_join(df, results, by=c("search_address" = "address"))
  return(df)
}

sdat_get_many_properties = function(addresses){
  n = length(addresses)
  result = NULL
  beg = 0
  end = 0
  while(end<n){
    beg= beg+1
    end = min(beg +9 , n)
    print(paste("beginning", beg))
    print(paste("END", end))
    tmp_result = sdat_query(where=where_meta(addresses[beg:end]))
    if(is.null(result)){
      result = tmp_result
    }else{
      result = rbind(result, tmp_result)
    }
    tmp_result = NULL   
    beg = end
  }
  return(result)
}

model.lm <- function(df, reno = T, dist = 1){
  if(reno){df <- filter(df, renovated==T)}
  
  lm(price~living_area + stories + basement , data = df)
}

model.rf <- function(df){
  #control <- trainControl(method='boot', 
  #                        number=10, 
  #                       repeats=3)
  train(price~living_area + tax_assessment + owner_type + stories + renovated + year_built, data =df, method="rf")
}

#   if(values()$all_comps %>% !is.null()){



sdat_models <- function(meta = NULL, comps = NULL, address = NULL){

if(is.null(meta)){
  meta = sdat_query(where=where_meta(address))
  if(nrow(meta<1)){return(NULL)}
}
comps <- filter(comps, land_use =="Residential (R)" | land_use =="Town House (TH)" , price <1500000 )
  
comps_reno <- filter(comps, renovated ==T)
mods <- list()
try({
mods$avg <- lm(price~basement, data = filter(comps, living_area > meta$living_area[[1]] -200,living_area > meta$living_area[[1]] +200) )
mods$all <- lm(price~living_area:basement:renovated, data=comps)
#mods$slr <- lm(price~living_area, data = comps_reno)
mods$lm <- lm(price~living_area:basement, data = comps_reno)
mods$lm2 <- lm(price~living_area:basement:stories, data = comps_reno)
mods$lm3 <- lm(price~living_area:basement:stories:land_use:renovated, data = comps)
mods$lm4 <- lm(price~living_area:basement:stories:neighborhood:land_use:renovated, data = comps)
#mods$rf <- train(price~living_area + basement + stories + renovated +land_use + tax_assessment, 
#                 data = comps,
#                 method="rf",
#                 ntree=50, 
#                 control = trainControl(number=1))

})
meta$renovated == T
#mods$predict <- predict(mods, meta) %>% data.frame()
return(mods)
}

sdat_predict <- function(models, meta, reno=T){
  meta$renovated <- reno
  df <- data.frame(Model = sapply(1:length(models), function(i) models[i] %>% names() %>% toupper())
                   #,Equation = sapply(1:length(models), function(i) models[[i]]$call)
                   )
  df$Estimate <- sapply(1:length(models), 
                        function(i) {
                          r <- NULL
                          try({r <- predict(models[[i]], meta)[[1]]})
                          return(r)
                        })
  
                  
  return(df)
}

