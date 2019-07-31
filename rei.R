library("rvest")
library("tidyverse")
auction_hw = function(){
  url ='https://www.hwestauctions.com/assets/php4/tabbedWebsite.php'
  frame <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/table') %>%
    html_table()
  frame <- frame[[1]]
  names(frame) = frame[1,]
  frame = frame[2:nrow(frame),]
  frame = tidyr::separate(frame,`Address, City (First Pub)`, c("Address", "City_Pub"), sep=",", extra="drop", remove=FALSE)
  frame$Address = str_replace(frame$Address,"STREET", "ST")
  frame$Address = str_replace(frame$Address,"AVENUE", "AVE")
  frame$Address = str_replace(frame$Address,"DRIVE", "DR")
  frame$Address = str_replace(frame$Address,"LANE", "CT")
  
  
  
  return(frame)
}

profit = function(PurchasePrice, SalesPrice, Rehab, ProjectDays = 180, NetSale = 0.92, ClosingPercent = 0.06, InterestRate = 0.08, CashAtClosingPercent = 0.12){
  D = ProjectDays / 365
  Profit = NetSale * SalesPrice - PurchasePrice - Rehab - ClosingPercent * (PurchasePrice + Rehab) - InterestRate * D * (PurchasePrice + Rehab - (CashAtClosingPercent * (PurchasePrice + Rehab)))
return(Profit)
}


best_offer =function(SalesPrice, Rehab, ProjectDays = 180, Profit="none", CashOnCashReturn = 0.5, NetSale = 0.92, ClosingPercent = 0.06, InterestRate = 0.08, CashAtClosingPercent = 0.12){
  D = ProjectDays / 365
  if(Profit == "none"){
      result = NetSale * SalesPrice - Rehab * (1 + ClosingPercent + InterestRate * D * (1 - CashAtClosingPercent)) - CashOnCashReturn * (InterestRate * D * Rehab - InterestRate * D * CashAtClosingPercent * Rehab + CashAtClosingPercent * Rehab)
      denom = (1 + ClosingPercent + InterestRate * D - InterestRate * D * CashAtClosingPercent + CashOnCashReturn * (InterestRate * D - InterestRate * D * CashAtClosingPercent + CashAtClosingPercent))
      result = result / denom
  }else{
    result = NetSale * SalesPrice - Profit - Rehab * (1 + ClosingPercent + InterestRate * D * (1 - CashAtClosingPercent))
    result = result / (1 + ClosingPercent + InterestRate * D - InterestRate * D * CashAtClosingPercent)
  }
  return(result)

}



library(RSocrata)
api_endpoint = "https://opendata.maryland.gov/resource/ed4q-f8tm.json?"

url_md = "https://opendata.maryland.gov/"
id_md = "pwpp-tk9r"
app_id = "fuixaI8OmedLW38WcI00DnoCn"

select_statement = "$Select=
    mdp_street_address_mdp_field_address as address,
    subdivision_code_mdp_field_subdivsn_sdat_field_37 as neighborhood, 
    sales_segment_1_consideration_mdp_field_considr1_sdat_field_90 as price,
    sales_segment_2_consideration_sdat_field_110 as price2,
    sales_segment_1_transfer_date_yyyy_mm_dd_mdp_field_tradate_sdat_field_89 as date,
    sales_segment_2_transfer_date_yyyy_mm_dd_sdat_field_109 as date2,
    additional_c_a_m_a_data_building_style_code_and_description_mdp_field_strustyl_descstyl_sdat_field_1 as style_code,
    sales_segment_1_grantor_name_mdp_field_grntnam1_sdat_field_80 as seller,
    c_a_m_a_system_data_year_built_yyyy_mdp_field_yearblt_sdat_field_235 as year_built,
    c_a_m_a_system_data_dwelling_grade_code_and_description_mdp_field_strugrad_strudesc_sdat_field_230 as grade,
    current_assessment_year_total_assessment_sdat_field_172 as tax_assessment,
    sales_segment_3_how_conveyed_ind_sdat_field_127 as sales_type_orig,
    c_a_m_a_system_data_structure_area_sq_ft_mdp_field_sqftstrc_sdat_field_241 as living_area,
    land_use_code_mdp_field_lu_desclu_sdat_field_50 as land_use,
    additional_c_a_m_a_data_dwelling_construction_code_mdp_field_strucnst_sdat_field_263 as constr_code,
    additional_c_a_m_a_data_dwelling_type_mdp_field_strubldg_sdat_field_265 as bld_code,
    latitude_longitude as location,
    mdp_latitude_mdp_field_digycord_converted_to_wgs84 as lat,
    mdp_longitude_mdp_field_digxcord_converted_to_wgs84 as lon

"
