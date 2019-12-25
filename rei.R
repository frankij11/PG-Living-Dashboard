library("rvest")
library("tidyverse")
auction_hw = function(){
  url ='https://www.hwestauctions.com/assets/php4/tabbedWebsite.php'
  url2 = 'http://www.mwc-law.com/lists/MD.html'
  x_path<-'/html/body/div[2]/div[3]/table/tbody/tr/td[2]/div/table/tbody/tr/td[1]/table'
  #/html/body/div[2]/div[3]/table/tbody/tr/td[2]/div/table/tbody/tr/td[1]/table/tbody
  x_path2= '/html/body/div[2]/div[3]/table/tbody/tr/td[2]/div/table'
  
  frame <- url2 %>%
    read_html() %>%
    #html_nodes(xpath=x_path2) %>%
    html_table()
  frame <- frame[[1]]
  names(frame) = frame[2,]
  frame = frame[3:nrow(frame),]
  #frame = tidyr::separate(frame,`Address, City (First Pub)`, c("Address", "City_Pub"), sep=",", extra="drop", remove=FALSE)
  #frame = tidyr::separate(frame,`City_Pub`, c("City", "Date_Published"), sep="[(]", extra="drop", remove=TRUE)
  #frame$Date_Published = str_replace(frame$Date_Published, "[)]", "")
  #frame$Address = str_replace(frame$Address,"STREET", "ST")
  #frame$Address = str_replace(frame$Address,"AVENUE", "AVE")
  #frame$Address = str_replace(frame$Address,"DRIVE", "DR")
  #frame$Address = str_replace(frame$Address,"LANE", "CT")
  
  
  
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


