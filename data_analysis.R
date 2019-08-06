library(readxl)
library(dplyr)
#library(sclaes)
library(ggplot2)

rehab_link ="https://www.dropbox.com/s/awyvkupytv6acve/2014%20Property%20Cost%20Breakdown%20NEW.xlsm?dl=1"

# rehab_costs <- read_excel("rehab_costs.xlsx", 
#                           sheet = "Data", col_types = c("text", 
#                                                         "date", "text", "text", "numeric", 
#                                                         "numeric", "text", "text", "text", 
#                                                         "text", "numeric"))


#Download data and read into R

if(file.info("rehab.xlsx")$ctime <= Sys.Date()-1){download.file(rehab_link, "rehab.xlsx")}
rehab_costs = read_excel("rehab.xlsx", sheet= "Total Expense", col_types="text")

#Clean Data
my_cols <- c("Property", "Date", "Pay Type", "Description", "Labor", "Materials")
rehab_costs = rehab_costs[my_cols]
rehab_costs$Labor = as.numeric(rehab_costs$Labor)
rehab_costs$Materials = as.numeric(rehab_costs$Materials)
rehab_costs$Date = as.numeric(rehab_costs$Date)
rehab_costs$Date = as.Date(rehab_costs$Date, "1899-12-30")

rehab_costs$Labor[is.na(rehab_costs$Labor)]<-0
rehab_costs$Materials[is.na(rehab_costs$Materials)]<-0
rehab_costs$Total = rehab_costs$Labor +rehab_costs$Materials



rehab_costs$Type[rehab_costs$Labor>0]="Labor"
rehab_costs$Type[rehab_costs$Materials>0]="Material"


rehab_costs$Property = toupper(rehab_costs$Property)
rehab_costs$Year = format(rehab_costs$Date, "%Y")
rehab_costs$Month = format(rehab_costs$Date, "%m")


basic_sum = summarise(group_by(rehab_costs, Property),Labor = sum(Labor), Material = sum(Materials), Total = sum(Total),  Start_Date = min(Date), End_Date=max(Date) )
basic_sum$Duration = round(difftime(basic_sum$End_Date, basic_sum$Start_Date, units="weeks")/4,2)
#basic_sum$Total = scales::dollar(basic_sum$Total)

filt_sum <- filter(basic_sum, End_Date > "2010-01-01")
filt_sum <- filt_sum[order(filt_sum$End_Date,decreasing=TRUE),]
filt_sum$Labor = paste0("$", format(round(filt_sum$Labor), big.mark = ","))
filt_sum$Material = paste0("$", format(round(filt_sum$Material), big.mark = ","))
filt_sum$Total = paste0("$", format(round(filt_sum$Total), big.mark = ","))

filt_sum[order("End_Date")]

year_sum = summarise(group_by(rehab_costs, Year), Labor = sum(Labor), Material = sum(Materials), Total = sum(Total))
year_sum = filter(year_sum, Year > 2010)
#barplot(year_sum$Total, names.arg = year_sum$Year)


get_filt_sum = function(){
  return(filt_sum)
}

get_year_sum = function(){
  return(year_sum)
}

