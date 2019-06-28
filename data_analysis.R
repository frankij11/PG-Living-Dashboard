library(readxl)
library(dplyr)
library(sclaes)
library(ggplot2)


rehab_costs <- read_excel("Desktop/PG LIVING DASHBOARD/PG_LIVING_DASHBOARD/rehab_costs.xlsx", 
                                 sheet = "Data", col_types = c("text", 
                                                                         "date", "text", "text", "numeric", 
                                                                         "numeric", "text", "text", "numeric", 
                                                                         "text", "numeric"))

rehab_costs$Property = toupper(rehab_costs$Property)
rehab_costs$Year = format(rehab_costs$Date, "%Y")
rehab_costs$Month = format(rehab_costs$Date, "%m")


basic_sum = summarise(group_by(rehab_costs, Property),Total = sum(Amount),  Start_Date = min(Date), End_Date=max(Date) )
basic_sum$Duration = round(difftime(basic_sum$End_Date, basic_sum$Start_Date, units="weeks")/4,2)
basic_sum$Total = scales::dollar(basic_sum$Total)

filt_sum <- filter(basic_sum, End_Date > "2018-01-01")
filt_sum <- filt_sum[order(filt_sum$End_Date,decreasing=TRUE),]
View(filt_sum)

filt_sum[order("End_Date")]

year_sum = summarise(group_by(rehab_costs, Year), Total = sum(Amount)/1000)
year_sum = filter(year_sum, Year > 2010)
barplot(year_sum$Total, names.arg = year_sum$Year)

View(year_sum)