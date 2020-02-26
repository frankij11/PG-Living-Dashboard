library(ggplot2)
source('C:\\Users\\kevin\\OneDrive\\Documents\\Projects\\PG-Living-Dashboard\\sdat.R')
#df = sdat_get_meta(data.frame(address = "6202 Richmanor Terrace"))
df = sdat_get_meta(data.frame(address = "1006 Broderick drive"))
comps = sdat_query(where=where_comps(df$lat[[1]], df$lon[[1]], miles =1, year=1950))
comps2 = filter(comps, living_area >= df$living_area[[1]]-200, living_area <=df$living_area[[1]]+200)

prop_hist = comps2 %>% 
  mutate(year = substr(date, 1,4) %>% as.numeric()) %>%
  group_by(year) %>%
  summarise(median_price = median(price),
            mean_price=mean(price), 
            low_price=quantile(price,.1), 
            top_price = quantile(price, .90),
            sqft = mean(living_area),
            n = n()) %>%
  gather(key="type", value="price",-year, -sqft, -n ) %>%
  mutate(norm_price = price / (1.03)^(year-2020))

ggplot(prop_hist %>% filter(year>2012 ), aes(x=year, y=norm_price, color = type)) + geom_point() + geom_smooth()
ggplot(prop_hist %>% filter(year>2012 | year<2005) , aes(x=year, y=price, color = type)) + geom_point() #+ geom_smooth()

filter(prop_hist, year == 2019)