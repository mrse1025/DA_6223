library(tidyverse)
names(txhousing)

help(txhousing)

head(txhousing)

cpi <-readRDS("~/Desktop/cpi.rds")
as.data.frame(cpi)

names(cpi)

head(cpi)
#Q1
month_map <- data.frame("month_name"=unique(cpi$month_name) , "month"=c(1: 12) )
print(month_map)

#Q2
cpi_merge<- cpi%>%
    left_join(month_map, by="month_name")
head(cpi_merge)

#Q3
housing <- txhousing %>% left_join(cpi_merge, by=c("year", "month")) %>% select(-month_name)
head(housing)

#Q4

cpi_2020_07 <- cpi_merge %>% filter(cpi_merge$month==7 & cpi_merge$year == 2020)
cpi_2020_07 <- cpi_2020_07$cpi
housing<- housing %>% mutate(cpi_latest= cpi_2020_07, multiplier = cpi_latest/cpi, volume_adj = volume * multiplier, median_adj=median*multiplier)
head(housing)

#Q5
housing_sum1 <- housing %>% 
  group_by(city) %>% 
  summarize(volume_adj_max=max(volume_adj), 
            volume_adj_min=min(volume_adj), 
            median_adj_max=max(median_adj),
            median_adj_min=min(median_adj))

housing_1 <- housing %>% left_join(housing_sum1, by = "city") 

head(housing_1)
tail(housing_1)

#Q6
housing_min <- housing_1 %>% 
  filter(volume_adj==volume_adj_min)
head(house_min)

housing_max <- housing_1 %>% 
  filter(median_adj==median_adj_max)
head(housing_max)

#Q7
housing_sum2 <- housing_1 %>% 
  group_by(year, month) %>% 
  summarize(listings_med = median(listings, na.rm=TRUE), 
            sales_med= median(sales, na.rm=TRUE))

housing_2 <- housing_1 %>% 
        left_join(housing_sum2, by= c("year","month"))

#Q8
housing_2<-housing_2 %>% 
  mutate(listings_ind= if_else(listings<=listings_med, 0, 1, missing = NULL), sales_ind= if_else(sales<=sales_med, 0, 1, missing= NULL)) 

#Q9
housing_2 <- housing_2 %>% 
  mutate(marktet_hotness= if_else(listings_ind==0&sales_ind==1, "High", 
                                  if_else(listings_ind==1&sales_ind==1, "Average",
                                  if_else(listings_ind==0&sales_ind==0, "Low",
                                  "Very low" ))))
#Q10
highest <- housing_1 %>% 
    group_by(city) %>% 
    summarize(max_med_adj=mean(median_adj)) %>% 
    arrange(desc(max_med_adj))
#The highest sales price is from Collin County, which is $252,325. 


