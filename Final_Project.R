library(tidyverse)
library(lubridate)
setwd("~/Desktop/Data_Viz")

Barcelona <- read.csv("Barcelona.csv")
Milan <- read.csv("Milan.csv")
Stockholm <- read.csv("Stockholm.csv")


#Convert last review into date format
Barcelona$last_review <- ymd(Barcelona$last_review)
unique(Barcelona$room_type)
#Make room type into factor for further anaylsis 

Barcelona$room_type <- as.factor(Barcelona$room_type)
Barcelona_Reviews <- Barcelona %>% 
    filter(year(last_review) %in% c(2019, 2020), month(last_review) %in% c(1,2,3,4,5,6))

Bac2019 <- Barcelona_Reviews %>%
          filter(year(last_review) == 2019, month(last_review) %in% c(1, 2, 3, 4, 5, 6))%>%
          arrange(host_id)
summary(Bac2019)

Bac2020 <- Barcelona_Reviews %>%
  filter(year(last_review) == 2020, month(last_review) %in% c(1, 2, 3, 4, 5, 6))%>%
  arrange(host_id)
summary(Bac2020)

ggplot (Bac2019, aes(x = last_review, y = price, color = room_type))+
  geom_point() 

ggplot (Bac2020, aes(x = last_review, y = price, color = room_type))+
  geom_point() 


