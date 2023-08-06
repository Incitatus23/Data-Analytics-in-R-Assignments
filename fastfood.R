suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))
#Q1
high_cal <- fastfood %>%
  select(restaurant,item, calories)%>%
  filter(restaurant== "Burger King" | restaurant == "Chick-Fil-A")%>%
  arrange(desc(calories))%>%
  select(item) 
Q1 <- head(high_cal, 1)
#Q2
high_sugar <- fastfood %>%
  select(restaurant, sugar) %>%
  filter(restaurant == "Subway")%>%
  summarise(mean_sugar = mean(sugar))
Q2 <- as.data.frame(high_sugar) %>%
  mutate_if(is.numeric, round, digits = 2)
#Q3
mean_taco <- fastfood %>%
  select(restaurant, calories) %>%
  filter(restaurant == "Taco Bell")%>%
  summarise(mean_cal = mean(calories))
Q3 <- as.data.frame(mean_taco) %>%
  mutate_if(is.numeric, round, digits = 2)
#Q4
fat_sugar <- fastfood %>%
  mutate(fatXsugar = total_fat * sugar) %>%
  select(restaurant, item, fatXsugar)%>%
  arrange(desc(fatXsugar))
Q4 <- head(fat_sugar, 3)
#Q5
high_sat <- fastfood %>%
  select(restaurant, sat_fat) %>%
  group_by(restaurant) %>%
  summarise(mean_sat = mean(sat_fat))%>%
  filter(mean_sat > 10)
Q5 <- 3
