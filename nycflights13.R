suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nycflights13))
#Q1
av_model <- flights %>%
filter(carrier == "AA" | carrier == "EV" | carrier == "FL") %>%
  group_by(carrier)%>%
  summarise(av_distance = mean(distance))
Q1 <- as.data.frame(av_model) %>%
  mutate_if(is.numeric, round, digits = 2)
print(Q1)
#Q2
Q2 <- flights %>%
  group_by(month) %>%
  summarize(n = n())%>%
  slice_max(n)
print(Q2)
#Q3
flights_table <- flights %>%
  select(origin, dest, distance) %>%
  distinct(origin, dest, distance) %>%
  arrange(distance) %>%
  rename(min_dist = distance )
Q3 <- head(flights_table, 5)
#Q4
m_dist <- flights %>%
  filter(origin == "JFK") %>%
  select(month, day, distance) %>%
  group_by(month,day) %>%
  summarise(mean_distance = mean(distance))%>%
  arrange(desc(mean_distance)) %>%
  as.data.frame(m_dist) %>%
  mutate_if(is.numeric, round, digits = 2)
Q4 <- head(m_dist, 5)
print(Q4)
#Q5
Q5 <- flights %>%
  group_by(dest) %>%
  filter(!is.na(arr_delay)) %>%
  summarise(max_arr_delay = max(arr_delay)) %>%
  filter(dest == "ATL" | dest == "BOS")

  
  
  
