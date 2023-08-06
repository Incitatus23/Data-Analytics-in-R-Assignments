suppressPackageStartupMessages(library(tidyverse))
#Q1
var1 <-msleep %>%
  select(vore, conservation, sleep_total)%>%
  filter(vore == "carni" & conservation == "lc") %>%
  summarise( var = var(sleep_total)) %>%
  as.data.frame()
Q1 <- round(var1, digits = 2)
#Q2
sleep_rat <- msleep %>%
  mutate(rem_prop = sleep_total/sleep_rem)%>%
  select(name, order, rem_prop)%>%
  filter(order == "Rodentia")%>%
  arrange(desc(rem_prop))%>%
  select(name)
Q2 <- head(sleep_rat, 1)
 #Q3
brain_body <- msleep %>%
  mutate(weight_prop = bodywt/brainwt)%>%
  select(name, order, weight_prop) %>%
  filter(order == "Primates" & weight_prop > 100)
Q3 <- 3
#Q4
new_var <- msleep %>%
  group_by(conservation)%>%
  mutate(mean_sleep = mean(sleep_total))%>%
  mutate(var_sleep = var(sleep_total)) %>%
  select(conservation, mean_sleep, var_sleep) %>%
  summarise_at(c("mean_sleep", "var_sleep"), mean, na.rm = TRUE) %>%
  na.omit()
Q4 <- new_var %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 2)

#Q5
most_sleep <- msleep %>%
  select(name, vore, conservation, sleep_total)%>%
  filter(vore == "herbi" &  conservation == "domesticated", sleep_total > 12)%>%
  select(name)
Q5 <-head(most_sleep, 1)


  
  