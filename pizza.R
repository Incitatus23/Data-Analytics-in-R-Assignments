suppressPackageStartupMessages(library(tidyverse))
#Q1
pizza <- read_csv('pizza.csv')
driver1 <- pizza %>%
  select(driver, free_wine, discount_customer, pizzas) %>%
  filter(free_wine == "1" & discount_customer == "1") %>%
  filter(pizzas > 4) %>%
  select(driver)
Q1 <- driver1
#Q2
bill_ratio <- pizza %>%
  mutate(ratio = bill / pizzas) %>%
  summarise(mean_ratio = mean(ratio)) %>%
  as.data.frame(bill_ratio)
Q2 <- round(bill_ratio, digits = 2)
#Q3
var_pizza <- pizza %>%
  group_by(day) %>%
  mutate(var_pizzas = var(pizzas)) %>%
  summarise(mean_var = mean(var_pizzas))
Q3 <- var_pizza
#Q4
high_bill <- pizza %>%
  group_by(operator) %>%
  select(operator, bill) %>%
  summarise(mean_bill = mean(bill)) %>%
  arrange(desc(mean_bill)) %>%
  select(operator)
Q4 <- head(high_bill, 1)
#Q5
high_free <- pizza %>%
  group_by(day, driver)%>%
  select(day, driver, free_wine) %>%
  summarise(n = as.integer(sum(free_wine))) %>%
  arrange(desc(n))
Q5 <- head(high_free, 1)

  



  
  

  