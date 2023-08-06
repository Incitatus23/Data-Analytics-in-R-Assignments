suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))
#Q1
pizza <- read_csv('pizza.csv')
cor1 <- pizza %>%
  select(temperature, bill, pizzas, got_wine) 
no_missing <- na.omit(cor1)
cor12 <- cor(no_missing)
Q1 <- round(cor12, digits = 2)
#Q2
cor2 <- pizza %>%
  select(operator, branch, time, temperature, bill, pizzas) %>%
  filter( operator == "Laura" & branch == "East") %>%
  select(time, temperature, bill, pizzas)
no_missing2 <-na.omit(cor2)
cor22 <-cor(no_missing2)
Q2 <-round(cor22, digits =2)
#Q3
wine_pred <- pizza %>%
  select(got_wine, temperature, bill, pizzas)
wine_pred2 <- na.omit(wine_pred)
wine_pred3<- glm(got_wine ~ temperature + bill + pizzas, family = "binomial", data = wine_pred2)
wine_pred4 <-summary(wine_pred3)$coefficients 
Q3 <- round(wine_pred4, digits = 2)
#Q4
pred_bill <- pizza %>%
  select(temperature, bill, pizzas, got_wine) 
pred_bill2 <-na.omit(pred_bill)
pred_bill3 <- lm(bill ~ temperature + pizzas + got_wine, data = pred_bill2)
pred_bill4 <- lm.beta(pred_bill3)
Q4 <- print(pred_bill4, digits = 1)
#Q5
add_oper <-pizza %>%
select(operator, temperature, bill, pizzas, got_wine)
add_oper2 <-na.omit(add_oper)
add_oper3 <-lm(bill ~ temperature + pizzas + got_wine + operator, data = add_oper2)
Q5 <- AIC(pred_bill3)
