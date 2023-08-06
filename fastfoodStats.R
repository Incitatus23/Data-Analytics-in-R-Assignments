suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))
suppressPackageStartupMessages(library(lm.beta))
#Q1
cor_fast <- fastfood %>%
  select(restaurant, calories, total_fat, sugar, calcium)%>%
  filter(restaurant == "Sonic" | restaurant == "Subway" | restaurant == "Taco Bell")%>%
  select(calories, total_fat, sugar, calcium)
cor_fast2 <- na.omit(cor_fast)
cor_matr <- cor(cor_fast2)
Q1 <- print(cor_matr, digits = 2)
#Q2
fast_2 <- fastfood %>% 
  filter(restaurant %in% c("Mcdonalds", "Subway")) %>%
  mutate(is_mcdonalds = restaurant == "Mcdonalds")
rest_prd <- glm(is_mcdonalds ~ calories + sodium + protein, 
             family = "binomial", data = fast_2)
Q2 <- round(coef(rest_prd), 2)
#Q3
fast_3 <- fastfood %>% 
  filter(restaurant %in% c("Mcdonalds", "Subway")) %>%
  mutate(is_mcdonalds = restaurant == "Mcdonalds")
with_sodium <-glm(is_mcdonalds ~ calories + protein + sodium, family = "binomial",  data = fast_3)
no_sodium <- glm(is_mcdonalds ~ calories + protein, 
                family = "binomial", data = fast_3)
Q3 <- round(AIC(with_sodium, k=2), digits = 2)
#Q4
cal_pred <- fastfood %>%
  select(calories, sat_fat, fiber, sugar)
cal_pred2 <- lm(calories ~ sat_fat + fiber + sugar, data = cal_pred)
cal_pred3 <- lm.beta(cal_pred2)
coefs2 <- cal_pred2$coefficients
coefs3 <- (coefs2[ c("sat_fat")])
Q4 <- round(coefs3, digits =2)
#Q5
test_fast <- fastfood %>%
  group_by(restaurant) %>%
  filter(n() >= 50 & n() <= 60)
fit_3 <- lm(total_fat ~ cholesterol + total_carb + restaurant,   data = test_fast)
coefs <- round(coef(lm.beta::lm.beta(fit_3)), 2)
Q5 <- coefs[which.max(coefs)]


