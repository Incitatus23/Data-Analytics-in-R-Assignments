#Q1
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nycflights13))
delay_upper <-quantile(flights$dep_delay, 0.997, na.rm = TRUE)
delay_lower <-quantile(flights$dep_delay, 0.003, na.rm = TRUE)
delay_out <- which(flights$dep_delay > delay_upper | flights$dep_delay < delay_lower)
Q1 <-(nrow(flights)-length(delay_out))/nrow(flights)*100
print(Q1)
#Q2
flights_noout <-flights[-delay_out,]
Q2 <- cor.test(flights_noout$dep_delay, flights_noout$distance)
print(Q2)
#Q3
flights_model <-lm(dep_delay ~ distance, flights_noout)
Q3 <- summary(flights_model)
print(Q3)
#Q4
suppressPackageStartupMessages(library(lm.beta))
Q4 <- lm.beta(flights_model)
print(Q4)
#Q5
flights_model2 <- lm(dep_delay ~ distance + carrier, flights_noout)
Q5 <- summary(flights_model2)
print(Q5)
