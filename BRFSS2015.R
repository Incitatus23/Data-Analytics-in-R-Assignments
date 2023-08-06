suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(psych))
codebook <- read_csv('BRFSS2015.csv')
#Q1
healthcare_num1 <- codebook %>%
  select(HLTHPLN1) %>%
  filter(HLTHPLN1 == 1)
Q1 <- count(healthcare_num1)
#Q2
names(codebook) <-sub("_", "", names(codebook), fixed = TRUE)
avg_days <- codebook %>%
  select(MENTHLTH,STATE) %>%
  filter(STATE == "42") %>%
  filter(is.numeric(MENTHLTH))%>%
  filter(MENTHLTH != "77" & MENTHLTH != "99")
avg_days[avg_days == 88] <- 0
avg_days2 <- avg_days %>%
  summarise(mean(MENTHLTH))
Q2 <- avg_days2
#Q3
Q3 <- codebook%>%
  select(HAVARTH3, WTKG3)%>%
  filter(HAVARTH3 == "1" |  HAVARTH3 == "2") %>%
  group_by(HAVARTH3) %>%
  mutate(weight_lbs = (WTKG3/100)*2.20462) %>%
  select(HAVARTH3, weight_lbs)%>%
  na.omit() %>%
  mutate(mean_weight = mean(weight_lbs))%>%
  mutate(sd_weight = sd(weight_lbs)) %>%
  select(HAVARTH3, mean_weight, sd_weight) %>%
  summarise_at(c("mean_weight", "sd_weight"), mean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 2)
#Q4
activity <- codebook%>%
  select(PA1MIN, MARITAL)%>%
  na.omit() %>%
  filter(MARITAL != "9")
act_upper <- quantile(activity$PA1MIN, 0.997, na.rm = TRUE)
act_lower <-quantile(activity$PA1MIN, 0.003, na.rm = TRUE)
#I included the less than or equal to sign for act_lower because without it, the smallest values all remained. Still no chance for normal
#distribution, but I wanted to remove those values
act_out <- which(activity$PA1MIN > act_upper | activity$PA1MIN <= act_lower)
act_percent <-(nrow(activity)-length(act_out))/nrow(activity)*100
Q4 <- round(act_percent, digits = 2)
#Q5
activity_noout <- activity[-act_out,]
activity_noout$MARITAL <-factor(activity_noout$MARITAL)
marital_act <- activity_noout %>%
  group_by(MARITAL) %>%
  mutate(mean_mins = mean(PA1MIN))%>%
  mutate(sd_mins = sd(PA1MIN))%>%
  mutate(max_mins = max(PA1MIN)) %>%
  mutate(min_mins = min(PA1MIN)) %>%
  select(MARITAL, mean_mins, sd_mins, max_mins, min_mins) %>%
  summarise_all(mean,na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 2)
#Q6
Q6 <- ggplot(activity_noout, aes(x = MARITAL, y= PA1MIN)) +
  geom_boxplot()
#Q7
marital_exercise <- lm(PA1MIN ~ MARITAL, activity_noout)
Q7 <- summary(marital_exercise)
#Q8
marital_exercise2 <- aov(PA1MIN ~ MARITAL, data = activity_noout)
Q8 <- TukeyHSD(marital_exercise2, conf.level = 0.95)
#Q9
add_frut <- codebook%>%
  select(PA1MIN, MARITAL, FRUTSUM)%>%
  na.omit() %>%
  filter(MARITAL != "9")
act_upper2 <- quantile(add_frut$PA1MIN, 0.997, na.rm = TRUE)
act_lower2 <-quantile(add_frut$PA1MIN, 0.003, na.rm = TRUE)
#Just as before, I included the less than or equal to sign for act_lower because without it, the smallest values all remained.
act_out2 <- which(add_frut$PA1MIN > act_upper | add_frut$PA1MIN <= act_lower)
add_frut2 <- add_frut[-act_out2,]
frut_marital <- lm(PA1Min ~ MARITAL + FRUTSUM, add_frut2)
summary(frut_marital)
summary(marital_exercise)
AIC(frut_marital)
AIC(marital_exercise)
Q9 <- AIC(frut_marital)
#Based on the AIC and adjusted R-squared score, the model including both marital status and fruit consumption is the better model
#Q10
#The code for removing outliers is in Q13, since I decided which outliers to remove after the initial analyses. I ultimately
#to remove the observations for MAXDRNKS that were 3 standard deviations above the mean. My logic for this was based on the distribution
#of the data. The outliers were so extreme compared to mean/median data, did not seems to reflect actual real life situations, and 
#represented a small amount of the observations. I had a hard time believing that people had consumed more than 30+ drinks in a single setting.
#Income was grouped, so there was no need to worry about outliers. For PHYSHLTH and ADDOWN, I did not worry about outliers because for those 
#variables, there was a decent amount of observations as the highest possible values, and those values represent an important part of the
#data. 30 for PHYSHLTH represented people who felt poor health for all 30 days of the last month, and ADDOWN represented people who felt
#sad,hopeless, or depressed for all 14 days of the last 2 weeks.
#Q11
#The four variables I chose are INCOME2, MAXDRNKS, PHYSHLTH, and ADDOWN. INCOME2 asks the income of the respondents. 1 means less than 10,000, 2 means
# 10 to less than 15,000, 3 means 15 to less than 20,000, 4 means 20 to less than 25,000, 5 means 25 to less than 35,000, 6 means 35 to less 
# than 50,000, 7 means 50 to less than 75,000, and 8 means more than 75,000.  77 means they didn't know and 99 means they refused to answer. MAXDRNKS measures the largest number of drinks the respondents had in a single setting during the last 30 days.
# The values range between 1-76; 77 means that respondents didn't know, 99 meant they refused to answer, and blank means not asked/missing.
#ADDOWN measures the number of days the respondents have felt down, depressed, or hopeless during the last 2 weeks. The values range from
#1-14; 77 means they don't know, 88 means 0 days, and blank means not asked/missing. PHYSHLTH measures the number of days the respondent had 
#"not good" physical health over the last 30 days. The values range from 1-30 days; 88 means 0 days, 77 means the respondents didn't know
#and 99 means the respondents refused to answer, and blank means not asked/missing.
#Q12
#I filtered to remove the missing values, and those respondents who either did not know or refused to answer the question. I also changed
#the none responses to 0 for the appropriate variables.
health <- codebook %>%
  select(INCOME2, ADDOWN, MAXDRNKS, PHYSHLTH) %>%
  na.omit()
health[health == 88] <- 0
health2 <- health %>%
  filter(ADDOWN !="77" & ADDOWN != "99" & MAXDRNKS != "77" & MAXDRNKS != "99" & PHYSHLTH != "77" & PHYSHLTH != "99" & INCOME2 != "77" & INCOME2 != "99")
#Below, I created a boxplot to compare income to the other variables, and then plotted to show the frequency of each possible response
#for all of the variables. This data led me to remove the upper level outliers for MAXDRNKS.
ex2 <- ggplot(health2, aes(x= INCOME2, y = PHYSHLTH))
   geom_boxplot()
ex3 <- ggplot(health2, aes(x= INCOME2, y = ADDOWN))+
   geom_boxplot()
ex4 <- ggplot(health2, aes(x= INCOME2, y = MAXDRNKS))+
   geom_boxplot()
ex5 <- ggplot(health2, aes(x= INCOME2, y = MAXDRNKS))+
  geom_count()
ex6 <- ggplot(health2, aes(x= INCOME2, y = PHYSHLTH))+
  geom_count()
ex7 <- ggplot(health2, aes(x= INCOME2, y = ADDOWN))+
  geom_count()
ex8 <- ggplot(health2, aes(x= INCOME2))+
  geom_bar()
ex9 <- ggplot(health2, aes(x= PHYSHLTH))+
  geom_bar()
ex10 <- ggplot(health2, aes(x= ADDOWN))+
  geom_bar()
ex11 <- ggplot(health2, aes(x= MAXDRNKS))+
  geom_bar()
#Q13
#I filtered out the MAXDRNKS outliers and then created boxplots using the updated dataset. I then created a correlation matrix for 
# the data. INCOME2 and PHYSHLTH are negatively correlated. As income increases, the number of days of poor health decreases. INCOME2 is also
# negatively correlated with ADDOWN. As income increases, the number of days respondents report being depressed, down, or hopeless decreases.
# ADDOWN and PHYSHLTH are positively correlated. 
descrpt1 <- summary(health2)
descrpt2 <- describe(health2)
drinks_upper <- quantile(health2$MAXDRNKS, 0.997, na.rm = TRUE)
drinks_out <- which(health2$MAXDRNKS > drinks_upper)
health3 <- health2[-drinks_out,]
descrpt3 <-summary(health3)
descrpt4 <- describe(health3)
ex12 <- ggplot(health3, aes(x= INCOME2, y = MAXDRNKS))+
  geom_boxplot()
ex13 <- ggplot(health3, aes(x= INCOME2, y = MAXDRNKS))+
  geom_count()
ex14 <- ggplot(health3, aes(x= MAXDRNKS))+
  geom_bar()
cor_health <- cor(health3)
#Q14
health_fit <- lm(PHYSHLTH ~ INCOME2 + ADDOWN +  MAXDRNKS, health3)
summary(health_fit)
AIC(health_fit)
health_fit2 <- lm(PHYSHLTH ~ INCOME2 + ADDOWN, health3)
health_fit3 <- lm(PHYSHLTH ~ INCOME2,  health3)
health_fit4 <- lm(PHYSHLTH ~ ADDOWN,  health3)
#Based on the AIC and R-squared values, the first model with INCOME2, ADDOWN, & MAXDRNKS
#is the best fit. However, dropping MAXDRNKS from the model only slightly lowers the AIC and R-squared value) The p-values
# show that the model has significant variables, but the low R-squared shows that the variables do not explain the variablity
# in the PHYSHLTH variable. 





  


  


