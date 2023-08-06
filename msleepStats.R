#Q1
#try different way of omitting NA values
suppressPackageStartupMessages(library(tidyverse))
Q1 <- cor.test(msleep$sleep_total,msleep$bodywt)
print(Q1)
#Q2
na_not <- msleep %>%
  select(sleep_total, sleep_rem, brainwt, bodywt)
na_notmissing <-na.omit(na_not)
cor_not  <- cor(na_notmissing)
Q2 <- print(cor_not, digits = 2)
#Q3
msleep_model <- lm(bodywt ~ vore, data = msleep)
mod_co <- msleep_model$coefficients 
Q3 <-round(mod_co, 2)
#Q4
sleep_nodel2 <- lm(bodywt ~ vore + sleep_rem, data = msleep)
sum_2 <- summary(sleep_nodel2)
msleep_model_AIC <- AIC(msleep_model, k =1)
sleep_nodel2_AIC <- AIC(sleep_nodel2, k =2)
Q4 <- sleep_nodel2_AIC
print(Q4)
#Q5
an_type <- msleep %>%
  select(vore, sleep_total )%>%
  filter(vore != "omni" & vore != "insecti") %>%
  mutate(vorebin = ifelse(vore == 'carni', 0, 1))
an_type_model <- glm(vorebin ~ sleep_total, data = an_type, family = "binomial")
Q5 <- an_type_model

  
