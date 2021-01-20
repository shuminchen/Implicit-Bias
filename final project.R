rm(list = ls())

library(readr)
library(ggplot2)
library(dplyr)
library(texreg)
library(foreign)
setwd("C:/Users/Shumin Chen/Harvard/Fall 2020/S040/dataset/Human Developmengt Report")
hdr <- read.dta('hdr.dta')



hdr$youth_literacy_female[hdr$youth_literacy_female <= -1] <- NA


hdr %>% ggplot(aes(x = youth_literacy_female)) + geom_histogram(na.rm = TRUE)
hdr %>% ggplot(aes(x = education_spending)) + geom_histogram(na.rm = TRUE)
hdr %>% ggplot(aes(x = gender_ineq_2015)) + geom_histogram(na.rm = TRUE)
hdr %>% ggplot(aes(x = l2_gni)) + geom_histogram(na.rm = TRUE)
hdr %>% ggplot(aes(x = school_life_exp)) + geom_histogram(na.rm = TRUE)
hdr %>% select(pre_primary_pct)%>% table()
hdr$gender_ineq_2015 %>% summary()


hdr_sub <- hdr %>% filter(complete.cases(cbind(school_life_exp_2015_female, child_labor_pct, ineq_educ))) # ensure that our sample does not include anyone missing school_life_exp, education_spending, and gni

model <- lm(school_life_exp_2015_female ~ country + gender_ineq_2015, pre_primary_pct, data = hdr_sub) # add predictors to a regression model using the '+' operator
summary(model)



mulregmodel <- lm(school_life_exp ~ education_spending + l2_gni, data = hdr_sub) # add predictors to a regression model using the '+' operator

summary(mulregmodel) # the fitted mdoel






linregmodel <- lm(school_life_exp ~ education_spending, data = hdr_sub) # create a new model for a taxonomy table
summary(linregmodel)
screenreg(list(linregmodel, mulregmodel)) # and make a table with both of the fitted models








mulregmodel2 <- lm(school_life_exp ~ education_spending + l2_gni+ineq_educ+ ineq_inc, data = hdr_sub) # add predictors to a regression model using the '+' operator

summary(mulregmodel2) # the fitted mdoel
mulregmodel2%>% confint.lm()




fullmodel <- lm(school_life_exp ~ education_spending + l2_gni+ineq_educ+ineq_inc, data = hdr_sub)
reducedmodel <- lm(school_life_exp ~ ineq_educ + l2_gni, data = hdr_sub)
anova(fullmodel,reducedmodel)
