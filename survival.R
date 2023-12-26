
# Install packages needed
install.packages(c("knitr", "dplyr", "survival", "ggplot2", "here", "tibble"))
install.packages(c("lubridate", "ggsurvfit", "gtsummary", "tidycmprsk"))
install.packages("ggsurvplot")


library(knitr)
library(dplyr)
library(survival)
library(ggplot2)
library(tibble)
library(lubridate)
library(ggsurvplot)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)

colon <- read.csv ("colon.csv")
head(colon [, c("time", "status", "sex")])

#Creating survival objects and curves

Surv(colon$time, colon$status)[1:10]

#creates survival curves using the Kaplan-Meier method

c1 <- survfit(Surv(time, status) ~ 1, data = colon)
str(c1)

#Kaplan-Meier plots

survfit2(Surv(time, status) ~ 1, data = colon) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )

#Add confidence intervals

survfit2(Surv(time, status) ~ 1, data = colon) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()

#add numbers at risk

survfit2(Surv(time, status) ~ 1, data = colon) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()+
  add_risktable()


#Estimating a probability of surviving beyond a certain number of years, x

#For example 1 year = 365.25
summary(survfit(Surv(time, status) ~ 1, data = colon), times = 1095.75) #3 years 

#Create a table

survfit(Surv(time, status) ~ 1, data = colon) %>% 
  tbl_survfit(
    times = 1095.75,
    label_header = "**3-year survival (95% CI)**"
  )

#Estimating median survival time
survfit(Surv(time, status) ~ 1, data = colon)

#Summarize the median survival time among the 446 patients who died:

colon %>% 
  filter(status == 1) %>% 
  summarize(median_surv = median(time))

#tables of median survival time estimates
survfit(Surv(time, status) ~ 1, data = colon) %>% 
  tbl_survfit(
    probs = 0.5,
    label_header = "**Median survival (95% CI)**"
  )

#Comparing survival times between groups

survdiff(Surv(time, status) ~ sex, data = colon) #Observe P value

#The Cox regression model

coxph(Surv(time, status) ~ sex, data = colon)

#Table it
coxph(Surv(time, status) ~ sex, data = colon) %>% 
  tbl_regression(exp = TRUE) 

















