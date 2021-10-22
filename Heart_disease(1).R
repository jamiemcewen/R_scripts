library(ggplot2)
library(tidyverse)

#Import dataset that contains information about age, blood cholesterol and resting blood pressure
heartdata <- read.csv("heart1.csv")

#This analysis will develop and validate a model to predict the effect of age on resting blood pressure and use the analysis to ask:
#"At what age is there an increased risk of hypertension?"
#Make the variables easier to read and remember:
age <- heartdata$age
chol <- heartdata$chol
restingbp <- heartdata$trtbps


#--------------------------Initial plots--------------------------#
#Age appears to be related blood pressure
heartdata %>% ggplot() +
  geom_point(mapping=aes(x=age,y=trtbps),size=2) +
  labs(x="Age",y="Resting Blood Pressure (mmHg)") +
  theme_light()


#Cholesterol and Blood Pressure also seem to be related
heartdata %>% ggplot() +
  geom_point(mapping=aes(x=chol,y=trtbps),color="#D55E00",shape=0, size=2) +
  labs(x="Cholesterol (mg/dl)",y="Resting Blood Pressure (mmHg)") +
  theme_light()


#Age and cholesterol
heartdata %>% ggplot() +
  geom_point(mapping=aes(x=age,y=chol),color="#0072B2",shape=1,size=2) +
  labs(x="Age",y="Cholesterol (mg/dl)") +
  theme_light()



#--------------------------Model development--------------------------#

#An initial model of age effects suggests that age does have an effect on resting blood pressure
summary(glm(restingbp ~ age, family = "gaussian"))

#But we can adjust the effect of age for cholesterol levels for a more comprehensive model. Age 
age_chol_model <- glm(restingbp ~ age + chol, family = "gaussian")
summary(age_chol_model)
with(summary(age_chol_model), 1 - deviance/null.deviance) #Douglas.DC3



#--------------------------Model validation--------------------------#
#To validate this model: 
  #1) plot the fitted model to residuals to test for remaining variation
  #2) test the residuals for normality
  #3) gauge the effect of outliers, if any
  #4) test the model against another data set


#1) Plot the fitted model to residuals.# The residuals appear to be fairly evenly distributed vs fitted model values, though with higher variability in middle ranges
model_resids <- resid(age_chol_model) #extract residuals from the model
plot(fitted(age_chol_model),model_resids) #plot the fitted model values vs. the residuals
abline(0,0) #add a line to help visualize


#2) test the residuals for normality. Residuals look to be normal with a slight tail and deviation at higher levels
hist(model_resids) #histogram visualization

qqnorm(model_resids) #qq plot visualization
qqline(model_resids)


#3) Assess outliers. There are no extreme outliers, though some higher resting blood pressures for individuals in the mid-50s
heartdata %>% ggplot() +
  geom_point(mapping=aes(x=age,y=trtbps),color="#696969",shape=1, size=2) +
  geom_smooth(mapping=aes(x=age,y=trtbps), method='lm', se=FALSE, color="#e9692c", size=0.75) +
  labs(x="Age",y="Resting Blood Pressure (mmHg)") +
  theme_light()


#4) Test the model against another data set of age, resting blood pressure and cholesterol
secondary_data <- read.csv("heart.csv")

test_model <- glm(secondary_data$trestbps ~ secondary_data$age + secondary_data$chol, family = "gaussian")
summary(test_model)
with(summary(test_model), 1 - deviance/null.deviance) #R2 estimate (goodness of fit)
#The model holds up to another data set, with simliar parameter estimates.

#Averaging the parameter estimates when applied to both datasets gives us the following model:
# 
(98.78 + 98.25)/2 #Intercept parameter
(0.493 + 0.512)/2 #Effect/slope estimate

#Blood pressure (mmHg) = 98.52 + 0.50(Age)
#Hypertension is defined as systolic pressure > 130 mmHg. So we would expect, on average:
(130-98.52)/0.5
#People 63 years and older will be at increased risk of hypertension and heart disease.

