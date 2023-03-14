setwd('C:/Users/m2000/Desktop/UOG/S2/DAS/小组作业2/GLM')
library(readr)
library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(areaplot)
library(dplyr)
library(skimr)
library(kableExtra)
library(gridExtra)
library(ggplot2)
library(ISLR)
library(plotly)
library(MASS)


dataset13 <- read_csv("dataset13.csv")
newdataset<- na.omit(dataset13)
newdataset<- newdataset%>%
  arrange(desc(altitude_mean_meters))
newdataset<- newdataset[-c(1:4),]
newdataset<- newdataset%>%
  arrange(aroma)
newdataset<- newdataset[-1,]

### Model 
str(newdataset)
newdataset[,2:6]%>%
  cor()
names(newdataset)
newdataset$Qualityclass <- as.factor(newdataset$Qualityclass)
newdataset$country_of_origin<- as.factor(newdataset$country_of_origin)
levels(newdataset$Qualityclass) <- c( "Good","Poor")
newdataset$harvested <- as.factor(newdataset$harvested)
#Glm
mod.cafe <- glm(Qualityclass ~  country_of_origin +aroma + flavor+acidity+category_two_defects+altitude_mean_meters+harvested, data = newdataset,
                family = binomial(link = "logit"))
mod.cafe %>%
  summary()#AIC 543

plot_model(mod.cafe, show.values = TRUE,
           title = "", show.p = FALSE, value.offset = 0.5)
# Fit a glm using stepwise regression with AIC as the criterion
model.step <- stepAIC(glm(Qualityclass ~  country_of_origin +aroma + flavor+acidity+category_two_defects+altitude_mean_meters+harvested, data = newdataset, family = binomial(link = "logit")), direction = "both", trace = FALSE)
# Print the selected model
summary(model.step)#AIC 537
plot_model(model.step, show.values = TRUE,
           title = "", show.p = FALSE, value.offset = 0.50)


#interaction
mod.cafe <- glm(Qualityclass ~  country_of_origin +aroma + flavor+acidity+category_two_defects+altitude_mean_meters+harvested+aroma:flavor+flavor:acidity+aroma:acidity, data = newdataset,
                   family = binomial(link = "logit"))
mod.cafe %>%
  summary() #AIC 539
plot_model(mod.cafe, show.values = TRUE,
           title = "", show.p = FALSE, value.offset = 0.5)
# Fit a glm using stepwise regression with AIC as the criterion
model.step <- stepAIC(glm(Qualityclass ~  country_of_origin +aroma + flavor+acidity+category_two_defects+altitude_mean_meters+harvested+aroma:flavor+flavor:acidity+aroma:acidity, data = newdataset, family = binomial(link = "logit")), direction = "both", trace = FALSE)
# Print the selected model
summary(model.step)#AIC 532
plot_model(model.step, show.values = TRUE,
           title = "", show.p = FALSE, value.offset = 0.50)

levels(newdataset$country_of_origin)
