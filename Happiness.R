#1) Initialization -------------------------------------------------------------
#-------------------------------------------------------------------------------

#Loading libraries
library(stargazer)
library(tidyverse)

#Loading data
happiness = read.csv("world-happiness-report-2021.csv")
head(happiness)

#Renaming columns
happiness = happiness %>%
  rename(country = ï..Country.name,
         region = Regional.indicator,
         score = Ladder.score,
         gdp = Logged.GDP.per.capita,
         social = Social.support,
         health = Healthy.life.expectancy,
         freedom = Freedom.to.make.life.choices,
         generosity = Generosity,
         corruption = Perceptions.of.corruption)

#Keeping columns of interest
happiness = select(happiness, -c(4:6, 13:20))

attach(happiness)

#Adding additional column: poor vs rich
summary(happiness)


#2) Visual inspection ----------------------------------------------------------
#-------------------------------------------------------------------------------

#First Model
model1 = lm(score ~ gdp)
summary(model1)

#plotting GDP vs happiness
plot(gdp, score,
     pch = 20,
     col = "steelblue",
     main = "GDP vs Happiness")

abline(model1, col = "red")



