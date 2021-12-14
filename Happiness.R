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
#Scatterplots
#Setting up arrangement of plots
par(mfrow=c(3,2))

#plotting GDP vs Happiness
plot(gdp, score,
     pch = 20,
     col = "steelblue",
     main = "GDP vs Happiness")

#plotting Social Support vs Happiness
plot(social, score,
     pch = 20,
     col = "steelblue",
     main = "Social Support vs Happiness")

#plotting Life Expectancy vs happiness
plot(health, score,
     pch = 20,
     col = "steelblue",
     main = "Life Expectancy vs Happiness")

#plotting Freedom vs happiness
plot(freedom, score,
     pch = 20,
     col = "steelblue",
     main = "Freedom vs Happiness")

#plotting Generosity vs happiness
plot(generosity, score,
     pch = 20,
     col = "steelblue",
     main = "Generosity vs Happiness")

#plotting Corruption vs happiness
plot(corruption, score,
     pch = 20,
     col = "steelblue",
     main = "Corruption vs Happiness")









