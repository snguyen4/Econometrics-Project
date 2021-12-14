#1) Initialization -------------------------------------------------------------
#-------------------------------------------------------------------------------

#Loading libraries
library(stargazer)
library(tidyverse)
library(corrplot)
library(AER)

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

#Summary
str(happiness)

#2) Visual inspection ----------------------------------------------------------
#-------------------------------------------------------------------------------
#Correlation plot
# Create a correlation plot
cols = sapply(happiness, is.numeric)
correlations = cor(happiness[, cols])
corrplot(correlations, method = "number")


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

#3) Regressions ----------------------------------------------------------------
#-------------------------------------------------------------------------------
#Most basic regression: GDP vs happiness
mod1 = lm(score ~ gdp, data = happiness)
summary(mod1)
coeftest(mod1, vcov. = vcovHC, type = "HC1")


#Model with all the variables
fullMod = lm(score ~ gdp + social + health + freedom + generosity + corruption, data = happiness)

summary(fullMod)


# Table with all the regressions
# stargazer(fullMod,
#           type="html",
#           digits = 3,
#           header = F,
#           out = "fullMod.html")





