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

#Testtest
data("CASchools")
CASchools$STR <- CASchools$students/CASchools$teachers       
CASchools$score <- (CASchools$read + CASchools$math)/2

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

#To use the dataset variables without always using happiness$
attach(happiness)

#Keeping columns of interest
happiness = select(happiness, -c(2, 4:6, 13:20))

#Creating Rich countries variable for dummy testing. Testing if GDP as an indicator
#of happiness is more impactful for rich countries compared to other ones.
summary(happiness$gdp)
happiness$rich = ifelse(gdp >= 10.421, 1, 0)

#Summary
str(happiness)



#2) Visual inspection ----------------------------------------------------------
#-------------------------------------------------------------------------------
#Correlation plot
# Create a correlation plot
cols = sapply(happiness, is.numeric)
correlations = cor(happiness[, cols])
corrplot(correlations, method = "number")

#Testtest
cols2 = sapply(CASchools, is.numeric)
correlations2 = cor(CASchools[, cols2])
corrplot(correlations2, method = "number")

#testplot
plot(CASchools$income, CASchools$score)


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

# #region grouping
# happinessRegion = happiness %>%
#   select(-3) %>%
#   group_by(region) %>%
#   summarise_at(vars(-country), funs(mean(., na.rm=TRUE)))
# 
# #Plotting
# ggplot(happiness, aes(x = gdp, y = score)) + 
#   geom_point(aes(color=region), size = 3, alpha = 0.8) +  
#   geom_smooth(aes(color = region, fill = region), 
#               method = "lm", fullrange = TRUE) +
#   facet_wrap(~region) +
#   theme_bw() + labs(title = "GDP vs Happiness with Regression Line", subtitle = "per region")

#GGplot plotting
ggplot(happiness, aes(x = gdp, y = score)) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", fullrange = TRUE) 

ggplot(happiness, aes(x = corruption, y = score)) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", fullrange = TRUE) 

ggplot(happiness, aes(x = generosity, y = score)) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", fullrange = TRUE) 

ggplot(happiness, aes(x = freedom, y = score)) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", fullrange = TRUE) 

ggplot(happiness, aes(x = health, y = score)) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", fullrange = TRUE) 

ggplot(happiness, aes(x = social, y = score)) +
  geom_point(color = "blue", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", fullrange = TRUE) 

#3) Regressions ----------------------------------------------------------------
#-------------------------------------------------------------------------------

#Basic regressions
#Most basic regression: GDP vs happiness
mod1 = lm(score ~ gdp, data = happiness)
summary(mod1)
coeftest(mod1, vcov. = vcovHC, type = "HC1")

#Adding social as a control variable
mod2 = lm(score ~ gdp + social, data = happiness)
summary(mod2)
coeftest(mod2, vcov. = vcovHC, type = "HC1")

#Adding health as a control variable
mod3 = lm(score ~ gdp + social + health, data = happiness)
summary(mod3)
coeftest(mod3, vcov. = vcovHC, type = "HC1")

#Adding freedom as a control variable
mod4 = lm(score ~ gdp + social + freedom, data = happiness)
summary(mod4)
coeftest(mod4, vcov. = vcovHC, type = "HC1")

#Health + freedom
mod5 = lm(score ~ gdp + social + health + freedom, data = happiness)
summary(mod5)
coeftest(mod5, vcov. = vcovHC, type = "HC1")

#Adding corruption as a control variable
mod6 = lm(score ~ gdp + social + corruption, data = happiness)
summary(mod6)
coeftest(mod6, vcov. = vcovHC, type = "HC1")

#Health + corruption
mod7 = lm(score ~ gdp + social + health + corruption, data = happiness)
summary(mod7)
coeftest(mod7, vcov. = vcovHC, type = "HC1")

#Freedom + corruption
mod8 = lm(score ~ gdp + social + freedom + corruption, data = happiness)
summary(mod8)
coeftest(mod8, vcov. = vcovHC, type = "HC1")

#Freedom + health + corruption
mod9 = lm(score ~ gdp + social + health + freedom + corruption, data = happiness)
summary(mod9)
coeftest(mod9, vcov. = vcovHC, type = "HC1")

#Model with all the variables
fullMod = lm(score ~ gdp + social + health + freedom + generosity + corruption, data = happiness)
summary(fullMod)
coeftest(fullMod, vcov. = vcovHC, type = "HC1")

#Standard robust errors
robSE = list(sqrt(diag(vcovHC(mod1, type = "HC1"))),
             sqrt(diag(vcovHC(mod2, type = "HC1"))),
             sqrt(diag(vcovHC(mod3, type = "HC1"))),
             sqrt(diag(vcovHC(mod4, type = "HC1"))),
             sqrt(diag(vcovHC(mod5, type = "HC1"))),
             sqrt(diag(vcovHC(mod6, type = "HC1"))),
             sqrt(diag(vcovHC(mod7, type = "HC1"))),
             sqrt(diag(vcovHC(mod8, type = "HC1"))),
             sqrt(diag(vcovHC(mod9, type = "HC1"))),
             sqrt(diag(vcovHC(fullMod, type = "HC1"))))

#Table with all the models
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, fullMod,
          se = robSE,
          digits = 3,
          header = F,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)", "(VII)", "(VIII)", "(IX)", "(X)"),
          type = "html",
          out = "Table.html")

#4) Non-Linear regression test -------------------------------------------------
#-------------------------------------------------------------------------------

#GDP ---------------------------------------------------------------------------

# fit the quadratic Model
quadraticModel = lm(score ~ gdp + I(gdp^2), data = happiness)

# obtain the model summary
coeftest(quadraticModel, vcov. = vcovHC, type = "HC1")

# draw a scatterplot of the observations for income and test score
plot(gdp, score,
     col  = "steelblue",
     pch = 20,
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the plot
abline(mod1, col = "black", lwd = 2)

# add quatratic function to the plot
orderId = order(gdp)

lines(x = gdp[orderId],
      y = fitted(quadraticModel)[orderId],
      col = "red",
      lwd = 2)

# estimate a cubic model
cubic_model <- lm(score ~ poly(gdp, degree = 3, raw = TRUE), data = happiness)

# test the hypothesis of a linear model against quadratic or polynomial
# alternatives

# set up hypothesis matrix
R <- rbind(c(0, 0, 1, 0),
           c(0, 0, 0, 1))

# do the test
linearHypothesis(cubic_model,
                 hypothesis.matrix = R,
                 white.adj = "hc1")
#summary
summary(cubic_model)

# test the hypothesis using robust standard errors
coeftest(cubic_model, vcov. = vcovHC, type = "HC1")

#Social ------------------------------------------------------------------------

# fit the quadratic Model
quadraticModel = lm(score ~ social + I(social^2), data = happiness)

# obtain the model summary
coeftest(quadraticModel, vcov. = vcovHC, type = "HC1")

# draw a scatterplot of the observations for income and test score
plot(social, score,
     col  = "steelblue",
     pch = 20,
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the plot
abline(lm(score ~ social), col = "black", lwd = 2)

# add quatratic function to the plot
orderId = order(social)

lines(x = social[orderId],
      y = fitted(quadraticModel)[orderId],
      col = "red",
      lwd = 2)

# estimate a cubic model
cubic_model <- lm(score ~ poly(social, degree = 3, raw = TRUE), data = happiness)

# test the hypothesis of a linear model against quadratic or polynomial
# alternatives

# set up hypothesis matrix
R <- rbind(c(0, 0, 1, 0),
           c(0, 0, 0, 1))

# do the test
linearHypothesis(cubic_model,
                 hypothesis.matrix = R,
                 white.adj = "hc1")
#summary
summary(cubic_model)

# test the hypothesis using robust standard errors
coeftest(cubic_model, vcov. = vcovHC, type = "HC1")

# estimate a level-log model
LinearLog_model <- lm(score ~ log(social), data = happiness)

# compute robust summary
coeftest(LinearLog_model, 
         vcov = vcovHC, type = "HC1")

# draw a scatterplot of the observations for income and test score
plot(social, score,
     col  = "steelblue",
     pch = 20,
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the plot
abline(lm(score ~ social), col = "black", lwd = 2)

# add quatratic function to the plot
orderId = order(social)

lines(x = social[orderId],
      y = fitted(LinearLog_model)[orderId],
      col = "red",
      lwd = 2)

#Corrution ---------------------------------------------------------------------------

# fit the quadratic Model
quadraticModel = lm(score ~ corruption + I(corruption^2), data = happiness)

# obtain the model summary
coeftest(quadraticModel, vcov. = vcovHC, type = "HC1")

# draw a scatterplot of the observations for income and test score
plot(corruption, score,
     col  = "steelblue",
     pch = 20,
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the plot
abline(lm(score ~ corruption), col = "black", lwd = 2)

# add quatratic function to the plot
orderId = order(corruption)

lines(x = corruption[orderId],
      y = fitted(quadraticModel)[orderId],
      col = "red",
      lwd = 2)

# estimate a cubic model
cubic_model <- lm(score ~ poly(corruption, degree = 3, raw = TRUE), data = happiness)

# test the hypothesis of a linear model against quadratic or polynomial
# alternatives

# set up hypothesis matrix
R <- rbind(c(0, 0, 1, 0),
           c(0, 0, 0, 1))

# do the test
linearHypothesis(cubic_model,
                 hypothesis.matrix = R,
                 white.adj = "hc1")
#summary
summary(cubic_model)

# test the hypothesis using robust standard errors
coeftest(cubic_model, vcov. = vcovHC, type = "HC1")

# estimate a level-log model
LinearLog_model <- lm(score ~ log(corruption), data = happiness)

# compute robust summary
coeftest(LinearLog_model, 
         vcov = vcovHC, type = "HC1")

# draw a scatterplot of the observations for income and test score
plot(corruption, score,
     col  = "steelblue",
     pch = 20,
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the plot
abline(lm(score ~ corruption), col = "black", lwd = 2)

# add linearlog function to the plot
orderId = order(corruption)

lines(x = corruption[orderId],
      y = fitted(cubic_model)[orderId],
      col = "red",
      lwd = 2)

modCorruption = lm(score ~ corruption)
summary(modCorruption)
summary(quadraticModel)
summary(cubic_model)
summary(LinearLog_model)
