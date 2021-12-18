#1) Initialization -------------------------------------------------------------
#-------------------------------------------------------------------------------

#Loading libraries
library(stargazer)
library(tidyverse)
library(corrplot)
library(AER)

#Loading data
mathGrade = read.csv("student-mat.csv", sep = ";", stringsAsFactors = TRUE)
porGrade = read.csv("student-por.csv", sep = ";", stringsAsFactors = TRUE)

#Combination
gradeTotal = rbind(mathGrade, porGrade)

#Removing missing values if there are any
gradeTotal = na.omit(gradeTotal)

#To use the dataset variables without always using happiness$
attach(gradeTotal)

#Removing G1 + G2 because they're highly correlated, could be a source of bias, keeping relevant variables
cor(G3, G1)
cor(G3, G2)
gradeTotal = select(gradeTotal, c("famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "guardian", "famsup", "famrel", "G3"))

#Summary
head(gradeTotal)
str(gradeTotal)


#2) Visual inspection ----------------------------------------------------------
#-------------------------------------------------------------------------------

# Create a correlation plot
cols = sapply(gradeTotal, is.numeric)
correlations = cor(gradeTotal[, cols])
corrplot(correlations, method = "number")

#Histogram of grade
ggplot(gradeTotal, aes(x = G3)) +
  geom_histogram()

#histogram of Medu
ggplot(gradeTotal, aes(x = Medu)) +
  geom_histogram()

#famsize
ggplot(gradeTotal, aes(x = factor(famsize), y = G3)) +
  geom_boxplot()

#Pstatus
ggplot(gradeTotal, aes(x = factor(Pstatus), y = G3)) +
  geom_boxplot()
#Plot mother's education vs final grade
ggplot(gradeTotal, aes(x = factor(Medu), y = G3)) +
  geom_boxplot()

#Fedu
ggplot(gradeTotal, aes(x = factor(Fedu), y = G3)) +
  geom_boxplot()

#Mjob
ggplot(gradeTotal, aes(x = factor(Mjob), y = G3)) +
  geom_boxplot()

#Fjob
ggplot(gradeTotal, aes(x = factor(Fjob), y = G3)) +
  geom_boxplot()

#guardian
ggplot(gradeTotal, aes(x = factor(guardian), y = G3)) +
  geom_boxplot()
#Famsup vs G3
ggplot(gradeTotal, aes(x = factor(famsup), y = G3)) +
  geom_boxplot()

#famrel
ggplot(gradeTotal, aes(x = factor(famrel), y = G3)) +
  geom_boxplot()


#3) Regressions ----------------------------------------------------------------
#-------------------------------------------------------------------------------

#Basic regressions
#Most basic regression: G3 vs home
mod1 = lm(G3 ~ Medu, data = gradeTotal)
summary(mod1)

#2
mod2 = lm(G3 ~ Medu + Fedu + famrel, data = gradeTotal)
summary(mod2)

#3
mod3 = lm(G3 ~ Medu + Fedu + famrel + Mjob + Fjob, data = gradeTotal)
summary(mod3)

#4
mod4 = lm(G3 ~ Medu + Fedu + famrel + Mjob + Fjob + Pstatus + guardian + famsup + famsize, data = gradeTotal)
summary(mod4)

#5
mod5 = lm(G3 ~ Medu + Mjob + Medu * Mjob, data = gradeTotal)
summary(mod5)

#6
mod6 = lm(G3 ~ Medu + Fedu + famrel + Mjob + Fjob + Medu * Mjob + Pstatus + guardian + famsup + famsize, data = gradeTotal)
summary(mod6)

#7
mod7 = lm(G3 ~ Medu + I(Medu^2) + Fedu + famrel + Mjob + Fjob + Medu * Mjob + Pstatus + guardian + famsup + famsize, data = gradeTotal)
summary(mod7)


robSE = list(sqrt(diag(vcovHC(mod1, type = "HC1"))),
             sqrt(diag(vcovHC(mod2, type = "HC1"))),
             sqrt(diag(vcovHC(mod3, type = "HC1"))),
             sqrt(diag(vcovHC(mod4, type = "HC1"))),
             sqrt(diag(vcovHC(mod5, type = "HC1"))),
             sqrt(diag(vcovHC(mod6, type = "HC1"))),
             sqrt(diag(vcovHC(mod7, type = "HC1"))))

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7,
          se = robSE,

          digits = 3,
          header = F,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)", "(VII)"),
          type = "html",
          out = "Table.html")

#ANOVA
anova(mod4, mod6)

#4) Plotting results -----------------------------------------------------------
#-------------------------------------------------------------------------------

ggplot(gradeTotal, aes(x = Medu, y = G3, col = Mjob)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Mjob)
