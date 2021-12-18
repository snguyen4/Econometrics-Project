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

# #Mom at home
# gradeTotal$Mjob = ifelse(Mjob == "at_home", 1, 0)
# 
# #Dad at home
# gradeTotal$Fjob = ifelse(Fjob == "at_home", 1 , 0)

#Removing G1 + G2 because they're highly correlated, could be a source of bias, keeping relevant variables
cor(G3, G1)
cor(G3, G2)
gradeTotal = select(gradeTotal, c("sex", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "guardian", "famsup", "famrel", "higher", "G3"))

# #Parents at home or not
# gradeTotal$home = ifelse(Mjob == "at_home" | Fjob == "at_home", 1, 0)

#Summary
head(gradeTotal)
str(gradeTotal)


#2) Visual inspection ----------------------------------------------------------
#-------------------------------------------------------------------------------
#Correlation plot
# Create a correlation plot
cols = sapply(gradeTotal, is.numeric)
correlations = cor(gradeTotal[, cols])
corrplot(correlations, method = "number")

#Histogram of grade
hist(G3, col = "orange")

#sex
boxplot(G3 ~ sex)

#famsize
boxplot(G3 ~ famsize)

#Pstatus
boxplot(G3 ~ Pstatus)

#Plot mother's education vs final grade
boxplot(G3 ~ Medu)

#Fedu
boxplot(G3 ~ Fedu)

#Mjob
boxplot(G3 ~ Mjob)

#Fjob
boxplot(G3 ~ Fjob)

#guardian
boxplot(G3 ~ guardian)

#Famsup vs G3
boxplot(G3 ~ famsup)

#famrel
boxplot(G3 ~ famrel)

#hgiher
boxplot(G3 ~ higher)

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
mod4 = lm(G3 ~ Medu + Fedu + famrel + Mjob + Fjob + Pstatus + guardian + famsup + higher + famsize, data = gradeTotal)
summary(mod4)

#5
mod5 = lm(G3 ~ Medu + Mjob + Medu * Mjob, data = gradeTotal)
summary(mod5)

#6
mod6 = lm(G3 ~ Medu + Fedu + famrel + Mjob + Fjob + Medu * Mjob + Pstatus + guardian + famsup + higher + famsize, data = gradeTotal)
summary(mod6)

#7
mod7 = lm(G3 ~ Medu + I(Medu^2) + Fedu + famrel + Mjob + Fjob + Medu * Mjob + Pstatus + guardian + famsup + higher + famsize, data = gradeTotal)
summary(mod7)

#8
mod8 = lm(G3 ~ Medu + Mjob + Medu * higher, data = gradeTotal)
summary(mod8)

#9
mod9 = lm(G3 ~ Medu + Fedu + famrel + Mjob + Fjob + Pstatus + guardian + famsup + higher + Medu * higher + famsize, data = gradeTotal)
summary(mod9)

robSE = list(sqrt(diag(vcovHC(mod1, type = "HC1"))),
             sqrt(diag(vcovHC(mod2, type = "HC1"))),
             sqrt(diag(vcovHC(mod3, type = "HC1"))),
             sqrt(diag(vcovHC(mod4, type = "HC1"))),
             sqrt(diag(vcovHC(mod5, type = "HC1"))),
             sqrt(diag(vcovHC(mod6, type = "HC1"))),
             sqrt(diag(vcovHC(mod7, type = "HC1"))),
             sqrt(diag(vcovHC(mod8, type = "HC1"))),
             sqrt(diag(vcovHC(mod9, type = "HC1"))))

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,
          se = robSE,
          digits = 3,
          header = F,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)", "(VII)", "(VIII)", "(IX)"),
          type = "html",
          out = "Table.html")


