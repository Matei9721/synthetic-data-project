library(readr)
library(performance)
library(gmodels)
library(caret)
library(tidyverse)

synthetic_sdv <- read.csv("synthetic_data/synthethicHalf.csv", header=T, stringsAsFactors=T)
View(synthetic_sdv)

indexes = sample(1:nrow(synthetic_sdv), size=0.5*nrow(synthetic_sdv)) # Random sample of 50% of row numbers created
Train50Syn <- synthetic_sdv[indexes,] # Training data contains created indices
Test50Syn <- synthetic_sdv[-indexes,] # Test data contains the rest

attach(Train50Syn)

LogisticModel50finalSyn <- glm(as.factor(Creditability) ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status, family=binomial, data = Train50Syn)
fit50Syn <- fitted.values(LogisticModel50finalSyn)

Threshold50Syn <- rep(0,500)
for (i in 1:1000)
  if(fit50Syn[i] >= 0.5) Threshold50Syn[i] <- 1

for (i in 1:500) {
  if (Threshold50Syn[i] == '1') {
    Threshold50Syn[i] <- 'Creditable'
  } 
}

for (i in 1:500) {
  if (Threshold50Syn[i] == '0') {
    Threshold50Syn[i] <- 'Non-Creditable'
  } 
}

for (i in 1:500) {
  if (Test50Syn$Creditability[i] == '0') {
    Test50Syn$Creditability[i] <- 'Non-Creditable'
  } 
}

for (i in 1:500) {
  if (Test50Syn$Creditability[i] == '1') {
    Test50Syn$Creditability[i] <- 'Creditable'
  } 
}

confusionSyn <- confusionMatrix(data = factor(Threshold50Syn), reference = factor(Test50Syn$Creditability))
confusionSyn