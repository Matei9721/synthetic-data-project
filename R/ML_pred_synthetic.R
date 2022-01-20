library(readr)
library(performance)
library(gmodels)
library(caret)
library(tidyverse)

# Load data into the environment and creates partition for 50-50 validation

synthetic_sdv <- read.csv("synthetic_data/synthethicHalf.csv", header=T,
                          stringsAsFactors=T)

View(synthetic_sdv)

Train50 <- read_csv("data/Training50.csv")


# Random sample of 50% of row numbers created
indexes = sample(1:nrow(synthetic_sdv), size=0.5*nrow(synthetic_sdv)) 
# Training data contains created indices
Train50Syn <- synthetic_sdv[indexes,] 
# Test data contains the rest
Test50Syn <- synthetic_sdv[-indexes,]



Test50 <- read_csv("data/Test50.csv")

attach(Train50Syn)

# Logistic regression model with 50:50 Cross-validation

LogisticModel50finalSyn <- glm(as.factor(Creditability) ~ Account.Balance +
                                 Payment.Status.of.Previous.Credit + Purpose +
                                 Length.of.current.employment + 
                                 Sex...Marital.Status, family=binomial,
                               data = Train50Syn)

fit50Syn <- fitted.values(LogisticModel50finalSyn)

Threshold50Syn <- rep(0,500)

for (i in 1:500)
  if(fit50Syn[i] >= 0.5) Threshold50Syn[i] <- 1

for (i in 1:500) {
  if (Threshold50Syn[i] == '1') {
    Threshold50Syn[i] <- 'Creditable'
  }
  
  if (Threshold50Syn[i] == '0') {
    Threshold50Syn[i] <- 'Non-Creditable'
  } 
}

for (i in 1:500) {
  if (Test50$Creditability[i] == '0') {
    Test50$Creditability[i] <- 'Non-Creditable'
  } 
  
  if (Test50$Creditability[i] == '1') {
    Test50$Creditability[i] <- 'Creditable'
  } 
}

confusionSyn <- confusionMatrix(data = factor(Threshold50Syn), reference = factor(Test50$Creditability))
confusionSyn


# Tree based method

library(tree)

Test50Syn <- synthetic_sdv[-indexes,]
Test50 <- read_csv("data/Test50.csv")

Train50_tree <- tree(as.factor(Creditability) ~ Account.Balance+
                       Duration.of.Credit..month.+
                       Payment.Status.of.Previous.Credit+Purpose+Credit.Amount
                     +Value.Savings.Stocks+Length.of.current.employment+
                       Instalment.per.cent+Sex...Marital.Status+Guarantors+
                       Duration.in.Current.address+
                       Most.valuable.available.asset+Age..years.+
                       Concurrent.Credits+Type.of.apartment+
                       No.of.Credits.at.this.Bank+Occupation+No.of.dependents+
                       Telephone, data=Train50Syn, method="class")
summary(Train50_tree)
plot(Train50_tree)
text(Train50_tree, pretty=0,cex=0.6)
Test50_pred <- predict(Train50_tree, Test50Syn, type="class")
table(Test50_pred, Test50Syn$Creditability)
Train50_prune8 <- prune.misclass(Train50_tree, best=8)
Test50_prune8_pred <- predict(Train50_prune8, Test50, type="class")
table(Test50_prune8_pred, Test50$Creditability)

confusionTree <- confusionMatrix(data = factor(Test50_prune8_pred),
                                 reference = factor(Test50$Creditability))
confusionTree


# Random Forest

library(randomForest)

rf50 <- randomForest(as.factor(Creditability) ~., data = Train50Syn, ntree=200, importance=T, proximity=T)
plot(rf50, main="")
rf50
Test50_rf_pred <- predict(rf50, Test50, type="class")
table(Test50_rf_pred, Test50$Creditability)
importance(rf50)

confusionForest <- confusionMatrix(data = factor(Test50_rf_pred),
                                   reference = factor(Test50$Creditability))
confusionForest

detach(Train50Syn)