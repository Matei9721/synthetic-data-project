library(readr)
library(performance)
library(gmodels)
library(caret)
library(tidyverse)


# Add data to R and attach it to the global environment

german_credit <- read_csv("data/german_credit.csv")
View(german_credit)

Train50 <- read_csv("data/Training50.csv")
View(Train50)

Test50 <- read_csv("data/Test50.csv")
View(Test50)

attach(Train50)


# Logistic regression model with 50:50 Cross-validation

LogisticModel50 <- glm(Creditability ~ Account.Balance + 
                         Payment.Status.of.Previous.Credit + Purpose + 
                         Value.Savings.Stocks + Length.of.current.employment + 
                         Sex...Marital.Status + Most.valuable.available.asset +
                         Type.of.apartment + Concurrent.Credits + 
                         Duration.of.Credit..month.+ Credit.Amount + 
                         Age..years., family=binomial, data = Train50)

LogisticModel50final <- glm(Creditability ~ Account.Balance +
                              Payment.Status.of.Previous.Credit + Purpose + 
                              Length.of.current.employment + 
                              Sex...Marital.Status, family=binomial, 
                            data = Train50)

fit50 <- fitted.values(LogisticModel50final)

Threshold50 <- rep(0,500)

for (i in 1:500)
  if(fit50[i] >= 0.5) Threshold50[i] <- 1
CrossTable(Train50$Creditability, Threshold50, digits=1, prop.r=F, prop.t=F, 
           prop.chisq=F, chisq=F, data=Train50)


for (i in 1:500) {
  if (Threshold50[i] == '1') {
    Threshold50[i] <- 'Creditable'
  }
  
  if (Threshold50[i] == '0') {
    Threshold50[i] <- 'Non-Creditable'
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

confusion <- confusionMatrix(data = factor(Threshold50),
                             reference = factor(Test50$Creditability))
confusion





# Tree based method

Test50 <- read_csv("data/Test50.csv")

library(tree)

Train50_tree <- tree(as.factor(Creditability) ~ Account.Balance+
                       Duration.of.Credit..month.+
                       Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+
                       Value.Savings.Stocks+
                       Length.of.current.employment+Instalment.per.cent+
                       Sex...Marital.Status+Guarantors+
                       Duration.in.Current.address+
                       Most.valuable.available.asset+Age..years.+
                       Concurrent.Credits+Type.of.apartment+
                       No.of.Credits.at.this.Bank+Occupation+No.of.dependents+
                       Telephone, data=Train50, method="class")
summary(Train50_tree)
plot(Train50_tree)
text(Train50_tree, pretty=0,cex=0.6)
Test50_pred <- predict(Train50_tree, Test50, type="class")
table(Test50_pred, Test50$Creditability)
Train50_prune8 <- prune.misclass(Train50_tree, best=8)
Test50_prune8_pred <- predict(Train50_prune8, Test50, type="class")
table(Test50_prune8_pred, Test50$Creditability)

confusionTree <- confusionMatrix(data = factor(Test50_prune8_pred),
                                 reference = factor(Test50$Creditability))
confusionTree


# Random Forest

library(randomForest)

rf50 <- randomForest(as.factor(Creditability) ~., data = Train50, ntree=200, importance=T, proximity=T)
plot(rf50, main="")
rf50
Test50_rf_pred <- predict(rf50, Test50, type="class")
table(Test50_rf_pred, Test50$Creditability)
importance(rf50)

confusionForest <- confusionMatrix(data = factor(Test50_rf_pred),
                                   reference = factor(Test50$Creditability))
confusionForest

detach(Train50)
