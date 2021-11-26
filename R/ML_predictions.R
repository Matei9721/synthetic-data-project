library(readr)
library(performance)
library(gmodels)
library(caret)
library(tidyverse)
synthethic <- read_csv("synthethic.csv")
View(synthethic)

german_credit <- read_csv("german_credit.csv")
View(german_credit)

Train50 <- read_csv("Training50.csv")
View(Train50)

Test50 <- read_csv("Test50.csv")
View(Test50)

SyntheticData <- read_csv("synthethicHalf.csv")
View(SyntheticData)

attach(german_credit)
#summary(german_credit$`Duration of Credit (month)`)
#hist(german_credit$`Duration of Credit (month)`, breaks=brksCredit, xlab = "Credit Month", ylab = "Frequency", main = " ", cex=0.4) # produces nice looking histogram


# Trying to run some model with 50:50 cross validation
indexes = sample(1:nrow(SyntheticData), size=0.5*nrow(SyntheticData)) # Random sample of 50% of row numbers created
Train50Syn <- SyntheticData[indexes,] # Training data contains created indices
Test50Syn <- SyntheticData[-indexes,] # Test data contains the rest
# Using any proportion, other than 0.5 above and size Training and Test data can be constructed

LogisticModel50 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Value.Savings.Stocks + Length.of.current.employment + Sex...Marital.Status + Most.valuable.available.asset + Type.of.apartment + Concurrent.Credits + Duration.of.Credit..month.+ Credit.Amount + Age..years., family=binomial, data = Train50)
LogisticModel50final <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status, family=binomial, data = Train50)
fit50 <- fitted.values(LogisticModel50final)
Threshold50 <- rep(0,500)
for (i in 1:500)
  if(fit50[i] >= 0.5) Threshold50[i] <- 1
CrossTable(Train50$Creditability, Threshold50, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=F, data=Train50)


for (i in 1:500) {
  if (Threshold50[i] == '1') {
    Threshold50[i] <- 'Creditable'
  } 
}

for (i in 1:500) {
  if (Threshold50[i] == '0') {
    Threshold50[i] <- 'Non-Creditable'
  } 
}

for (i in 1:500) {
  if (Test50$Creditability[i] == '0') {
    Test50$Creditability[i] <- 'Non-Creditable'
  } 
}

for (i in 1:500) {
  if (Test50$Creditability[i] == '1') {
    Test50$Creditability[i] <- 'Creditable'
  } 
}
confusion <- confusionMatrix(data = factor(Threshold50), reference = factor(Test50$Creditability))
confusion

detach(Train50)
attach(Train50Syn)

########### Same things on the synthtetic data
LogisticModel50finalSyn <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status, family=binomial, data = Train50Syn)
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
