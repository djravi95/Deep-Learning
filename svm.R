setwd("G:/R programming/Rfiles")
getwd()
challenge=read.csv("G:/R programming/Rfiles/challenger.csv")
head(challenge)
str(challenge)
library(C50)
library(e1071)
set.seed(25)
index<-1:nrow(challenge)
testindex=sample(index,trunc(length(index)/3))
train_challenge=sample(challenge[testindex,])
head(train_challenge)
test_challenge=sample(challenge[-testindex,])
head(test_challenge)

svm_model=svm(distress_ct~temperature + pressure,data = train_challenge)
summary(svm_model)
pred=predict(svm_model,data=test_challenge)
summary(pred)
pred













