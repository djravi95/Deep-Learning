library(e1071)
library(gplot2)
iris=read.table("G:/R programming/Rfiles/iris.txt",sep=",",quote = "\t")
ggpairs(iris)
head(iris)
tail(iris)
plot(iris$V4~iris$V3)
cor(iris$V4,iris$V3)
sapply(iris,class)
set.seed(225)
index<-1:nrow(iris)
testindex=sample(index,trunc(length(index)/3))
train_iris=iris[testindex,]
head(train_iris)
test_iris=iris[-testindex,]
head(test_iris)
model.log=glm(V5~V1,data=train_iris,family = binomial("logit"))
summary(model.log)

pred.iris=predict(model.iris,data=test_iris,type = "response")
summary(pred.iris)
pred.iris

