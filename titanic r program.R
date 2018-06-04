library(randomForest)

titanic.train=read.csv("G:/R programming/Rfiles/train.csv",stringsAsFactors = FALSE,header = TRUE)
head(titanic.train)
tail(titanic.train)
titanic.test=read.csv("G:/R programming/Rfiles/test.csv",stringsAsFactors = FALSE,header = TRUE)
head(titanic.test)
tail(titanic.test)
str(titanic.test)
median(titanic.train$Age,na.rm = TRUE)
titanic.train$istrain<-TRUE
tail(titanic.train$istrain)
titanic.test$istrain<-FALSE
ncol(titanic.train)
ncol(titanic.test)

titanic.test$Survived<- NA
tail(titanic.test$Survived)
tail(titanic.train$Survived)

titanic.full=rbind(titanic.train,titanic.test)
head(titanic.full)
tail(titanic.full)
age.median<-median(titanic.full$Age,na.rm = TRUE)
titanic.full[titanic.full$Embarked=="","Embarked"]<-'S'
titanic.full[is.na(titanic.full$Age),"Age"]<-age.median

fare.median=median(titanic.full$Fare,na.rm=TRUE)
titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.median

titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)


titanic.train<-titanic.full[titanic.full$istrain==TRUE,]
titanic.test<-titanic.full[titanic.full$istrain==FALSE,]


titanic.train1$Survived<-as.factor(titanic.train1$Survived)

str(titanic.train1)
model=randomForest(Survived~Pclass + Sex + Age + SibSp + Fare + Embarked ,data = titanic.train,ntree=500,mtry=3,nodesize=0.01*nrow(titanic.test))
summary(model)
model
pred=predict(model,newdata=titanic.test)
pred
PassengerID<-titanic.test$PassengerId
output.df<-as.data.frame(PassengerID)
output.df$Survived<-pred
output.df
output.df$Survived=round(output.df$Survived,0)
output.df
write.csv(output.df,"Kaggle_submission.csv")
getwd()
