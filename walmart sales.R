library(ggplot2)
library(e1071)
store=read.table("G:/R programming/Rfiles/stores.csv",header = TRUE,sep = ",")
head(store)
tail(store)
dim(store)
str(store)
class(store)
index=1:nrow(store)
testindex=sample(index,trunc(length(index)/3))
train_store=store[testindex,]
head(train_store)
test_store=store[-testindex,]
head(test_store)

model.store=lm(Size~Type,data = train_store)
summary(model.store)
model.store
pred.store=predict(model.store,data=test_store,type="response")
summary(pred.store)
pred.store
