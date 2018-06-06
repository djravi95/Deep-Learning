# example 9.1 of section 9.1.1 
# (example 9.1 of section 9.1.1)  : Exploring advanced methods : Using bagging and random forests to reduce training variance : Using bagging to improve prediction 
# Title: Preparing Spambase data and evaluating the performance of decision trees 

spamD <- read.table('E:/DATASETS_ALL/spamD.tsv',header=T,sep='\t')  	# Note: 1 
spamTrain <- subset(spamD,spamD$rgroup>=10)
spamTest <- subset(spamD,spamD$rgroup<10)

head(spamTrain)
head(spamTest)

spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"',   	# Note: 2 
                                paste(spamVars,collapse=' + '),sep=' ~ '))

loglikelihood <- function(y, py) {   	# Note: 3 
  pysmooth <- ifelse(py==0, 1e-12,
                     ifelse(py==1, 1-1e-12, py))
  
  sum(y * log(pysmooth) + (1-y)*log(1 - pysmooth))
}


accuracyMeasures <- function(pred, truth, name="model") {  	# Note: 4 
  dev.norm <- -2*loglikelihood(as.numeric(truth), pred)/length(pred)  	# Note: 5 
  ctable <- table(truth=truth,
                  pred=(pred>0.5))                                    	# Note: 6 
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/sum(ctable[2,])
  f1 <- precision*recall
  data.frame(model=name, accuracy=accuracy, f1=f1, dev.norm)
}


library(rpart)                                                  	# Note: 7 
treemodel <- rpart(spamFormula, spamTrain)

accuracyMeasures(predict(treemodel, newdata=spamTrain),  	# Note: 8 
                 spamTrain$spam=="spam",
                 name="tree, training")


accuracyMeasures(predict(treemodel, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="tree, test")

# Note 1: 
#   Load the data and split into training (90% of data) 
#   and test (10% of data) sets. 

# Note 2: 
#   Use all the features and do binary classification, 
#   where TRUE corresponds to spam documents. 

# Note 3: 
#   A function to calculate log likelihood (for 
#   calculating deviance). 

# Note 4: 
#   A function to calculate and return various measures 
#   on the model: normalized deviance, prediction accuracy, and f1, which is the 
#   product of precision and recall. 

# Note 5: 
#   Normalize the deviance by the number of data points 
#   so that we can compare the deviance across training and test 
#   sets. 

# Note 6: 
#   Convert the class probability estimator into a 
#   classifier by labeling documents that score greater than 0.5 as 
#   spam. 

# Note 7: 
#   Load the rpart library and fit a decision tree 
#   model. 

# Note 8: 
#   Evaluate the decision tree model against the 
#   training and test sets. 

##---------------------------
##----------------------------------------

# example 9.2 of section 9.1.1 
# (example 9.2 of section 9.1.1)  : Exploring advanced methods : Using bagging and random forests to reduce training variance : Using bagging to improve prediction 
# Title: Bagging decision trees 

ntrain <- dim(spamTrain)[1]
n <- ntrain                  	# Note: 1 
ntree <- 100

samples <- sapply(1:ntree,      	# Note: 2 
                  FUN = function(iter)
                  {sample(1:ntrain, size=n, replace=T)})

treelist <-lapply(1:ntree,       	# Note: 3 
                  FUN=function(iter)
                  {samp <- samples[,iter];
                  rpart(spamFormula, spamTrain[samp,])})

predict.bag <- function(treelist, newdata) {  	# Note: 4 
  preds <- sapply(1:length(treelist),
                  FUN=function(iter) {
                    predict(treelist[[iter]], newdata=newdata)})
  predsums <- rowSums(preds)
  predsums/length(treelist)
}

accuracyMeasures(predict.bag(treelist, newdata=spamTrain),  	# Note: 5 
                 spamTrain$spam=="spam",
                 name="bagging, training")


accuracyMeasures(predict.bag(treelist, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="bagging, test")

# Note 1: 
#   Use bootstrap samples the same size as the training 
#   set, with 100 trees. 

# Note 2: 
#   Build the bootstrap samples by sampling the row indices of spamTrain with replacement. Each 
#   column of the matrix samples represents the row indices into spamTrain 
#   that comprise the bootstrap sample. 

# Note 3: 
#   Train the individual decision trees and return them 
#   in a list. Note: this step can take a few minutes. 

# Note 4: 
#   predict.bag assumes the underlying classifier returns decision probabilities, not 
#   decisions. 

# Note 5: 
#   Evaluate the bagged decision trees against the 
#   training and test sets. 
#-----------------------------
#------------------------------

# example 9.3 of section 9.1.2 
# (example 9.3 of section 9.1.2)  : Exploring advanced methods : Using bagging and random forests to reduce training variance : Using random forests to further improve prediction 
# Title: Using random forests 

library(randomForest)           	# Note: 1 
set.seed(5123512) 	# Note: 2 
fmodel <- randomForest(x=spamTrain[,spamVars], 	# Note: 3 
                       y=spamTrain$spam,
                       ntree=100, 	# Note: 4 
                       nodesize=7, 	# Note: 5 
                       importance=T) 	# Note: 6 
accuracyMeasures(predict(fmodel, 	# Note: 7 
                         newdata=spamTrain[,spamVars],type='prob')[,'spam'],
                 spamTrain$spam=="spam",name="random forest, train")
##                  model  accuracy        f1  dev.norm
## 1 random forest, train 0.9884142 0.9706611 0.1428786
accuracyMeasures(predict(fmodel,
                         newdata=spamTest[,spamVars],type='prob')[,'spam'],
                 spamTest$spam=="spam",name="random forest, test")
##                 model  accuracy        f1  dev.norm
## 1 random forest, test 0.9541485 0.8845029 0.3972416

# Note 1: 
#   Load the randomForest package. 

# Note 2: 


#   Set the pseudo-random seed to a known value to try 
#   and make the random forest run repeatable. 

# Note 3: 
#   Call the randomForest() function to build the model 
#   with explanatory variables as x and the category to be predicted as 
#   y. 

# Note 4: 
#   Use 100 trees to be compatible with our bagging 
#   example. The default is 500 trees. 

# Note 5: 
#   Specify that each node of a tree must have a minimum 
#   of 7 elements, to be compatible with the default minimum node size that rpart() 
#   uses on this training set. 

# Note 6: 
#   Tell the algorithm to save information to be used for 
#   calculating variable importance (we'll see this later). 

# Note 7: 
#   Report the model quality. 
#------------------------------
#---------------------------------

# example 9.4 of section 9.1.2 
# (example 9.4 of section 9.1.2)  : Exploring advanced methods : Using bagging and random forests to reduce training variance : Using random forests to further improve prediction 
# Title: randomForest variable importances 

varImp <- importance(fmodel)              	# Note: 1 

varImp[1:10, ]                           	# Note: 2 


varImpPlot(fmodel, type=1)                       	# Note: 3

# Note 1: 
#   Call importance() on the spam 
#   model. 

# Note 2: 
#   The importance() function returns a matrix of 
#   importance measures (larger values = more important). 

# Note 3: 
#   Plot the variable importance as measured by 
#   accuracy change. 

#------------------------------------
#------------------------------------------

# example 9.5 of section 9.1.2 
# (example 9.5 of section 9.1.2)  : Exploring advanced methods : Using bagging and random forests to reduce training variance : Using random forests to further improve prediction 
# Title: Fitting with fewer variables 

selVars <- names(sort(varImp[,1], decreasing=T))[1:25] 	# Note: 1 

fsel <- randomForest(x=spamTrain[,selVars],y=spamTrain$spam, 	# Note: 2 
                     ntree=100,
                     nodesize=7,
                     importance=T)

accuracyMeasures(predict(fsel,
                         newdata=spamTrain[,selVars],type='prob')[,'spam'],
                 spamTrain$spam=="spam",name="RF small, train")
##             model  accuracy        f1  dev.norm
## 1 RF small, train 0.9876901 0.9688546 0.1506817

accuracyMeasures(predict(fsel,
                         newdata=spamTest[,selVars],type='prob')[,'spam'],
                 spamTest$spam=="spam",name="RF small, test")
##            model  accuracy        f1 dev.norm
## 1 RF small, test 0.9497817 0.8738142 0.400825

# Note 1: 
#   Sort the variables by their importance, as 
#   measured by accuracy change. 

# Note 2: 
#   Build a random forest model using only the 25 
#   most important variables. 













