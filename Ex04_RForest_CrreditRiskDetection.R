

####RANDOM FOREST -EXAMPLES

###Credit Risk Detection and Prediction###########

###We will now start our analytics pipeline by loading the necessary dependencies: 
library(randomForest)  #rf model 
library(caret) 	# feature selection
library(e1071)	 # model tuning
library(ROCR) 	# model evaluation
source("performance_plot_utils.R")	 # plot curves


####### load in the data and attach the data frame

credit.df <- read.csv("E:/DATASETS_ALL/credit.csv", header = TRUE, sep = ",") 

# class should be data.frame
 class(credit.df)

# get a quick peek at the data
 head(credit.df)

# get dataset detailed info
 str(credit.df)

 #### split data into training and test datasets in 60:40 ratio
 
indexes <- sample(1:nrow(credit.df), size=0.6*nrow(credit.df))
 
train.data <- credit.df[indexes,]
str(train.data)
 
test.data <- credit.df[-indexes,]
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

 
###Build training Model

####Next, we will build the initial training model with all the features as follows: 
formula.init <- credit.rating ~ .
 formula.init <- as.formula(formula.init)
rf.model <- randomForest(credit_history ~ ., data = train.data,importance=T, proximity=T)
# You can view the model details by using the following code:
 print(rf.model)

##This gives us information about the out of bag error (OOBE), 
##which is around 23%, and the confusion matrix which is calculated on the training data,
###and also how many variables it is using at each split.

 #Next, we will perform predictions using this model on the test data and evaluate them:
 
rf.predictions <- predict(rf.model, test.feature.vars, type="class")
 
confusionMatrix(data=rf.predictions, reference=test.class.var,   positive="1")
 
#The initial model yields quite decent results. We see that a fair amount of bad credit rating customers are classified as bad and most of the good rating based customers are rated as good.
 
