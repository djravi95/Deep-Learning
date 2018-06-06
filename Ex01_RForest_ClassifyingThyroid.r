
#####################RANDOM FOREST###############################

###Classifying Thyroid Function######################

###Today, it is understood that the thyroid gland secretes hormones which influence metabolic rate and protein synthesis. 
##The status of the thyroid can be assessed by laboratory tests. 
##The tests determine whether a patient's thyroid function is euthyroidism (normal), hypothyroidism (under active thyroid) or hyperthyroidism (overactive).

##In this section, we build Random Forests to classify thyroid function from laboratory tests.

####Step 1 - Collecting and Exploring the Data

##The thyroid data-frame in the mclust package was constructed from five laboratory tests
## administered to a sample of 215 patients. 
##The tests were used to predict whether a patient's thyroid function could be correctly classified.

####Let's load the data and take a look at the first few observations: 
data(thyroid, package="mclust") 
head (thyroid)
plot(thyroid[ , -1])
cor(thyroid$T3,thyroid$T4)
cor( thyroid$TSH, thyroid$DTSH)
       
#####Step 2 - Preparing the Data##########

###We will select 150 examples without replacement for the training set.
###The remainder set aside for the test set: 

set.seed (2018)
N = nrow (thyroid) 
train <- sample (1: N, 150, FALSE)

####Step 3 - Train Model using Train Set##########################

###The random Forest package can be used to build the model.
###In building the random forest model we have two primary options to choose. 
###The first is the number of trees; the second is the number of features to randomly select.
##We build a model with a forest of 800 trees: 

library (randomForest) 
num.trees =800
 fit <- randomForest(Diagnosis ~., data = thyroid [train,], ntree = num.trees, mtry =4)

#For each tree in the forest, a randomly selected subset of features is used to split each node. 
#This is controlled by the mytry parameter. 
 #In the above code we set mytry=4 to randomly select four of the six features.
 
 #####Step 4 - Evaluate Model Performance###############
 
 ####Details on the model can be viewed using the print statement:
 print( fit)
 
 
 ###########Test set performance#################################
 
 ######To assess how the model performed on the test set,
 ##we re-run it using the test set data:
 fit_test <- randomForest(Diagnosis ~., data =thyroid[- train,], ntree = num.trees, mtry =4) 
 print( fit_test)
 
 
###############Step 5 - Improving Model Performance################
 
 ###Step 5 - Improving Model Performance
 
fit2<-randomForest(Diagnosis ~., data =thyroid [train,], ntree = num.trees, importance = TRUE, mtry =2) 
 print( fit2)
 
##########Variable importance########
 
####We can describe our trained model fit2 in terms of the features by ranking them according to their splitting efficiency. This can be achieved via the varImpPlot function. 
### It returns two plots of variable importance, one using the average decrease in accuracy of including a feature, and the second using the average decrease in the Gini coefficient: 
varImpPlot (fit2)
 
 
 #########Step 6- Test set performance
 
####The lower training set error rate is encouraging. 
 #####Let's take a look at how fit2 performed with the test set data:
fit2_test<- randomForest(Diagnosis ~., data = thyroid [- train,], ntree = num.trees, mtry =2) 
 print( fit2_test)
 
 
 
 