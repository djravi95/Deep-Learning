# -*- coding: utf-8 -*-
"""
Created on Sat Jun 16 22:51:02 2018

@author: G702394
"""
import pandas as pd

Data = pd.read_csv("LGD_DATA.csv")

Data.head()

Data.info()

Data.shape 

#Return a list with the row axis labels and column axis labels as members in order.
Data.axes
  
Data.describe()



Data[Data.duplicated(keep= False)]
len(Data[Data.duplicated(keep= False)])

Data = Data.drop_duplicates(keep='first')
len(Data[Data.duplicated(keep= False)])

len(Data)

Data[Data.duplicated(subset = ['Ac_No'],  keep= False)]
len(Data[Data.duplicated(keep= False)])

set(Data['Gender'])
set(Data['Married'])


Data.groupby('Gender')['Ac_No'].count().reset_index(name = "Count")

Data[Data['Gender'] == 'm']['Gender'] 

Data["Gender"].replace(["m"], "M", inplace=True)
Data.groupby('Gender')['Ac_No'].count().reset_index(name = "Count")

Data["Gender"].replace(["f"], "F", inplace=True)
Data.groupby('Gender')['Ac_No'].count().reset_index(name = "Count")

Data["Gender"].replace(["Z",  "v"], float('nan'), inplace=True)
Data.groupby('Gender')['Ac_No'].count().reset_index(name = "Count")

set(Data['Gender'])
Data[Data.duplicated(keep= False)]

Data = Data.drop_duplicates(keep='first')
len(Data[Data.duplicated(keep= False)])



#outlier treatment
import matplotlib.pyplot as plt

Data.columns
plt.scatter(Data['Age'], Data['Losses in Thousands'])
plt.scatter(Data['Years of Experience'], Data['Losses in Thousands'])
plt.scatter(Data['Number of Vehicles'], Data['Losses in Thousands'])

q = Data["Age"].quantile(0.99)
q


Data = Data[ (Data["Age"] <= q) | (Data["Age"].isnull())]
plt.scatter(Data['Age'], Data['Losses in Thousands'])

q = Data["Years of Experience"].quantile(0.99)
q
Data = Data[(Data["Years of Experience"] <= q) | (Data["Years of Experience"].isnull())]
plt.scatter(Data["Years of Experience"], Data['Losses in Thousands'])


q = Data['Number of Vehicles'].quantile(0.99)
q
Data = Data[(Data['Number of Vehicles'] <= q) | (Data["Number of Vehicles"].isnull())]
plt.scatter(Data['Number of Vehicles'], Data['Losses in Thousands'])

Data.describe()

#missing value treatment
Data[Data.isnull().any(axis=1)]

Data = Data.drop('Ac_No', axis = 1)

Data[Data.isnull().any(axis=1)]

Data = Data.drop([132, 129], axis = 0)

Data[Data.isnull().any(axis=1)]

Data['Years of Experience'].mean()
m = Data['Years of Experience'].mean()
Data = Data.fillna({'Gender' : 'F', 'Years of Experience' : m})

Data[Data.isnull().any(axis=1)]


Data.columns
Data = Data.rename(index=str, columns={"Years of Experience": "YOE", "Number of Vehicles": "NOV", "Losses in Thousands" : "LIT"})
Data.columns

#Linear Regression
Data.corr()
import statsmodels.formula.api as sm

result = sm.ols(formula="Losses in Thousands  ~ Age", data = Data).fit()

Data.columns
Data = Data.rename(index=str, columns={"Years of Experience": "YOE", "Number of Vehicles": "NOV", "Losses in Thousands" : "LIT"})
Data.columns


result = sm.ols(formula="LIT  ~ Age + NOV + Gender + Married", data = Data).fit()

print (result.params)
print (result.summary())

result = sm.ols(formula="LIT  ~ Age + YOE + Gender + Married", data = Data).fit()
print (result.summary())

result = sm.ols(formula="LIT  ~ Age + Gender + Married", data = Data).fit()
print (result.summary())

result.predict(Data[['Age', 'Gender',  'Married']])


#Logistic Regression
Data.sample(3)
Data.sample(3,random_state=1)


train_data = Data.sample(frac=0.8,random_state=1)
test_data = Data.drop(train_data.index)

train_data.columns
test_data.columns

len(Data)
len(train_data) + len(test_data)

train_data.groupby(['Defaulter_Flag'])['Defaulter_Flag'].count()
test_data.groupby(['Defaulter_Flag'])['Defaulter_Flag'].count()

train_data.groupby(['Gender', 'Defaulter_Flag'])['Defaulter_Flag'].count()
train_data.groupby(['Gender', 'Defaulter_Flag'])['Defaulter_Flag'].count()


table=pd.crosstab(train_data.Gender, train_data.Defaulter_Flag)
table.div(table.sum(1).astype(float), axis=0).plot(kind='bar', stacked=True)
plt.title('Stacked Bar Chart of Gender vs Defaulter_Flag')
plt.xlabel('Gender')
plt.ylabel('Proportion of Defaulter_Flag')
plt.savefig('Gender_vs_Defaulter_Flag')


pd.crosstab(train_data.Gender,train_data.Defaulter_Flag).plot(kind='bar')
plt.title('Stacked Bar Chart of Gender vs Defaulter_Flag')
plt.xlabel('Gender')
plt.ylabel('Number of Defaulter_Flag')
plt.savefig('Gender_vs_Defaulter_Flag')


table=pd.crosstab(train_data.Married,train_data.Defaulter_Flag)
table.div(table.sum(1).astype(float), axis=0).plot(kind='bar', stacked=True)
plt.title('Bar Chart of Married vs Defaulter_Flag')
plt.xlabel('Married')
plt.ylabel('Proportion of Defaulter_Flag')
plt.savefig('Married_vs_Defaulter_Flag')

pd.crosstab(train_data.Married,train_data.Defaulter_Flag).plot(kind='bar')
plt.title('Bar Chart of Married vs Defaulter_Flag')
plt.xlabel('Gender')
plt.ylabel('Number of Defaulter_Flag')
plt.savefig('Gender_vs_Defaulter_Flag')


from sklearn.linear_model import LogisticRegression

train_data.columns

X = train_data.loc[: , 'Age': 'Married']
y = train_data.loc[: , 'Defaulter_Flag']


model = LogisticRegression()
model = model.fit(X,y)


import statsmodels.formula.api as smf
train_data.columns 

logitfit = smf.logit(formula = 'Defaulter_Flag ~ Age + YOE + Gender + Married', data = train_data).fit()

logitfit.summary()       # summary of the model
logitfit.predict()            # predict
logitfit.pred_table()          # confusion matrix


threshold = 0.5

predicted = logitfit.predict(test_data.loc[: , 'Age': 'Married']) 

predicted_choice = (predicted > threshold).astype(int)

Confusion_matrix = pd.crosstab(test_data.Defaulter_Flag, predicted_choice, rownames=['Defaulter_Flag_count'], colnames=["Predicted_count"])
Confusion_matrix
Accuracy = (Confusion_matrix[0][0] + Confusion_matrix[1][1])/ (Confusion_matrix[0][0] + Confusion_matrix[1][1] + Confusion_matrix[0][1] + Confusion_matrix[1][0])
Accuracy

logitfit.aic

