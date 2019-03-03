rm(list=ls())
source("DataProcessFunctions.R")
source("TestSetProcess.R")
source("writeResult.R")
source("test_functions.R")

library("FactoMineR")
library("factoextra")
library(e1071)
library(arules)
library(glmnet)
library("neuralnet")
library(nnet)
library(rpart)
library(neuralnet)
library(rpart)
library(adabag)
library(ipred)  
library(randomForest)  



s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = Outlier_delete(s)
data_id = data.frame(data[,"id"])
data = Interpolate_ALL(data)
col_names_for_test = get_Col_names_For_test(data)
data = Combine_All_features(data)
data = central_scale(data)
data_features = data
data = delete_features_for_linear(data)
dim(data)
# data = Province_centra_scale(data)
index = sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train_data = data.frame(data[index==1,])
test_data = data.frame(data[index==2,])
test_data = subset(test_data,select = -c(happiness))
test_label = data.frame(data[index==2,1])
test_id = data.frame(data_id[index==2,1])

reg<-bagging(happiness~.,data=train_data,coob=TRUE,control=rpart.control(cp=0.025))  
result = predict(reg,newdata = test_data)
result
score = result_compare(data.frame(result),test_label)
score



data = data_features
reg_RF<-randomForest(happiness~.,data=train_data,importance=TRUE,ntree=400)  
print(reg)
pred<-predict(reg_RF,newdata=test_data)  


