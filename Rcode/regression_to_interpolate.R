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

#以回归进行插值补充，将test_set 以及train_data 结合在一起进行处理
s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
dataset = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv")
data = Outlier_delete(s)
data_label = data[,"happiness"]
test_id = 8001
data = subset(data,select = -c(happiness))
new_data_with_id = data.frame(rbind(data,dataset))
new_data = new_data_with_id()
