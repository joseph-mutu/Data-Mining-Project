rm(list=ls())
source("DataProcessFunctions.R",encoding = "utf-8")
source("TestSetProcess.R",encoding = "utf-8")
source("writeResult.R",encoding = "utf-8")
source("test_functions.R",encoding = "utf-8")

require(parallel)
require(doParallel)
library("FactoMineR")
library("factoextra")
library(e1071)
library(arules)
library(glmnet)
library(neuralnet)
library(nnet)
library(rpart)
library(neuralnet)
library(rpart)
library(adabag)
library(ipred)  
library(randomForest)  
require(Hmisc)
require(caret)
library(xgboost)
n_Cores <- detectCores()##check the number of the clusters of the computer's CPU
cluster_Set <- makeCluster(n_Cores)
registerDoParallel(cluster_Set)
#=================================================Interpolate the data=============================
data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
# data_id = data.frame(data[,"id"])
#Interpolate based on the happiness
data_happiness_interpol = Happiness_inter(data)
dim(data_happiness_interpol)
#Interpolate based on 'Carpet' package
ptm = proc.time()
data_bag = Bag_inter(data)
# describe(data_bag)
proc.time() - ptm
#only use mean value to interpolate
data_mean = Mean_inter(data)


col_names_for_test = get_Col_names_For_test(data_happiness_interpol)
#=============================Using happiness to interpolate the training data，
#=============================Using mean value to interpolate the test_data ===================
test_set = getTestdata(col_names_for_test)
test_set = data.frame(scale(test_set,center=T,scale=T))
dim(test_set)
#====================================Get the data interpolated by carpet=============================
#====================================It takes time, so pre-store the data======================================================
complete_data = Get_complete_data_carpet() 
tem = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
tem = Outlier_delete(tem)
traindata_happiness = tem[,"happiness"]
#================================Using preposses to lower the dimension=========================
Process_data_pca  = preProcess(complete_data,method=c("scale","center","pca"))
complete_data = predict(Process_data_pca,complete_data)
dim(complete_data)
#=====================Using manul feature combine===================================================================
data_happiness = Combine_All_features(data_happiness_interpol)
dim(data_happiness)
data_happiness = central_scale(data_happiness)
data_bag = delete_features_for_linear(data_bag)
data_happiness_label = data.frame(data_happiness[,"happiness"])
dim(data_happiness_label)
data_happiness_delete_happ = subset(data_happiness,select = -c(happiness))
colnames(data_happiness_delete_happ)
dim(data_happiness)
#======================split the training set and the test set=========================================
train_data = complete_data[1:7988,]
train_data = data.frame(cbind(traindata_happiness,train_data))
colnames(train_data)[1] = "happiness"
test_data = complete_data[7989:10956,]


#=======================split training set into training data and test data===================

index = sample(2,nrow(train_data),replace = T,prob = c(0.8,0.2))
train_data_train = data.frame(train_data[index==1,])
train_data_test = data.frame(train_data[index==2,])
train_data_test = subset(train_data_test,select = -c(happiness))
train_data_test_label = data.frame(train_data[index==2,1])


#=======================function test=============================================
#SVM 
type_reg = "eps-regression"
type_reg2 = "nu-regression"
#===================================parameter tune(takes time)========================================
# tuned <- tune.svm(happiness ~ .,type = type_reg2,kernel ="radial",data = data_bag,gamma = 10^(-6:-1),cost = 10^(1:2))
# summary(tuned)
ptm = proc.time()
model_SVM = svm(happiness ~., data = train_data,type = type_reg2,kernel ="radial",gamma = 0.001,cost = 10) 
proc.time() - ptm
train_data_tem = data_happiness
train_data_tem = subset(train_data_tem,select = -c(happiness))


train_result = datapredict(model_SVM,test_data)
train_error = result_compare(data.frame(train_result),data.frame(data_happiness$happiness))
train_error

train_result = round(train_result)
writeResult(data.frame(train_result))
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]

#===================================
#lm 
ptm = proc.time()
Model_linear = lm(formula = happiness ~.,data = data_happiness)
proc.time() - ptm
summary(Model_linear)
lm_test_result = predict(Model_linear,train_data)
score = result_compare(data.frame(lm_test_result),data.frame(traindata_happiness))
score

#trainerror
train_result = predict(Model_linear,train_data_tem)
train_error = result_compare(data.frame(train_result),data.frame(data_happiness$happiness))
train_error
#===================================
#Random Forest
ptm = proc.time()
reg_RF = randomForest(happiness~.,data=train_data,importance=TRUE,proximity = TRUE,ntree=300)  
pred = predict(reg_RF,newdata=test_data)  
proc.time() - ptm


#train_error======================================================================================
train_data_tem = data_happiness
train_data_tem = subset(train_data_tem,select = -c(happiness))


reg_RF = randomForest(happiness~.,data=data_happiness,importance=TRUE,proximity = TRUE,ntree=300)  
pred = predict(reg_RF,newdata=train_data_tem)  
score = result_compare(data.frame(pred),data.frame(data_happiness$happiness))
score
#=======================================================================================

writeResult(data.frame(pred))
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]

#===================================
#Bagging
ptm = proc.time()
reg_boost =bagging(happiness~.,data=train_data,coob=TRUE,control=rpart.control(cp=0.025))  
result = predict(reg_boost,newdata = test_data)
proc.time() - ptm

#train_error================================================

reg_boost =bagging(happiness~.,data=data_happiness)  
train_result = predict(reg_boost,newdata = train_data)
score  = result_compare(data.frame(train_result),data.frame(data_happiness$happiness))
score
#================================================


writeResult(data.frame(result))
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]
#===================================
#NN
n = names(data_happiness)
f = as.formula(paste("happiness ~", paste(n[!n %in% "happiness"], collapse = " + ")))
ptm = proc.time()
nn = neuralnet(f,data=data_happiness,hidden=c(10,5),linear.output=T,stepmax = 1e+06,threshold = 0.05,err.fct = "sse")
proc.time() - ptm
dim(data_happiness)
dim(test_set)
net.predict = compute(nn,test_set)
result = data.frame(net.predict$net.result)
result = round(result)
writeResult(result)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]


#train error
train_data_tem = train_data
train_data_tem = subset(train_data_tem,select = -c(happiness))
train_tem = compute(nn,train_data_tem)
result = data.frame(train_tem$net.result)
result = round(result)
score = result_compare(result,data.frame(data_happiness$happiness))
score
dim(result)

#===================================================================================
#XGBoost
colnames(data_happiness)
train_data = data_happiness

train_data = subset(train_data,select = -c(happiness))

dim(data_happiness_delete_happ)
dim(test_set)

ptm = proc.time()
xgb <- xgboost(data = as.matrix(train_data),
                label = as.matrix(data_happiness$happiness),
                objective='reg:linear',nrounds=1000)
proc.time() - ptm

pre6 = data.frame(predict(xgb,as.matrix(test_set)))

score = result_compare(pre6,train_data_test_label)

writeResult(data.frame(pre6))
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]

#trainerror
pre6 = data.frame(predict(xgb,as.matrix(train_data)))
score = result_compare(pre6,train_data_test_label)


#===================================================================================
#SVM.province

# index = sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
# train_data = data.frame(data[index==1,])
# test_data = data.frame(data[index==2,])
# test_data = subset(test_data,select = -c(happiness))
# test_label = data.frame(data[index==2,1])
# test_id = data.frame(data_id[index==2,1])
#Bag Impute===============================
complete_data = Get_complete_data_carpet()
#happiness Impute=========================
complete_data = data_happiness

original_data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
original_data = Outlier_delete(original_data)
train_data_province = data.frame(original_data[,"province"])
train_id = data.frame(original_data[,"id"])
traindata_happiness = data.frame(original_data[,"happiness"])
colnames(train_data_province) = 'province'
colnames(traindata_happiness) = 'happiness'


complete_data = subset(complete_data,select = -c(province))


traindata = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
traindata = Outlier_delete(traindata)


traindata_happiness = traindata[,"happiness"]
train_id = traindata[,"id"]
test.tem = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv')
test_id = test.tem[,"id"]

Process_data_pca  = preProcess(complete_data,method=c("scale","center","pca"))
complete_data = predict(Process_data_pca,complete_data)

complete_data = cbind(traindata_happiness,complete_data,train_data_province)
complete_data = subset(complete_data,select = -c(happiness))

train_data = complete_data[1:7988,]
train_data = data.frame(cbind(traindata_happiness,train_data))
colnames(train_data)[1] = "happiness"
colnames(train_data)
test_data = complete_data[7989:10956,]

ptm = proc.time()
result_test = testData_province_SVM(train_data,test_data,test_id)
proc.time() - ptm


#train_error========================
result_test = testData_province_SVM(complete_data,complete_data,train_id)
re
score = result_compare(data.frame(result_test),data.frame(complete_data$happiness))
score 
#=========================
writeResult(result_test)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]
#===================================================================================
#LM.province
# index = sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
# train_data = data.frame(data[index==1,])
# test_data = data.frame(data[index==2,])
# test_data = subset(test_data,select = -c(happiness))
# test_label = data.frame(data[index==2,1])
# test_id = data.frame(data_id[index==2,1])
complete_data = Get_complete_data_carpet()
train_data_province = data.frame(complete_data[,"province"])
colnames(train_data_province) = 'province'
complete_data = subset(complete_data,select = -c(province))
traindata = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
traindata = Outlier_delete(traindata)
train_id = traindata[,'id']
traindata_happiness = traindata[,"happiness"]
test.tem = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv')
test_id = test.tem[,"id"]

Process_data_pca  = preProcess(complete_data,method=c("scale","center","pca"))
complete_data = predict(Process_data_pca,complete_data)
complete_data = cbind(complete_data,train_data_province)

train_data = complete_data[1:7988,]
train_data = data.frame(cbind(traindata_happiness,train_data))
colnames(train_data)[1] = "happiness"
colnames(train_data)
test_data = complete_data[7989:10956,]

ptm = proc.time()
result_test = testData_province_lm(train_data,train_data,train_id)
score = result_compare(data.frame(result_test),data.frame(traindata_happiness))
score
proc.time() - ptm

#train_error====================================================

result_test = testData_province_lm(complete_data,complete_data,train_id)
score = result_compare(data.frame(result_test),data.frame(complete_data$happiness))
score 
#=============================================================


writeResult(result_test)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]

score = result_compare(data.frame(result_test),test_label)

#================================get the test data and write the result================================================



SVM_test_result_1 = predict(model_SVM,test_set)
pre1 = predict(glm1,test_data)

writeResult(pre6)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]
#===============================================================================================


