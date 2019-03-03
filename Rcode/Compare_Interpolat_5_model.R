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
n_Cores <- detectCores()##检测你的电脑的CPU核数
cluster_Set <- makeCluster(n_Cores)##进行集群
registerDoParallel(cluster_Set)
#Interpolate_all
#1. 合并 invest1_8,只要存在一种就认为存在 invest 行为
#2. 删除异常值太多，存在中文，调查时间等特征
  # id,edu_yr,s_work_type,
  # s_work_status,invest_other,work_manage,
  # work_type,work_yr,work_status,join_party,
  # edu_other,city,county,survey_time,
  # property_other,invest_0,invest_1,invest_2,
  # invest_3,invest_4,invest_5,invest_6,invest_7,
  # invest_8,invest_other
#3. 对有负数值进行插值
#3.1 可以使用幸福值进行插值
#3.2 可以直接使用平均值进行插值

#尝试使用 carpet 函数的 prepross 函数进行插值


data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
# data_id = data.frame(data[,"id"])
#幸福度插值
data_happiness_interpol = Happiness_inter(data)
#Carpet 函数插值
ptm = proc.time()
data_bag = Bag_inter(data)
# describe(data_bag)
proc.time() - ptm
#单纯使用平均值进行插值
data_mean = Mean_inter(data)

col_names_for_test = get_Col_names_For_test(data_bag)



#================================使用 preProces 函数进行降维处理=========================
Process_data_pca  = preProcess(complete_data,method=c("scale","center","pca"))
complete_data_pca = predict(Process_data_pca,complete_data)

#========================================================================================
data_bag = Combine_All_features(data_bag)
data_bag = central_scale(data_bag)
data_bag = delete_features_for_linear(data_bag)




#=======================函数测试方法=============================================
#SVM 函数测试
type_reg = "eps-regression"
type_reg2 = "nu-regression"
tuned <- tune.svm(happiness ~ .,type = type_reg2,kernel ="radial",data = data_bag,gamma = 10^(-6:-1),cost = 10^(1:2))
summary(tuned)


model_SVM = svm(happiness ~., data = data_bag,type = type_reg2,kernel ="radial",gamma = 0.001,cost = 10) 
#===================================
#lm 函数
Model_linear = lm(formula = happiness ~.,data = data_bag )
#===================================
#随机森林
reg_RF = randomForest(happiness~.,data=train_data,importance=TRUE,ntree=1000)  
pred<-predict(reg_RF,newdata=test_data)  
score = result_compare(data.frame(pred),test_label)
#===================================

#Bagging
reg_boost =bagging(happiness~.,data=data,coob=TRUE,control=rpart.control(cp=0.025))  
test_set = data.frame(scale(test_set,center=T,scale=T))
dim(test_set)
result = predict(reg_boost,newdata = test_set)
#===================================
#NN
n <- names(train_data)
f <- as.formula(paste("happiness ~", paste(n[!n %in% "happiness"], collapse = " + ")))
nn <- neuralnet(f,data=train_data,hidden=c(10,5),linear.output=T)
#===================================================================================




#================================获取测试数据集================================================
test_set = getTestdata(col_names_for_test)
test_set = scale(test_set,center = T,scale = T)
SVM_test_result_1 = predict(model_SVM,test_set)
writeResult(SVM_test_result_1)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]
#===============================================================================================


