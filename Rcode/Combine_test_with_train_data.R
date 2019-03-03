rm(list=ls())
source("DataProcessFunctions.R",encoding = "utf-8")
source("TestSetProcess.R",encoding = "utf-8")
source("writeResult.R",encoding = "utf-8")
source("test_functions.R",encoding = "utf-8")
require(parallel)
require(doParallel)
library(e1071)
require(corrplot)
library(rpart)
library(adabag)
library(ipred)  
library(randomForest)  
require(Hmisc)
require(caret)
library(neuralnet)
library(xgboost)



n_Cores <- detectCores()##检测你的电脑的CPU核数
cluster_Set <- makeCluster(n_Cores)##进行集群
registerDoParallel(cluster_Set)

#=======================================将train_data与test结合在一起进行处理，然后进行分离================
traindata = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
traindata_id = traindata[,"id"]

# testdata = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv")
# traindata = Outlier_delete(traindata)
# traindata_happiness = traindata[,"happiness"]
# traindata = subset(traindata,select = -c(happiness))
# complete_data = data.frame(rbind(traindata,testdata))
# #testdata 从 7969 开始 到 10956 结束
# complete_data = Bag_inter(complete_data)


complete_Data = Get_complete_data_carpet()
dim(complete_data)
cor_complete = cor(complete_data)
corrplot(cor_complete,method="color",tl.pos = 'n')
#=============================使用 prepossess 函数进行降维=================================
Process_data_pca  = preProcess(complete_data,method=c("scale","center","pca"))
complete_data_pca = predict(Process_data_pca,complete_data)
cor_pca = cor(complete_data_pca)
corrplot(cor_pca,method="color",tl.pos = 'n')
#================================使用自己写的函数进行特征合并=========================================================
complete_data_combine_feature = Combine_All_features(complete_data)
cor_complete_combine = cor(complete_data_combine_feature)
corrplot(cor_complete_combine,method="color",tl.pos = 'n')

Process_data_pca_combine  = preProcess(complete_data_combine_feature,method=c("scale","center","pca"))
complete_data_pca_combine = predict(Process_data_pca_combine,complete_data_combine_feature)
cor_complete_combine = cor(complete_data_pca_combine)
corrplot(cor_complete_combine,method="color",tl.pos = 'n')
# str(complete_data)
complete_data_feature = complete_data_pca_combine

#根据 lm 线性回归的重要性对特征进行删减
complete_data = delete_features_for_linear(complete_data)
cor_combine_feature = cor(complete_data_combine_feature)
corrplot(cor_combine_feature,method="color",tl.pos = 'n')

dim(complete_data_pca)


#================================将train_data与test_data进行分裂============================
train_data = complete_data_pca_combine[1:7988,]
train_data = data.frame(cbind(traindata_happiness,train_data))
colnames(train_data)[1] = "happiness"
colnames(train_data)
test_data = complete_data_pca_combine[7989:10956,]
dim(train_data)
dim(test_data)
Model_linear = lm(formula = happiness ~.,data = train_data )

complete_data = get
cor_society = cor(train_data)
corrplot(cor_society,method="number",tl.pos = 'n')

#SVM=================================================================
type_reg = "eps-regression"
type_reg2 = "nu-regression"
tuned <- tune.svm(happiness ~ .,type = type_reg2,kernel ="radial",data = train_data,gamma = 10^(-6:-1),cost = 10^(1:2))
summary(tuned)
model_SVM = svm(happiness ~., data = train_data,type = type_reg2,kernel ="radial",gamma = 0.001,cost = 10) 

#==神经网络================================================================
n <- names(train_data)
f <- as.formula(paste("happiness ~", paste(n[!n %in% "happiness"], collapse = " + ")))
nn <- neuralnet(f,data=train_data,hidden=c(10,5),linear.output=T)
#==========================================================================
#XGBoost
xgb1 <- xgboost(data = as.matrix(train1[,-c('trees')]),
                label = train1$trees,
                objective='reg:linear',nrounds=300)
pre6 = predict(xgb1,as.matrix(test1[,-c('trees')]))
#==========================================================================



SVM_test_result_1 = predict(model_SVM,test_data)
writeResult(SVM_test_result_1)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]
# complete_data = Combine_All_features(complete_data)
# dim(complete_data)
# complete_data = central_scale(complete_data)