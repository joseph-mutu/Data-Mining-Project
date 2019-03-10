rm(list=ls())
source("DataProcessFunctions.R",encoding = "utf-8")
source("TestSetProcess.R",encoding = "utf-8")
source("writeResult.R",encoding = "utf-8")
source("test_functions.R",encoding = "utf-8")

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
dim(s)
data = Outlier_delete(s)
data_id = data.frame(data[,"id"])
data = Interpolate_ALL(data)
dim(data)
col_names_for_test = get_Col_names_For_test(data)
data = Combine_All_features(data)
data = central_scale(data)
data_features = data
data = data_features
data = delete_features_for_linear(data)
dim(data)
# data = Province_centra_scale(data)


# reg_boost<-bagging(happiness~.,data=train_data,coob=TRUE,control=rpart.control(cp=0.025)) 
# reg_boost<-bagging(happiness~.,data=train_data)  
# result = predict(reg_boost,newdata = test_data)
# score = result_compare(data.frame(result),test_label)
# score
index = sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train_data = data.frame(data[index==1,])
test_data = data.frame(data[index==2,])
test_data = subset(test_data,select = -c(happiness))
test_label = data.frame(data[index==2,1])
test_id = data.frame(data_id[index==2,1])


type_reg = "eps-regression"
type_reg2 = "nu-regression"

tuned <- tune.svm(happiness ~ .,type = type_reg2,kernel ="radial",data = train_data,gamma = 10^(-6:-1),cost = 10^(1:2))
model_SVM_1_no_scale_59_feature = svm(happiness ~., data = train_data,type = type_reg2,kernel ="radial",gamma = 0.001,cost = 100) 




tuned <- tune.svm(happiness ~ .,type = type_reg2,kernel ="radial",data = data,gamma = 10^(-6:-1),cost = 10^(1:2))
summary(tuned)
model_SVM_1 = svm(happiness ~., data = data,type = type_reg2,kernel ="radial",gamma = 0.001,cost = 10) 

SVM_test_result_1 = predict(model_SVM_1_no_scale_59_feature,test_data)
score = result_compare(data.frame(SVM_test_result_1),test_label)
score

test_set = getTestdata(col_names_for_test)
dim(test_set)
test_set = subset(test_set,select = -c(survey_type,province,nationality,edu,income,
                                       political,floor_area,health_problem,hukou,
                                       hukou_loc,leisure_12,socialize,learn,socia_outing,
                                       work_exper,family_income,son,f_political,f_work_14,
                                       m_birth,m_political,inc_exp,invest,inc_income,trust_familar,
                                       property_own,property_other,media_old,media_new,insurance,effort,f_birth,
                                       religion_freq,house,marital))

dim(test_set)

dim(data)
test_set = scale(test_set,center = T,scale = T)
SVM_test_result_1 = predict(model_SVM_1,test_set)
# result = data.frame(net.predict$net.result)
writeResult(SVM_test_result_1)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]

data = data_features
dim(data)
reg_RF = randomForest(happiness~.,data=train_data,importance=TRUE,ntree=1000)  
pred<-predict(reg_RF,newdata=test_data)  
score = result_compare(data.frame(pred),test_label)
score
reg_RF$importance


test_set = getTestdata(col_names_for_test)
dim(data)
dim(test_set)
reg_boost =bagging(happiness~.,data=data,coob=TRUE,control=rpart.control(cp=0.025))  
test_set = data.frame(scale(test_set,center=T,scale=T))
dim(test_set)
result = predict(reg_boost,newdata = test_set)
result = data.frame(result)
dim(result)
writeResult(result)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]

library(ggplot2)
data_info = matrix(ncol = 7,nrow = 8)
data_info = data.frame(data_info)
colnames(data_info) = c("Model","bag.trainerror","bag.testerror","bag.bias","Happ.traierror","Happ.testerror","Happ.bias")
data_info[,"Model"] = c("NN","SVM","SVM.pro","LM","LM.pro","XGboost","RF","Bagging")
data_info[,"bag.trainerror"] = c(0.34,0.39,0.23,0.47,0.45,0.002,0.08,0.55)
data_info[,"bag.testerror"] = c(0.78,0.49,0.53,0.50,0.53,0.58,0.53,0.61)
data_info[,"bag.bias"] = c(0.44,0.096,0.30,0.030,0.070,0.58,0.45,0.057)
data_info[,"Happ.traierror"] = c(0.33,0.43,0.26,0.47,0.46,0.006,0.06,0.41)
data_info[,"Happ.testerror"] = c(30.1357,0.506,0.51,0.509,0.539,0.65,0.74,0.93)
data_info[,"Happ.bias"] = c(29.80,0.070,0.25,0.033,0.075,0.65,0.68,0.517)
data_info

res_par <- data.frame(cbind(data_info[,1],data_info[,3]))
colnames(res_par) = c("Model","test")
res2 = res_par
ggplot(res2,aes(Model,test))+ 
  geom_point(aes(x='SVM',y=res2[2,2]),size=15,col='pink') +
  geom_point(aes(col=Model,size = 3))+
  geom_text(aes(y=as.numeric(test)+0.2,label=paste(Model,sep = '.')),
            size = 4)+
  geom_hline(yintercept = 2,linetype = 2,col = 'red') +
  geom_text(aes(x=1,y=1.9,label=paste('Rule: 0.5')),
            size = 4,col = 'red')+
  theme(legend.position = 'none',
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  labs(title = 'Regression Comparations',x = 'Model',y = 'RMSE')

