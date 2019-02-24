rm(list=ls())
source("DataProcessFunctions.R")
source("TestSetProcess.R")
source("writeResult.R")
source("test_functions.R")

library("FactoMineR")
library("factoextra")
library(e1071)
library(arules)

#处理数据，合并，public_service 以及 meida 和property




s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = Outlier_delete(s)
data_id = data[,"id"]

data_invest = Combine_invest_feature(data)
data = cbind(data,data_invest)
data = Process_col(data)

col_need_inter = c()
for (i in 1:ncol(data)){
  index = which(data[,colnames(data)[i]] < 0)
  if(length(index)>0){
    col_need_inter = c(col_need_inter,colnames(data)[i])
  }
}
data = interpolate_outlier_round(data,col_need_inter)
need_inter_col = 	c("hukou_loc","family_income")
data = interpolate(data,need_inter_col)

col_not_na = Extract_col_not_na(data)
data = data[,col_not_na]
col_names_for_test = col_not_na
data = Interpolate_BMI_Combine_Leisure_trust(data)
col_names = colnames(data)
tem_store = data


col_names


data = tem_store


# 以下开始尝试合并property特征
property_own = c("property_1","property_2")
data_property_own = data[,property_own]
data_property_own_combine = ceiling(data.frame(apply(data_property_own,1,mean)))
data_property_own_combine = data.frame(data_property_own_combine)
colnames(data_property_own_combine) = "property_own"
data = cbind(data,data_property_own_combine)
dim(data)
#删除数据中的 property_1 到 property_2 
data = data[,-which(names(data) %in% property_own )]

## 合并property_0_8
property_other = c("property_0","property_3","property_4","property_5",
                   "property_8","property_7","property_6")
data_property_other = data[,property_other]

data_property_other_combine = ceiling(data.frame(apply(data_property_other,1,mean)))
data_property_other_combine = data.frame(data_property_other_combine)
colnames(data_property_other_combine) = "property_other"
data = cbind(data,data_property_other_combine)
#删除数据中的 leisure_1 到 Leisure_11 
data = data[,-which(names(data) %in% property_other )]


tem_data_property_delete = data

#以下尝试合并 Public_service
feature_society = c("public_service_1","public_service_2","public_service_3",
                    "public_service_4","public_service_5","public_service_6",
                    "public_service_7","public_service_8","public_service_9")
data_feature_society = data[,feature_society]
data_feature_society_combine = round(data.frame(apply(data_feature_society,1,mean)))
data_feature_society_combine = data.frame(data_feature_society_combine)
colnames(data_property_other_combine) = "society_service"
data = cbind(data,data_feature_society_combine)
#删除数据中的 public_service_1 到 public_service_9
data = data[,-which(names(data) %in% feature_society )]
tem_data_public_delete = data

#尝试合并 Meida_1 Media_2 Media_3 
feature_media_old = c("media_1","media_2","media_3","media_4")
data_feature_media_old = data[,feature_media_old]
data_feature_media_old_combine = round(data.frame(apply(data_feature_media_old,1,mean)))
data_feature_media_old_combine = data.frame(data_feature_media_old_combine)
colnames(data_feature_media_old_combine) = "media_old"
data = cbind(data,data_feature_media_old_combine)
#删除数据中的 media_1 到 media_2
data = data[,-which(names(data) %in% feature_media_old )]

feature_media_new = c("media_5","media_6")
data_feature_media_new = data[,feature_media_new]
data_feature_media_new_combine = round(data.frame(apply(data_feature_media_new,1,mean)))
data_feature_media_new_combine = data.frame(data_feature_media_new_combine)
colnames(data_feature_media_new_combine) = "media_new"
data = cbind(data,data_feature_media_new_combine)
#删除数据中的 media_1 到 media_2
data = data[,-which(names(data) %in% feature_media_new )]
tem_data_media_delete = data


dim(data)


# data = tem_data_public_delete
data = tem_data_media_delete
data_label = data.frame(data[,"happiness"])
# data_province = data.frame(data[,"province"])
colnames(data_label) = "happiness"
# colnames(data_province) = "province"
data = subset(data,select = -c(happiness))
# data = subset(data,select = -c(happiness,province))
data = scale(data,center=T,scale=T)
# data = scale(data)
data = data.frame(cbind(data_label,data))
# data = data.frame(cbind(data_label,data_province,data))
data = tem_data_media_delete


avg_score = 0.0

for ( i in 1:50){
  #抽样train和test
  index = sample(2,nrow(data),replace = T,prob = c(0.6,0.4))
  train_data = data.frame(data[index==1,])
  test_data = data.frame(data[index==2,])
  test_data = subset(test_data,select = -c(happiness))
  test_label = data.frame(data[index==2,1])
  test_id = data.frame(data_id[index==2])
  
  #Porvince_lm
  # result_test = testData_province_lm(train_data,test_data,test_id)
  # score = result_compare(data.frame(result_test),test_label)
  
  Model_linear = lm(formula = happiness ~.,data = train_data )
  lm_test_result = predict(Model_linear,test_data)
  score = result_compare(data.frame(lm_test_result),test_label)
  
  # print(i)
  # type_reg = "eps-regression"
  # type_reg2 = "nu-regression"
  # model_SVM_1 = svm(happiness ~., data = train_data,type = type_reg,kernel ="radial")
  # SVM_test_result_1 = predict(model_SVM_1,test_data)
  # score = result_compare(data.frame(SVM_test_result_1),test_label)
  
  
  avg_score = avg_score + score
  
}
avg_score/50
#抽样train和test

#SVM
type_reg = "eps-regression"
type_reg2 = "nu-regression"
model_SVM_1 = svm(happiness ~., data = train_data,type = type_reg,kernel ="radial") 
SVM_test_result_1 = predict(model_SVM_1,test_data)
SVM_test_result_1 = round(SVM_test_result_1)
score = result_compare(data.frame(SVM_test_result_1),test_label)

#lm模型
Model_linear = lm(formula = happiness ~.,data = data )
lm_test_result = predict(Model_linear,test_data)
lm_test_result = round(lm_test_result)
score = result_compare(data.frame(lm_test_result),test_label)
score 


model_SVM = svm(happiness ~., data = data,type = type_reg,kernel ="radial") 




#以下对 test_set 进行同样的处理

test_set = getTestdata(col_names_for_test)
dim(test_set)
dim(data)
class(test_set)

test_set = data.frame(scale(test_set,center=T,scale=T))

Lm_test_result = predict(Model_linear,test_set)
Lm_test_result
writeResult(Lm_test_result)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]
