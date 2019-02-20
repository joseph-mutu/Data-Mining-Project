#将数据特征合并
rm(list=ls())
source("TestSetProcess.R")
source("DataProcessFunctions.R")
source("writeResult.R")
source("test_functions.R")
library("FactoMineR")
library("factoextra")
library(e1071)
library(arules)

s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = Outlier_delete(s)
data_id = data[,"id"]


need_inter_col = 	c("hukou_loc","family_income")
data = interpolate(data,need_inter_col)

data_invest = Combine_invest_feature(data)
data = Process_col(data)

col_not_na = Extract_col_not_na(data)
data = data[,col_not_na]
data = cbind(data,data_invest)

col_need_inter = c()
for (i in 1:ncol(data)){
  index = which(data[,colnames(data)[i]] < 0)
  if(length(index)>0){
    col_need_inter = c(col_need_inter,colnames(data)[i])
  }
}
data = interpolate_outlier_round(data,col_need_inter)
col_names = colnames(data)


index = sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train_data = data[index==1,]
test_data = data[index==2,]
test_data = subset(test_data,select = -c(happiness))
test_label = data.frame(data[index==2,1])
test_id = data.frame(data_id[index==2])

#进行普通的 SVM 测试
type_reg = "eps-regression"
type_reg2 = "nu-regression"
model_SVM = svm(happiness ~., data = train_data,type = type_reg,kernel ="radial") 
SVM_test_result = predict(model_SVM,test_data)
score = result_compare(data.frame(SVM_test_result),test_label)
score

#提取省份的SVM测试
result_test = testData_province_lm(train_data,test_data,test_id)
score = result_compare(result_test,test_label)
score


#进行真实的 test_set 数据写入
test_id = getTest_id()
dim(test_id)
test_set = getTestdata(colnames(data))
dim(test_id)
dim(data)
result_test = testData_province_lm(data,test_set,test_id)
writeResult(result_test)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
new_result[2,2]

