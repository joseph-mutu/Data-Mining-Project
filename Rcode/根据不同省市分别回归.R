rm(list=ls())
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
data = subset(data, select = -c(id,invest_7,edu_yr,s_work_type,s_work_status,invest_other,work_manage,work_type,work_yr,work_status,join_party,edu_yr,edu_other,city,county,survey_time,property_other,invest_6))
need_inter_col = 	c("hukou_loc","family_income")
data = interpolate(data,need_inter_col)
col_not_na = Extract_col_not_na(data)
data = data[,col_not_na]
dim(data)

index = sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train_data = data[index==1,]
test_data = data[index==2,]
test_data = subset(test_data,select = -c(happiness))
test_label = data.frame(data[index==2,1])
test_id = data.frame(data_id[index==2])



result_test = testData_province_lm(train_data,test_data,test_id)
score = result_compare(result_test,test_label)
score


test_s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv')
test_id = data.frame(test_s[,"id"])
col_not_na = col_not_na[-1]
test_s =  test_s[,col_not_na]
dim(test_s)
dim(data)
dim(test_id)
result_test = testData_province_SVM(data,test_s,test_id)
writeResult(result_test)

new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
new_result[2,2]
