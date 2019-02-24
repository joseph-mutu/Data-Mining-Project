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
library(Hmisc)
library(corrplot)



s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
test_set = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv")
test_set = subset(test_set,select = -c(id,edu_other,invest_other,property_other,
                                       survey_time,city,county,work_manage,minor_child,
                                       nationality,edu_yr,join_party,work_yr,work_type,
                                       marital_1st,s_birth,marital_now,s_edu,s_political,work_status,
                                       s_hukou,s_income,s_work_exper,s_work_status,s_work_type,edu_status))

test_set = interpolate_outlier_Test(test_set,colnames(test_set))
need_inter_col = colnames(test_set)
for (i in 1:length(need_inter_col)){
  #找出在 need_inter_col 列表中小于 0 的值，以平均值插值
  tem_need_inter_position = which(data[,need_inter_col[i]] < 0)
  num_need_inter = length(tem_need_inter_position)
  if(num_need_inter > 0 ){
    for (j in 1:num_need_inter){
      data[tem_need_inter_position[j],need_inter_col[i]] = inter_col_mean_matrix[1,i]
    }
  }
}



corrplot(factor_Corr,method="number")
#social_friend and social_neightbor

#cha zhi

#小于0
#class class_10_before class_10_after class_14 work_status family_income family_m son
#daughter f_birth f_edu f_political f_work_14 m_birth m_edu m_political m_work_14 status_peer

# data = subset(data, select = -c(id,edu_yr,s_work_type,
#                                 s_work_status,invest_other,work_manage,
#                                 work_type,work_yr,work_status,join_party,
#                                 edu_other,city,county,survey_time,
#                                 property_other,invest_0,invest_1,invest_2,
#                                 invest_3,invest_4,invest_5,invest_6,invest_7,
#                                 invest_8,invest_other))

