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


s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = Outlier_delete(s)
data_id = data.frame(data[,"id"])
data = Interpolate_ALL(data)
col_names_for_test = get_Col_names_For_test(data)
data = Combine_All_features(data)
data = Province_centra_scale(data)
# data = central_scale(data)

avg_score = 0.0
for ( i in 1:100){
  index = sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
  train_data = data.frame(data[index==1,])
  test_data = data.frame(data[index==2,])
  test_data = subset(test_data,select = -c(happiness))
  test_label = data.frame(data[index==2,1])
  test_id = data.frame(data_id[index==2,1])
  
  
  #Province_SVM
  # print(i)
  # result_test = testData_province_SVM(train_data,test_data,test_id)
  # score = result_compare(data.frame(result_test),test_label)
  
  # #Porvince_lm
  # result_test = testData_province_lm(train_data,test_data,test_id)
  # result_test = round(result_test)
  # score = result_compare(data.frame(result_test),test_label)
  
  #Lm model
  Model_linear = lm(formula = happiness ~.,data = train_data )
  lm_test_result = predict(Model_linear,test_data)
  lm_test_result = round(lm_test_result)
  score = result_compare(data.frame(lm_test_result),test_label)
  # score
  # print(i)
  # type_reg = "eps-regression"
  # type_reg2 = "nu-regression"
  # model_SVM_1 = svm(happiness ~., data = train_data,type = type_reg,kernel ="radial")
  # SVM_test_result_1 = predict(model_SVM_1,test_data)
  # score = result_compare(data.frame(SVM_test_result_1),test_label)
  
  
  avg_score = avg_score + score
  
}
# summary(Model_linear)
avg_score/100
data[,"inc_ability"]






#SVM
type_reg = "eps-regression"
type_reg2 = "nu-regression"
model_SVM_1 = svm(happiness ~., data = data,type = type_reg,kernel ="radial") 
SVM_test_result_1 = predict(model_SVM_1,test_data)
SVM_test_result_1 = round(SVM_test_result_1)
score = result_compare(data.frame(SVM_test_result_1),test_label)

#lmģ??
Model_linear = lm(formula = happiness ~.,data = data )
summary(Model_linear)

lm_test_result = predict(Model_linear,test_data)
lm_test_result = round(lm_test_result)
score = result_compare(data.frame(lm_test_result),test_label)
score 





dataset = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv")
# test_set_id = getTest_id()

# data_province_test_set = data.frame(test_set[,"province"])
# colnames(data_province_test_set) = "province"
# test_set = subset(test_set,select = -c(province))

# reg = rpart(happiness~.,data)
# result = predict(reg,data.frame(test_set))


#Province_SVM 
# result_test = testData_province_SVM(data,test_set,test_set_id)
# result_test


# test_set = subset(test_set,select = -c(f_birth,f_work_14,m_birth,
#                                        m_work_14))
test_set = getTestdata(col_names_for_test)

test_set = subset(test_set,select = -c(survey_type,province,nationality,edu,income,
                               political,floor_area,health_problem,hukou,
                               hukou_loc,leisure_12,socialize,learn,socia_outing,
                               work_exper,family_income,son,f_political,f_work_14,
                               m_birth,m_political,inc_exp,invest,inc_income,trust_familar,
                               property_own,property_other,media_old,media_new,insurance,effort,f_birth,
                               religion_freq,house,marital))

maxs <- apply(test_set, 2, max) 
mins <- apply(test_set, 2, min)
test_set <- as.data.frame(scale(test_set, center = mins, scale = maxs - mins))
dim(test_set)
dim(data)

n <- names(data)

f <- as.formula(paste("happiness ~", paste(n[!n %in% "happiness"], collapse = " + ")))
nn_4 <- neuralnet(f,data=data,hidden = 4,linear.output=T,err.fct = "sse",stepmax = 1e+06,
                threshold = 0.05)
nn_5 <- neuralnet(f,data=data,hidden = 5,linear.output=T,err.fct = "sse",stepmax = 1e+06,
                  threshold = 0.05)


net.predict<-compute(nn_5,test_set)
result = data.frame(net.predict$net.result)
writeResult(result)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]

dim(result)
dim(test_data)
score = result_compare(data.frame(result),test_label)
score




# SVM_test_result_1 = predict(model_SVM,test_set)


writeResult(SVM_test_result_1)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
new_result[2,2]
