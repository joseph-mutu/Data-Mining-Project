source("DataProcessFunctions.R")
source("writeResult.R")
source("test_functions.R")
library("FactoMineR")
library("factoextra")
library(e1071)

s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')

new_data_col_names = processData(s)
data = getDataFrom_new_data_col_names(new_data_col_names)
col_na_names = getCol_names_From_new_data_col_names(new_data_col_names)

#³éÑùtrainºÍtest
index = sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
train_data = data[index==1,]
test_data = data[index==2,]

#SVM
type_reg = "eps-regression"
type_reg2 = "nu-regression"

model_SVM = svm(happiness ~., data = train_data,type = type_reg2,kernel ="radial")  
SVM_result = data.frame(predict(model_SVM,test_data))
score = result_compare(SVM_result,test_label)
score


#test
test_s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv')
col_na_names = unlist(col_na_names)
test_s = test_s[,col_na_names]
Test_data_Real = processTestdata(test_s,col_na_names)
dim(Test_data_Real)
SVM_test_result = predict(model_SVM,Test_data_Real)
length(SVM_test_result)
class(SVM_test_result[20])
writeResult(SVM_test_result)

test_s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv')
class(test_s[2,2])
