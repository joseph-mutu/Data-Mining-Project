rm(list=ls())
source("TestSetProcess.R",encoding = "utf-8")
source("writeResult.R",encoding = "utf-8")
source("test_functions.R",encoding = "utf-8")

source("DataProcessFunctions.R",encoding = "utf-8")
library(misc)
require(caret)
require(Hmisc)
require(corrplot)


colnames(s)[1]
tem = data.frame(s[,'f_birth'])

# index = data[data$f_birth<1900,]
# index[,'f_birth']
#以下是显示数据特征中有多少是具有小于0的异常值的特征
s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = Outlier_delete(s)
summary(data[,''])




colname_Less_0 = c()
for (i in 1:ncol(s)){
  tem_data = data.frame(s[,colnames(s)[i]])
  tem_data = tem_data<0
  tem_data = s[tem_data,]
  if(nrow(tem_data)>0){
    colname_Less_0 = c(colname_Less_0,colnames(s)[i])
  }
}
colnames(data)
length(colname_Less_0)


#以下是比较carpet 插值结果的==================
set.seed(1001)
random_Number = sample(1:nrow(data),30)
data1 = data
data1[random_Number,17] = NA#17 为 income
describe(data1)
income_impute = preProcess(data1,method = "bagImpute")
data_tem <- predict(income_impute,data1)

compare_Imputation <- data.frame(
  data[random_Number,17],
  data_tem[random_Number,17]
)
compare_Imputation
#=-========================

##====================检验因子之间的相关性
s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = Outlier_delete(s)
par(mfrow=c(1,2))
data_cor  =cor(data)
feature_society = c("public_service_1","public_service_2","public_service_3",
                    "public_service_4","public_service_5","public_service_6",
                    "public_service_7","public_service_8","public_service_9")
colnames(data)
data[,"public_service_2"]
data_feature_society = data.frame(data[,feature_society])
cor_society = cor(data_feature_society)
corrplot(data_cor,method="number")
corrplot(cor_society,method="number")

##=======================================


#============构建train_data 与 test_data 以及train_data.abbr test_data.abbr==============
train.comp = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
train.abbr = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_abbr.csv')
test.comp = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv')
test.abbr = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_abbr.csv')
dim(train.comp)
dim(train.abbr)
dim(test.comp)
dim(test.abbr)
sum(is.na(train.comp))
sum(is.na(train.abbr))
sum(is.na(test.comp))
sum(is.na(test.comp))

outlier_train.comp = Outlier_num(train.comp)
outlier_train.abbr = Outlier_num(train.abbr)
outlier_test.comp = Outlier_num(test.comp)
outlier_test.abbr = Outlier_num(test.comp)



# ```{r,echo=FALSE} 
# library(pander) 
# panderOptions('table.split.table', Inf) 
# set.caption('Hello Fisher!') 
# pander(head(iris)) 
# ```
data_info = matrix(ncol = 5,nrow = 4)
data_info = data.frame(data_info)
colnames(data_info) = c("Name","Data","Data.Outlier","Feature","Missing Ratio")
data_info[,"Name"] = c("train.comp","test.comp","train.abbr","test.abbr")
data_info[,"Data"] = c(8000,2968,8000,2968)
data_info[,"Data.Outlier"] = c(12,'\\',12,'\\')
data_info[,"Feature"] = c(140,139,42,41)
data_info[,"Missing"] = c(56903/(8000*140),21331/(2968*139),20197/(8000*42),7558/(2968*41))
head(data_info)
