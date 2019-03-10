rm(list=ls())
source("DataProcessFunctions.R",encoding = "utf-8")
source("TestSetProcess.R",encoding = "utf-8")
source("writeResult.R",encoding = "utf-8")
source("test_functions.R",encoding = "utf-8")
require(Hmisc)
require(caret)
library(ggplot2)



data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = Outlier_delete(data)
colnames_not_na = Extract_col_not_na(data)
colnames_not_na

#========================随机指定 30 个值设定为NA，然后进行插补，与原值进行对比================
#选择 public_service_4 作为测试对象，因为 Public_service在原始数据中没有缺失值
set.seed(803)
random_Number = sample(1:nrow(data),15)
data_need_inter = data
data_need_inter[random_Number,"public_service_8"] = NA
describe(data_need_inter["public_service_8"])
#幸福度插值
data_happiness_interpol = Happiness_na_inter(data_need_inter,"public_service_8")
#Carpet 插值
data_need_inter_Impute = preProcess(data_need_inter,method="bagImpute")
data_bag_inter = predict(data_need_inter_Impute,data_need_inter)
#
compare_Imputation <- data.frame(
  data[random_Number,"public_service_8"],
  data_happiness_interpol[random_Number,"public_service_8"],
  data_bag_inter[random_Number,"public_service_8"]
)
colnames(compare_Imputation) = c("original","happiness_inter","Bag_inter")
compare_Imputation
compare_Imputation = data.frame(compare_Imputation)
X = seq(1:15)
X
compare_Imputation = cbind(X,compare_Imputation)
p1 = ggplot(compare_Imputation, aes(x=X,y = original)) +
  geom_point(aes(y =happiness_inter,color="Happiness_inter")) + 
  geom_line(aes(y=happiness_inter,color="Happiness_inter"))+
  geom_point(aes(y =Bag_inter,color="Bag_inter")) + 
  geom_line(aes(y=Bag_inter,color="Bag_inter"))+
  geom_point(aes(y=original,color="Original")) + geom_line(aes(y=original, color="Original"))+
  scale_colour_manual("",values = c("Happiness_inter" = "red","Bag_inter" = "green","Original" = "black"))
  
p1 + scale_y_continuous(limits = c(30,110)) 



#==============================================================================================


#进行省份的有效样本分裂
data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = Outlier_delete(data)
NW = c(2,6,8,23,25,26,30,28,29,14)
N = c(5,9,11,16,18,21,22,31)
S = c(1,3,4,7,10,12,13,15,17,19,20,24,27)
data_NW = data[data$province %in% NW,]
data_N = data[data$province %in% N,]
data_S = data[data$province %in% S,]

data_distribution = data.frame(rbind(nrow(data_NW)/nrow(data),nrow(data_N)/nrow(data),nrow(data_S)/nrow(data)))
data_distribution = data.frame(cbind(name,data_distribution))
colnames(data_distribution) = c('name',"data")
data_distribution
filler = c("West","Central","North")
ggplot(data = data_distribution, aes(x = name, y = data, fill  = filler)) + 
geom_bar(stat = 'identity',position = 'dodge')+
ggtitle('Valid Data')+               
scale_fill_brewer(palette = 'Accent')+
scale_y_continuous(name='valid ratio', #y轴坐标名称
                   limits=c(0,0.5))#连续的标签和坐标轴
#=========================================================================================




#=======================================================================================
#进行离婚人群以及 BMI 指数与 幸福度指数的检测