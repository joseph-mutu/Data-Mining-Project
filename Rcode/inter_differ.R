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
original_data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
tem_data = Outlier_delete(original_data)
tem_data = cbind(tem_data[,"happiness"],tem_data[,"marital"])
data_happiness_interpol = Happiness_inter(original_data)

data_happiness = Combine_All_features(data_happiness_interpol)


tem_data = cbind(tem_data,data_happiness[,"BMI"])
colnames(tem_data) = c("happiness","martial","BMI")
tem_data = data.frame(tem_data)
#将数据分两个部分，一部分为结婚的，一部分为没结婚的
for (i in 1:nrow(tem_data)){
  if(tem_data[i,"martial"] == 1 ||tem_data[i,"martial"] == 2 ||tem_data[i,"martial"] == 5 || tem_data[i,"martial"] == 6 || tem_data[i,"martial"] == 7){
    tem_data[i,"martial"] = "UnMarried"
  }
  else{
    tem_data[i,"martial"] = "Married"
  }
}

for (i in 1:nrow(tem_data)){
  if(tem_data[i,"happiness"] < 3){
    tem_data[i,"happiness"] = "unHappiness"
  }
  else if(tem_data[i,"happiness"] == 3){
    tem_data[i,"happiness"] = "hard to say"
  }
  else{
    tem_data[i,"happiness"] = "Happiness"
  }
}


dim(tem_data)

marry_index = which(tem_data[,"martial"] %in% "Married")
unmarry_index = which(tem_data[,"martial"] %in% "UnMarried")

marry_data = tem_data[marry_index,]
unmarry_data = tem_data[unmarry_index,]
happ_ratio_marry = which(marry_data[,"happiness"] %in% "Happiness")
happ_ratio_unmarry = which(unmarry_data[,"happiness"] %in% "Happiness")
happ_ratio_marry = length(happ_ratio_marry)/nrow(marry_data)
happ_ratio_unmarry = length(happ_ratio_unmarry)/nrow(unmarry_data)
happ_ratio_marry
happ_ratio_unmarry


unmarry_index = sample(1:6247, size = 1793)
marry_data = marry_data[unmarry_index,]
dim(marry_data)
dim(unmarry_data)
test_data = data.frame(rbind(unmarry_data,marry_data))
set.seed(123)
test_data = sample(test_data,replace = FALSE)
dim(test_data)
test_data

qplot(martial,BMI,data=tem_data,colour = factor(happiness))+
  geom_hline(yintercept = 18.5,linetype = 2,col = 'red') +
  geom_text(aes(x=0.7,y=18.1,label=paste('underweight: 18.3')),
            size = 4,col = 'red')+
  geom_hline(yintercept = 23.9,linetype = 2,col = 'red') +
  geom_text(aes(x=0.7,y=23.5,label=paste('normal: 23')),
            size = 4,col = 'red')+
  geom_hline(yintercept = 27,linetype = 2,col = 'red') +
  geom_text(aes(x=0.7,y=26.6,label=paste('overweight: 27')),
            size = 4,col = 'red')+
  geom_hline(yintercept = 32,linetype = 2,col = 'red') +
  geom_text(aes(x=0.7,y=31.6,label=paste('obesity: 32')),
            size = 4,col = 'red')
  
ggplot(test_data, aes(x=martial, y=BMI,colour = factor(happiness)))+geom_point(position="jitter")+
  geom_hline(yintercept = 18.5,linetype = 2,col = 'red') +
  geom_text(aes(x=0.5,y=18.1,label=paste('18.3')),
            size = 4,col = 'red')+
  geom_hline(yintercept = 23.9,linetype = 2,col = 'red') +
  geom_text(aes(x=0.5,y=23.5,label=paste('23')),
            size = 4,col = 'red')+
  geom_hline(yintercept = 27,linetype = 2,col = 'red') +
  geom_text(aes(x=0.6,y=26.6,label=paste('OverW: 27')),
            size = 4,col = 'red')+
  geom_hline(yintercept = 32,linetype = 2,col = 'red') +
  geom_text(aes(x=0.6,y=31.6,label=paste('obesity: 32')),
            size = 4,col = 'red')+ylim(10,35)




happ_ratio_marry = which(marry_data[,"happiness"] %in% "Happiness")
happ_ratio_unmarry = which(unmarry_data[,"happiness"] %in% "Happiness")
length(happ_ratio_marry)/1793

length(happ_ratio_unmarry)/1793



#effort 和 attitude
library(ggplot2)
data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data_happiness_interpol = Happiness_inter(data)
data_happiness = Combine_All_features(data_happiness_interpol)
colnames(data_happiness)

happiness_data = data.frame(data_happiness[,"happiness"])
happiness_data
for (i in 1:nrow(happiness_data)){
  if(happiness_data[i,1] < 3){
    happiness_data[i,1] = "unHappiness"
  }
  else if(happiness_data[i,1] == 3){
    happiness_data[i,1] = "hard to say"
  }
  else{
    happiness_data[i,1] = "Happiness"
  }
}
happiness_data
effort_data = data_happiness[,'effort']
attitude_data = data_happiness[,'attitude']

effort_attitude_data = data.frame(cbind(happiness_data,effort_data,attitude_data))
colnames(effort_attitude_data) = c("happiness","effort","attitude")


attitude_data = data.frame(cbind(happiness_data,attitude_data))
colnames(attitude_data) = c("happiness","attitude")

effort_data = data.frame(cbind(happiness_data,effort_data))
colnames(effort_data) = c("happiness","effort")


happ_ratio_effort = which(effort_data[,"happiness"] %in% "Happiness")
unhapp_ratio_effort = which(effort_data[,"happiness"] %in% "unHappiness")
not_say_ratio_effort = which(effort_data[,"happiness"] %in% "hard to say")

length(unhapp_ratio_effort)
happiness_data



dim(effort_data)


happ_e_a_data = which(effort_attitude_data[,"happiness"] ==4)
length((happ_e_a_data))
happ_e_a_data = data.frame(effort_attitude_data[happ_e_a_data,])
dim(happ_e_a_data)

qplot(effort,attitude,data=effort_attitude_data,colour=factor(happiness))+
geom_point(position="jitter")

qplot(happiness,effort,data=effort_data,colour=factor(effort))+
  geom_point(position="jitter")
