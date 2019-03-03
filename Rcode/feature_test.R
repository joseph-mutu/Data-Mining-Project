source("test_functions.R")
source("DataProcessFunctions.R")
source("writeResult.R")


data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = subset(data,select = -c(id))
data[3123,"family_income"] = 49865.04 
# which(data_select_features[,"family_income"] %in% NA)

outlier = c()
for (i in 1:nrow(data)){
  if (data[i,1] < 0 | data[i,1] > 5){
    outlier = c(outlier,i)
  }
}
data = data[-outlier,]
colnames(data)
dim(data)
happiness_1 = data[data$happiness==1,]
happiness_2 = data[data$happiness==2,]
happiness_3 = data[data$happiness==3,]
happiness_4 = data[data$happiness==4,]
happiness_5 = data[data$happiness==5,]
happiness_3[435,"family_income"] = 49865.04 

happiness_public_mean_matrix = matrix(nrow = 5, ncol = 9)
for (i in 1:5){
  tem_happiness = data[data$happiness==i,]
  # happiness_public_mean_matrix[i,1] = mean(tem_happiness[,(ncol(data)-7)])
  for (j in (ncol(data)-8):ncol(data)){
    happiness_public_mean_matrix[i,j-130] = mean(tem_happiness[,j])
  }
}

data_public_service = data[,(ncol(data)-8):ncol(data)]
#?ڸ???????????��??У??????쳣ֵ??ʹ????ͬ?Ҹ???ָ???ڸù?????????ƽ??ֵ
#?????

for (i in 1:nrow(data_public_service)){
  for (j in 1:ncol(data_public_service)){
    if (data_public_service[i,j]<0 | data_public_service[i,j] >100){
      index = data[i,1]
      data_public_service[i,j] = happiness_public_mean_matrix[index,j]
    }
  }
}
select_features = c("health","depression","equity","class","f_edu","family_income","family_status","view")

data_select_features = data[,select_features]
dim(data_select_features)
data_features = data.frame(cbind(data_select_features,data_public_service))
dim(data_features)
select_features_all = c(select_features,colnames(data_public_service))


index = sample(2,nrow(data_features),replace = T,prob = c(0.8,0.2))
train_data = data_features[index==1,]
test_data = data_features[index==2,]
train_label = data.frame(data[index==1,1])
test_label = data.frame(data[index==2,1])
dim(train_label)
dim(train_data)
train_data = data.frame(cbind(train_label,train_data))
colnames(train_data)[1] = "happiness"
all_label = data.frame(data[,1])
data_features_model = cbind(all_label,data_features)
data_features_model = data.frame(data_features_model)
colnames(data_features_model)[1] = "happiness"

Model_linear = lm(formula = happiness ~. ,
                  data = data_features_model )

result = predict(Model_linear, newdata = test_data)

test_data_submit = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv')
dim(test_data_submit)
test_data_submit = test_data_submit[,select_features_all]
check_na = checkNA(test_data_submit)
result_select_features = data.frame(predict(Model_linear, newdata = test_data_submit))
dim(result_select_features)
dim(test_data_submit)
writeResult(result_select_features)
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
class(new_result[2,2])
