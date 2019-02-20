





#现在尝试进行重组特征
#首先进行查看 happiness 有没有违规值

data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = subset(data,select = -c(id))
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

#先构架一个 5x8 的矩阵，表示5种幸福度指数分别在 8 种公共领域内评分的平均值
happiness_public_mean_matrix = matrix(nrow = 5, ncol = 9)
for (i in 1:5){
  tem_happiness = data[data$happiness==i,]
  # happiness_public_mean_matrix[i,1] = mean(tem_happiness[,(ncol(data)-7)])
  for (j in (ncol(data)-8):ncol(data)){
    happiness_public_mean_matrix[i,j-130] = mean(tem_happiness[,j])
  }
}

data_public_service = data[,(ncol(data)-8):ncol(data)]
#在各项公共领域的评分中，存在异常值，使用相同幸福度指数在该公共领域的平均值
#进行填补
dim(data_public_service)
data_public_service[123,1]
ncol(data)
for (i in 1:nrow(data_public_service)){
  for (j in 1:ncol(data_public_service)){
    if (data_public_service[i,j]<0 | data_public_service[i,j] >100){
      index = data[i,1]
      data_public_service[i,j] = happiness_public_mean_matrix[index,j]
    }
  }
}
index = sample(2,nrow(data_public_service),replace = T,prob = c(0.7,0.3))
train_data = data_public_service[index==1,]
test_data = data_public_service[index==2,]
train_label = data.frame(data[index==1,1])
test_label = data.frame(data[index==2,1])
train_data = data.frame(cbind(train_label,train_data))

colnames(train_data)[1] = "happiness"



Model_linear = lm(formula = happiness ~. +public_service_1:public_service_9,
                  data = train_data )

model_SVM = svm(happiness ~., data = train_data,type = "nu-regression",kernel ="radial")  
result = predict(model_SVM,test_data)
score = result_compare(data.frame(result),test_label)
score
