testData_Linear <- function(train_data,test_data){
  
  Model_linear = lm(formula = happiness ~. +survey_type:public_service_9,
                    data = train_data )
  
  result = predict(Model_linear, newdata = test_data)
  
  return(result)
}

testData_KNN <- function(traindata,testdata){
  
  library("class")
  data_test_pred <- knn(train=traindata,test=testdata,cl = traindata$happiness,k=10)
  return(as.integer(data_test_pred))
}

testData_PCA_Linear <- function(new_data_col_names){

  new_data = as.data.frame(new_data_col_names[1])
  col_names = as.vector(unlist(new_data_col_names[2]))
  train_test_label = new_data[,1]
  
  
  #PCA
  train_data = subset(new_data,select = -c(happiness))
  test_data = getTestsetComplete()
  test_data = processTestdata(test_data,col_names)
  train_test_data = rbind(train_data,test_data)
  dim(train_test_data)
  
  #将test数据集与train数据集结合在一起，送入 PCA 函数，进行归一化并降维
  #降维完成之后，将 train以及 test 拆分，以 train 进行回归
  train.pca = PCA(train_test_data,scale.unit = TRUE, graph = FALSE)
  eig.ind = get_pca_ind(train.pca)
  train_pca_data = as.data.frame(eig.ind$coord)
  train_data = train_pca_data[1:dim(train_data)[1],]
  test_data = train_pca_data[dim(train_data)[1]+1:dim(test_data)[1],]
  
  #将train的幸福度指数与归一化后的 train_data 结合，送入 lm 函数进行回归
  train_label = unlist(train_test_label)
  train_data$happiness = train_label
  train_data = as.data.frame(train_data)
  
  # eig.vari = get_pca_var(train.pca)
  # rotate_matrix = as.matrix(eig.vari$coord)
  
  Model_linear = lm(formula = happiness ~. +Dim.1:Dim.5,
                    data = train_data )
  
  #对 test 数据进行同样的 PCA 处理
  
  result = predict(Model_linear, newdata = test_data)
  result = data.frame(result)
  return(result)
}


result_compare <- function(result,test_label){
  num = dim(test_label)[1]
  score = 0.0
  for (i in 1:num){
    score = score + (result[i,1]-test_label[i,1])^2
  }
  score = as.numeric(score / num)
  return(score)
}