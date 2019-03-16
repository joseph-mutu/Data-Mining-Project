testData_Linear <- function(train_data,test_data){
  
  Model_linear = lm(formula = happiness ~. +survey_type:public_service_9,
                    data = train_data )
  
  result = predict(Model_linear, newdata = test_data)
  
  return(result)
}

testData_KNN <- function(traindata,testdata){
  
  library("class")
  data_test_pred <- knn(train=traindata,test=testdata,cl = traindata$happiness,k=5)
  return(data.frame(as.integer(data_test_pred)))
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

testData_SVM <- function(data){
  #抽样train和test
  index = sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
  train_data = data[index==1,]
  test_data = data[index==2,]
  
  #SVM
  type_reg = "eps-regression"
  type_reg2 = "nu-regression"
  
  model_SVM = svm(happiness ~., data = train_data,type = type_reg2,kernel ="radial")  
  
  #test
  test_s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv')
  col_na_names = unlist(col_na_names)
  test_s = test_s[,col_na_names]
  Test_data_Real = processTestdata(test_s,col_na_names)
  dim(Test_data_Real)
  SVM_test_result = predict(model_SVM,Test_data_Real)
  return(SVM_test_result)
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

testData_province_lm <- function(train_data,test_data,test_id){
  
  train_data = data.frame(train_data)
  test_data = data.frame(test_data)
  test_id = data.frame(test_id)
  
  #将数据按照地理分为 3 个部分，分别为西部，中部，以及南方
  #注意 NW 代表西部 N 代表中部 S 代表南方
  
  NW = c(2,6,8,23,25,26,30,28,29,14)
  N = c(5,9,11,16,18,21,22,31)
  S = c(1,3,4,7,10,12,13,15,17,19,20,24,27)
  data_NW = train_data[train_data$province %in% NW,]
  data_N = train_data[train_data$province %in% N,]
  data_S = train_data[train_data$province %in% S,]
  
  lm_model_NW = lm(formula = happiness ~. ,data = data_NW)
  lm_model_N = lm(formula = happiness ~. ,data = data_N)
  lm_model_S = lm(formula = happiness ~. ,data = data_S)
  
  
  index = which(test_data[,"province"] %in% NW)
  test_NW = test_data[index,]
  test_NW_id = test_id[index,]
  
  index = which(test_data[,"province"] %in% N)
  test_N = test_data[index,]
  test_N_id = test_id[index,]
  
  index = which(test_data[,"province"] %in% S)
  test_S = test_data[index,]
  test_S_id = test_id[index,]
  
  lm_result_NW = predict(lm_model_NW,test_NW)
  lm_result_NW = data.frame(test_NW_id,lm_result_NW)
  colnames(lm_result_NW) = c("id","result")
  dim(lm_result_NW)
  
  lm_result_N = predict(lm_model_N,test_N)
  lm_result_N = data.frame(test_N_id,lm_result_N)
  colnames(lm_result_N) = c("id","result")
  dim(lm_result_N)
  
  lm_result_S = predict(lm_model_S,test_S)
  lm_result_S = data.frame(test_S_id,lm_result_S)
  colnames(lm_result_S) = c("id","result")
  dim(lm_result_S)
  
  lm_result_need_sort = rbind(lm_result_NW,lm_result_N,lm_result_S)
  lm_result_sort = lm_result_need_sort[order(lm_result_need_sort$id),]
  return(data.frame(lm_result_sort[,"result"]))
  
}


testData_province_SVM <- function(train_data,test_data,test_id){
  
  train_data = data.frame(train_data)
  test_data = data.frame(test_data)
  test_id = data.frame(test_id)
  
  #将数据按照地理分为 3 个部分，分别为西北，北部，以及南方  
  NW = c(2,6,8,23,25,26,30,28,29,14)
  N = c(5,9,11,16,18,21,22,31)
  S = c(1,3,4,7,10,12,13,15,17,19,20,24,27)
  data_NW = train_data[train_data$province %in% NW,]
  data_N = train_data[train_data$province %in% N,]
  data_S = train_data[train_data$province %in% S,]
  
  type_reg ="nu-regression"
  type_reg2 = "eps-regression"
  SVM_model_NW = svm(happiness ~., data = data_NW,type = type_reg2,kernel ="radial") 
  SVM_model_N = svm(happiness ~., data = data_N,type = type_reg2,kernel ="radial") 
  SVM_model_S = svm(happiness ~., data = data_S,type = type_reg2,kernel ="radial") 
  
  
  index = which(test_data[,"province"] %in% NW)
  test_NW = test_data[index,]
  test_NW_id = test_id[index,]
  
  index = which(test_data[,"province"] %in% N)
  test_N = test_data[index,]
  test_N_id = test_id[index,]
  
  index = which(test_data[,"province"] %in% S)
  test_S = test_data[index,]
  test_S_id = test_id[index,]
  
  SVM_result_NW = predict(SVM_model_NW,test_NW)
  SVM_result_NW = data.frame(test_NW_id,SVM_result_NW)
  colnames(SVM_result_NW) = c("id","result")
  dim(SVM_result_NW)
  
  SVM_result_N = predict(SVM_model_N,test_N)
  SVM_result_N = data.frame(test_N_id,SVM_result_N)
  colnames(SVM_result_N) = c("id","result")
  dim(SVM_result_N)
  
  SVM_result_S = predict(SVM_model_S,test_S)
  SVM_result_S = data.frame(test_S_id,SVM_result_S)
  colnames(SVM_result_S) = c("id","result")
  dim(SVM_result_S)
  
  SVM_result_need_sort = rbind(SVM_result_NW,SVM_result_N,SVM_result_S)
  dim(SVM_result_need_sort)
  dim(test_data)
  SVM_result_sort = SVM_result_need_sort[order(SVM_result_need_sort$id),]
  return(data.frame(SVM_result_sort[,"result"]))
}


test_Linear_Train_test <- function(data){
  index = sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
  train_data = data.frame(data[index==1,])
  train_label = data.frame(data[index==1,"happiness"])
  test_data = data.frame(data[index==2,])
  test_data = subset(test_data,select = -c(happiness))
  test_label = data.frame(data[index==2,1])
  test_id = data.frame(data_id[index==2,1])
  
  Model_linear = lm(formula = happiness ~.,data = train_data )
  lm_test_result = predict(Model_linear,test_data)
  score = result_compare(data.frame(lm_test_result),test_label)
  return(score)
}