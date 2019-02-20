processTestdata <- function(data,col_names){
  
  #在源文件中有一列是 invest
  data_invest = Combine_invest_feature(data)
  data = cbind(data,data_invest)
  
  data = interpolate_outlier_Test(data,col_names)
  need_inter_col = 	c("hukou_loc","family_income")
  data = interpolate_Test(data,need_inter_col)
  data = data[,col_names]
#-----------------------------------------------------------------------
  #添加新的一列 inc_exp - income
  data_inc_income = data.frame(abs(data[,"inc_exp"] - data[,"income"]))
  colnames(data_inc_income) = "inc_income"
  data = cbind(data,data_inc_income)
  
  #添加新的一列 BMI
  data_BMI = data.frame(abs(data[,"weight_jin"] / data[,"height_cm"]^2))
  colnames(data_BMI) = "BMI"
  data = cbind(data,data_BMI)
  #尝试删除weight_jin 和 height_cm 的值
  weight_height = c("weight_jin","height_cm")
  data = data[,-which(names(data) %in% weight_height )]
  
  #以下尝试合并 leisure—_1 到 Leisure_11 的特征，取round(mean)
  feature_leisure = c("leisure_1","leisure_2","leisure_3","leisure_4","leisure_5",
                      "leisure_6","leisure_7","leisure_8","leisure_9","leisure_10",
                      "leisure_11")
  data_leisure = data[,feature_leisure]
  data_leisure_combine = round(data.frame(apply(data_leisure,1,mean)))
  data_leisure_combine = data.frame(data_leisure_combine)
  colnames(data_leisure_combine) = "leisure"
  data = cbind(data,data_leisure_combine)
  #删除数据中的 leisure_1 到 Leisure_11 
  data = data[,-which(names(data) %in% feature_leisure )]
  dim(data)
  
  #合并 trust 特征
  feature_trust_stranger = c("trust_2","trust_3","trust_4","trust_7","trust_9",
                             "trust_10","trust_11","trust_12","trust_13")
  data_trust_stranger = data[,feature_trust_stranger]
  data_trust_stranger_combine = round(data.frame(apply(data_trust_stranger,1,mean)))
  data_trust_stranger_combine = data.frame(data_trust_stranger_combine)
  colnames(data_trust_stranger_combine) = "trust_stranger"
  data = cbind(data,data_trust_stranger_combine)
  data_trust_stranger_combine
  
  feature_trust_familar = c("trust_1","trust_5","trust_6","trust_8")
  data_trust_familar = data[,feature_trust_familar]
  dim(data_trust_familar)
  data_trust_familar_combine = round(data.frame(apply(data_trust_familar,1,mean)))
  data_trust_familar_combine = data.frame(data_trust_familar_combine)
  colnames(data_trust_familar_combine) = "trust_familar"
  data = cbind(data,data_trust_familar_combine)
  data_trust_familar_combine
  #删除数据中的 trust 数据集
  data = data[,-which(names(data) %in% feature_trust_stranger )]
  data = data[,-which(names(data) %in% feature_trust_familar )]
  
  return(data)
}

getTest_id <- function(){
  dataset = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv")
  test_id = dataset[,"id"]
  return(data.frame(test_id))
}

getTestdata <- function(col_names){
  col_names = col_names[-1]
  
  dataset = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv")
  dataset = processTestdata(dataset,col_names)
  
  return(data.frame(dataset))
}

interpolate_outlier_Test <- function(data,need_inter_col){

  inter_col_mean_matrix = matrix(nrow = 5, ncol = length(need_inter_col))
  
  for (i in 1:length(need_inter_col)){
    tem_index = which(data[,need_inter_col[i]] >= 0)
    tem_data = data[tem_index,need_inter_col[i]]
    inter_col_mean_matrix[1,i] = round(mean(tem_data))
  }
  
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
  return(data)
}

interpolate_Test <- function(data,need_inter_col){

  inter_col_mean_matrix = matrix(nrow = 5, ncol = length(need_inter_col))
  
  for (i in 1:length(need_inter_col)){
    tem_index = !is.na(data[,need_inter_col[i]])
    tem_data = data[tem_index,need_inter_col[i]]
    inter_col_mean_matrix[1,i] = mean(tem_data)
    
  }
  for (i in 1:length(need_inter_col)){
    #找出在 need_inter_col 列表中为 NA 的值，然后根据其幸福度指数以相应的值插值
    tem_need_inter_position = which(data[,need_inter_col[i]] %in% NA)
    num_need_inter = length(tem_need_inter_position)
    if(num_need_inter > 0 ){
      for (j in 1:num_need_inter){
        data[tem_need_inter_position[j],need_inter_col[i]] = inter_col_mean_matrix[1,i]
      }
    }
  }
  return(data)
}