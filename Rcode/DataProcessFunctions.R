processData <- function(data){
  #delete_set = c("id","city","county","survey_time","property_other")
  outlier = c()
  for (i in 1:dim(s)[1]){
    if(s[i,2]>5 | s[i,2] < 1){
      outlier = c(outlier,i)
    }
  }
  data = data[-outlier,]
  
  data = subset(data, select = -c(id,city,county,survey_time,property_other,invest_6))
  
  col_not_NA = c()
  #提取所有没有 NA 的列
  for (i in 1:dim(data)[2]){
    if(sum(is.na(data[,i])) == 0 && data[1,i]!=""){
      col_not_NA = c(col_not_NA,colnames(data[i]))
    }
  }
  new_data = data[,col_not_NA]
  
  return(list(new_data,col_not_NA))
}

getDataFrom_new_data_col_names <- function(dataset){
  data = dataset[1]
  data = as.data.frame(data)
  return(data)
}

getCol_names_From_new_data_col_names <- function(dataset){
  col_na_names  = dataset[2]
  return(col_na_names)
}

processTestdata <- function(data,col_not_na){
  col_not_na = col_not_na[-1]
  data = subset(data, select = -c(id,city,county,survey_time,property_other,invest_6))
  new_data = data[,col_not_na]
  return(new_data)
}

getTraindata <- function(dataset){
  return(dataset[1:7000,])
}
getTrainLabel <- function(dataset){
  train_label = dataset[1:7000]
  return(train_label)
}
getTestdata <- function(dataset){
  return(dataset[7001:dim(dataset)[1],])
}
getTestLabel <- function(dataset){
  test_label = dataset[7001:length(dataset)]
  return(test_label)
}
getTestsetComplete <- function(){
  dataset = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv")
  return(dataset)
}