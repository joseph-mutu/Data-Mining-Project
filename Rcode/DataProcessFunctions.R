Extract_col_not_na <- function(data){
  #delete_set = c("id","city","county","survey_time","property_other")
  col_not_NA = c()
  #提取所有没有 NA 的列
  for (i in 1:dim(data)[2]){
    if(sum(is.na(data[,i])) == 0 && data[1,i]!=""){
      col_not_NA = c(col_not_NA,colnames(data[i]))
    }
  }
  return(col_not_NA)
}
Process_col <- function(data){
  # data = subset(data, select = -c(id,edu_yr,s_work_type,
  #                                 s_work_status,invest_other,work_manage,
  #                                 work_type,work_yr,work_status,join_party,
  #                                 edu_other,city,county,survey_time,
  #                                 property_other,invest_other))
  data = subset(data, select = -c(id,edu_yr,s_work_type,
                                  s_work_status,invest_other,work_manage,
                                  work_type,work_yr,work_status,join_party,
                                  edu_other,city,county,survey_time,
                                  property_other,invest_0,invest_1,invest_2,
                                  invest_3,invest_4,invest_5,invest_6,invest_7,
                                  invest_8,invest_other))
  return(data)
}

Combine_invest_feature <- function(data){
  invest = c("invest_0","invest_1","invest_2","invest_3","invest_4","invest_5","invest_6","invest_7","invest_8")
  data_invest = data[,invest]
  nrow(data_invest)
  invest[1]
  data_com_invest = matrix(nrow = nrow(data_invest),ncol = 1)
  for(i in 1:nrow(data_invest)){
    flag = FALSE
    for (j in 1:length(invest)){
      if(data_invest[i,invest[j]] == 1){
        flag = TRUE
      }
    }
    if (flag){
      data_com_invest[i,1] = 1
    }
    else{
      data_com_invest[i,1] = 0
    }
  }
  data_com_invest = data.frame(data_com_invest)
  colnames(data_com_invest) = "invest"
  return(data_com_invest)
}
Extract_col_na <- function(data){
  col_NA = c()
  #提取所有 NA 的列
  for (i in 1:dim(data)[2]){
    if(sum(is.na(data[,i])) != 0 | data[1,i]==""){
      col_NA = c(col_NA,colnames(data[i]))
    }
  }
  return(col_NA)
}

Extract_province_data <- function(data){
  province_happiness = matrix(nrow = 5,ncol = 31)
  data_num = nrow(data)
  data_num
  for (i in 1:31){
    
    tem_data_province = data[data$province==i,] #提取省数据
    
    province_happ_1 = tem_data_province[tem_data_province$happiness==1,]
    province_happ_2 = tem_data_province[tem_data_province$happiness==2,]
    province_happ_3 = tem_data_province[tem_data_province$happiness==3,]
    province_happ_4 = tem_data_province[tem_data_province$happiness==4,]
    province_happ_5 = tem_data_province[tem_data_province$happiness==5,]
    
    province_happiness[1,i] = (nrow(province_happ_1)/data_num)*100
    province_happiness[2,i] = (nrow(province_happ_2)/data_num)*100
    province_happiness[3,i] = (nrow(province_happ_3)/data_num)*100
    province_happiness[4,i] = (nrow(province_happ_4)/data_num)*100
    province_happiness[5,i] = (nrow(province_happ_5)/data_num)*100
    
    
  }
  province_happiness = data.frame(province_happiness)
  colnames(province_happiness) = c("上海市","云南省","内蒙古自治区","北京市","吉林省","四川省",
                                   "天津市","宁夏回族自治区","安徽省","山东省","山西省",
                                   "广东省","广西壮族自治区","新疆维吾尔自治区","江苏省",
                                   "江西省","河北省","河南省","浙江省","海南省","湖北省",
                                   "湖南省","甘肃省","福建省","西藏自治区","贵州省","辽宁省",
                                   "重庆市","陕西省","青海省","黑龙江省")
  province_happiness = subset(province_happiness,select = -c(新疆维吾尔自治区,海南省,西藏自治区))
  return(province_happiness)
}



Outlier_delete <- function(data){
  outlier = c()
  for (i in 1:dim(s)[1]){
    if(s[i,2]>5 | s[i,2] < 1){
      outlier = c(outlier,i)
    }
  }
  data = data[-outlier,]
  return(data)
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

interpolate <- function(data,need_inter_col){
  
  happiness_1 = data[data$happiness==1,]
  happiness_2 = data[data$happiness==2,]
  happiness_3 = data[data$happiness==3,]
  happiness_4 = data[data$happiness==4,]
  happiness_5 = data[data$happiness==5,]
  
  # need_inter_col = 	c("hukou","social_neighbor","social_friend","family_income","minor_child")
  
  inter_col_mean_matrix = matrix(nrow = 5, ncol = length(need_inter_col))
  
  for (i in 1:length(need_inter_col)){
    tem_index_happiness_1 = !is.na(happiness_1[,need_inter_col[i]])
    tem_data_happiness_1 = happiness_1[tem_index_happiness_1,need_inter_col[i]]
    inter_col_mean_matrix[1,i] = mean(tem_data_happiness_1)
    
    tem_index_happiness_2 = !is.na(happiness_2[,need_inter_col[i]])
    tem_data_happiness_2 = happiness_2[tem_index_happiness_2,need_inter_col[i]]
    inter_col_mean_matrix[2,i] = mean(tem_data_happiness_2)
    
    tem_index_happiness_3 = !is.na(happiness_3[,need_inter_col[i]])
    tem_data_happiness_3 = happiness_3[tem_index_happiness_3,need_inter_col[i]]
    inter_col_mean_matrix[3,i] = mean(tem_data_happiness_3)
    
    tem_index_happiness_4 = !is.na(happiness_4[,need_inter_col[i]])
    tem_data_happiness_4 = happiness_4[tem_index_happiness_4,need_inter_col[i]]
    inter_col_mean_matrix[4,i] = mean(tem_data_happiness_4)
    
    tem_index_happiness_5 = !is.na(happiness_5[,need_inter_col[i]])
    tem_data_happiness_5 = happiness_5[tem_index_happiness_5,need_inter_col[i]]
    inter_col_mean_matrix[5,i] = mean(tem_data_happiness_5)
    
  }
  for (i in 1:length(need_inter_col)){
    #找出在 need_inter_col 列表中为 NA 的值，然后根据其幸福度指数以相应的值插值
    tem_need_inter_position = which(data[,need_inter_col[i]] %in% NA)
    num_need_inter = length(tem_need_inter_position)
    if(num_need_inter > 0 ){
      for (j in 1:num_need_inter){
        tem_happiness_index = data[tem_need_inter_position[j],"happiness"]
        data[tem_need_inter_position[j],need_inter_col[i]] = inter_col_mean_matrix[tem_happiness_index,i]
      }
    }
  }
  return(data)
}

interpolate_outlier_round <- function(data,need_inter_col){
  
  happiness_1 = data[data$happiness==1,]
  happiness_2 = data[data$happiness==2,]
  happiness_3 = data[data$happiness==3,]
  happiness_4 = data[data$happiness==4,]
  happiness_5 = data[data$happiness==5,]
  
  
  # need_inter_col = 	c("hukou","social_neighbor","social_friend","family_income","minor_child")
  
  inter_col_mean_matrix = matrix(nrow = 5, ncol = length(need_inter_col))
  
  for (i in 1:length(need_inter_col)){

    tem_index_happiness_1 = which(happiness_1[,need_inter_col[i]] >= 0)
    tem_data_happiness_1 = happiness_1[tem_index_happiness_1,need_inter_col[i]]
    inter_col_mean_matrix[1,i] = round(mean(tem_data_happiness_1))
    
    tem_index_happiness_2 = which(happiness_2[,need_inter_col[i]] >= 0)
    tem_data_happiness_2 = happiness_2[tem_index_happiness_2,need_inter_col[i]]
    inter_col_mean_matrix[2,i] = round(mean(tem_data_happiness_2))
    
    tem_index_happiness_3 = which(happiness_3[,need_inter_col[i]] >= 0)
    tem_data_happiness_3 = happiness_3[tem_index_happiness_3,need_inter_col[i]]
    inter_col_mean_matrix[3,i] = round(mean(tem_data_happiness_3))
    
    tem_index_happiness_4 = which(happiness_4[,need_inter_col[i]] >= 0)
    tem_data_happiness_4 = happiness_4[tem_index_happiness_4,need_inter_col[i]]
    inter_col_mean_matrix[4,i] = round(mean(tem_data_happiness_4))
    
    tem_index_happiness_5 = which(happiness_5[,need_inter_col[i]] >= 0)
    tem_data_happiness_5 = happiness_5[tem_index_happiness_5,need_inter_col[i]]
    inter_col_mean_matrix[5,i] = round(mean(tem_data_happiness_5))
    
  }
  
  for (i in 1:length(need_inter_col)){
    #找出在 need_inter_col 列表中为 NA 的值，然后根据其幸福度指数,以相应的值插值
    tem_need_inter_position = which(data[,need_inter_col[i]] < 0)
    num_need_inter = length(tem_need_inter_position)
    if(num_need_inter > 0 ){
      for (j in 1:num_need_inter){
        tem_happiness_index = data[tem_need_inter_position[j],"happiness"]
        data[tem_need_inter_position[j],need_inter_col[i]] = inter_col_mean_matrix[tem_happiness_index,i]
      }
    }
  }
  return(data)
}