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
  index_outlier = which(data[,"happiness"]<0)
  data = data[-index_outlier,]
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

Happiness_na_inter <- function(data,need_inter_col){
  #以下是在训练数据中以幸福度指数进行插值
  
  
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

Happiness_inter_less_0 <- function(data,need_inter_col){
  
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


Interpolate_BMI_Combine_Leisure_trust <- function(data){
  #添加新的一列 inc_exp - income
  data_inc_income = data.frame(abs(data[,"inc_exp"] - data[,"income"]))
  colnames(data_inc_income) = "inc_income"
  data = cbind(data,data_inc_income)
  
  #添加新的一列 BMI
  data_BMI = data.frame(abs((data[,"weight_jin"]/2.0) / (data[,"height_cm"]/100)^2))
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
  
  feature_trust_familar = c("trust_1","trust_5","trust_6","trust_8")
  data_trust_familar = data[,feature_trust_familar]
  dim(data_trust_familar)
  data_trust_familar_combine = round(data.frame(apply(data_trust_familar,1,mean)))
  data_trust_familar_combine = data.frame(data_trust_familar_combine)
  colnames(data_trust_familar_combine) = "trust_familar"
  data = cbind(data,data_trust_familar_combine)
  #删除数据中的 trust 数据集
  data = data[,-which(names(data) %in% feature_trust_stranger )]
  data = data[,-which(names(data) %in% feature_trust_familar )]
  
  return(data)
}

Combine_All_features <- function(data){
  
  
  col_not_na = Extract_col_not_na(data)
  data = data[,col_not_na]
  
  data = Interpolate_BMI_Combine_Leisure_trust(data)
  col_names = colnames(data)
  tem_store = data
  
  
  col_names
  
  
  data = tem_store
  
  
  # 以下开始尝试合并property特征
  property_own = c("property_1","property_2")
  data_property_own = data[,property_own]
  data_property_own_combine = ceiling(data.frame(apply(data_property_own,1,mean)))
  data_property_own_combine = data.frame(data_property_own_combine)
  colnames(data_property_own_combine) = "property_own"
  data = cbind(data,data_property_own_combine)
  dim(data)
  #删除数据中的 property_1 到 property_2 
  data = data[,-which(names(data) %in% property_own )]
  
  ## 合并property_0_8
  property_other = c("property_0","property_3","property_4","property_5",
                     "property_8","property_7","property_6")
  data_property_other = data[,property_other]
  
  data_property_other_combine = ceiling(data.frame(apply(data_property_other,1,mean)))
  data_property_other_combine = data.frame(data_property_other_combine)
  colnames(data_property_other_combine) = "property_other"
  data = cbind(data,data_property_other_combine)
  #删除数据中的 leisure_1 到 Leisure_11 
  data = data[,-which(names(data) %in% property_other )]
  
  
  tem_data_property_delete = data
  
  #以下尝试合并 Public_service
  feature_society = c("public_service_1","public_service_2","public_service_3",
                      "public_service_4","public_service_5","public_service_6",
                      "public_service_7","public_service_8","public_service_9")
  data_feature_society = data[,feature_society]
  data_feature_society_combine = round(data.frame(apply(data_feature_society,1,mean)))
  data_feature_society_combine = data.frame(data_feature_society_combine)
  colnames(data_feature_society_combine) = "society_service"
  data = cbind(data,data_feature_society_combine)
  #删除数据中的 public_service_1 到 public_service_9
  data = data[,-which(names(data) %in% feature_society )]
  tem_data_public_delete = data
  
  #尝试合并 Meida_1 Media_2 Media_3 
  feature_media_old = c("media_1","media_2","media_3","media_4")
  data_feature_media_old = data[,feature_media_old]
  data_feature_media_old_combine = round(data.frame(apply(data_feature_media_old,1,mean)))
  data_feature_media_old_combine = data.frame(data_feature_media_old_combine)
  colnames(data_feature_media_old_combine) = "media_old"
  data = cbind(data,data_feature_media_old_combine)
  #删除数据中的 media_1 到 media_2
  data = data[,-which(names(data) %in% feature_media_old )]
  
  feature_media_new = c("media_5","media_6")
  data_feature_media_new = data[,feature_media_new]
  data_feature_media_new_combine = round(data.frame(apply(data_feature_media_new,1,mean)))
  data_feature_media_new_combine = data.frame(data_feature_media_new_combine)
  colnames(data_feature_media_new_combine) = "media_new"
  data = cbind(data,data_feature_media_new_combine)
  #删除数据中的 media_1 到 media_2
  data = data[,-which(names(data) %in% feature_media_new )]
  
  data_age = data.frame(data[,"birth"])
  data_age = 2015 - data_age 
  colnames(data_age) = "age"
  data = cbind(data,data_age)
  data = subset(data,select = -c(birth))
  
  #以下对 class进行处理
  
  #class - class_10_before 称为 change
  feature_change = c("class","class_10_before")
  data_change = data[,feature_change]
  data_change_combine = data_change[,1] - data_change[,2]
  data_change_combine = data.frame(data_change_combine)
  colnames(data_change_combine) = "change"
  data = cbind(data,data_change_combine)
  
  #class_10_after - class 称为 attitude
  feature_attitude = c("class_10_after","class")
  data_attitude = data[,feature_attitude]
  data_attitude_combine = data_attitude[,1] - data_attitude[,2]
  data_attitude_combine = data.frame(data_attitude_combine)
  colnames(data_attitude_combine) = "attitude"
  data = cbind(data,data_attitude_combine)
  
  #class - class_14 family_change
  feature_family_change = c("class","class_14")
  data_family_change = data[,feature_family_change]
  data_family_change_combine = data_family_change[,1] - data_family_change[,2]
  data_family_change_combine = data.frame(data_family_change_combine)
  colnames(data_family_change_combine) = "family_change"
  data = cbind(data,data_family_change_combine)
  #删除 class,class_10_before，class_10_after，class_14
  data = subset(data,select = -c(class,class_14,class_10_after,class_10_before))
  dim(data)
  colnames(data)
  
  #以下合并insurance
  
  feature_insur = c("insur_1","insur_2","insur_3","insur_4")
  data_insur = data[,feature_insur]
  data_insur_combine = ceiling(data.frame(apply(data_insur,1,mean)))
  data_insur_combine = data.frame(data_insur_combine)
  colnames(data_insur_combine) = "insurance"
  data = cbind(data,data_insur_combine)
  data = data[,-which(names(data) %in% feature_insur )]
  
  
  #合并 status_peer 以及 status_3_before
  feature_effort = c("status_3_before","status_peer")
  data_effort = data[,feature_effort]
  data_effort_combine = data_effort[,1] - data_effort[,2]
  data_effort_combine = data.frame(data_effort_combine)
  colnames(data_effort_combine) = "effort"
  data = cbind(data,data_effort_combine)
  #删除 class,class_10_before，class_10_after，class_14
  data = subset(data,select = -c(status_3_before,status_peer))
  
  #以下对父亲母亲的情况进行处理
  #首先将父母的生日转化为年龄
  # feature_f_age = c("f_birth")
  # data_f_age = data[,feature_f_age]
  # data_f_age = 2015 - data_f_age
  # data_f_age = data.frame(data_f_age)
  # colnames(data_f_age) = "f_age"
  # data = cbind(data,data_f_age)
  # data = data[,-which(names(data) %in% feature_f_age )]
  # #母亲生日 --> 年龄
  # feature_m_age = c("m_birth")
  # data_m_age = data[,feature_m_age]
  # data_m_age = 2015 - data_m_age
  # data_m_age = data.frame(data_m_age)
  # colnames(data_m_age) = "m_age"
  # data = cbind(data,data_m_age)
  # data = data[,-which(names(data) %in% feature_m_age )]
  # #得到一个新 feature --> 父母年龄差 f_m_differ
  # data_f_m_differ = data.frame(abs(data_m_age - data_f_age ))
  # colnames(data_f_m_differ) = "f_m_age_differ"
  # data = data.frame(cbind(data,data_f_m_differ))
  # 
  # for( i in 1:nrow(data)){
  #   if(data[i,"f_m_age_differ"]>90){
  #     data[i,"f_m_age_differ"] = abs(100-data[i,"f_m_age_differ"])
  #   }
  # }
  # 
  #获取新 feature --> 父母平均教育edu
  feature_f_m_edu = c("f_edu","m_edu")
  data_f_m_edu = data[,feature_f_m_edu]
  data_f_m_edu = data.frame(round(data.frame(apply(data_f_m_edu,1,mean))))
  colnames(data_f_m_edu) = "f_m_edu"
  data = cbind(data,data_f_m_edu)
  data = data[,-which(names(data) %in% feature_f_m_edu )]
  
  colnames(data)
  return(data.frame(data))
}



get_Col_names_For_test <- function(data){
  col_not_na = Extract_col_not_na(data)
  data = data[,col_not_na]
  col_names_for_test = col_not_na
  return(col_names_for_test)
}
central_scale <- function(data){
  #以下进行所有特征的归一化
  data_label = data.frame(data[,"happiness"])
  colnames(data_label) = "happiness"
  data = subset(data,select = -c(happiness))
  data = scale(data,center=T,scale=T)
  data = data.frame(cbind(data_label,data))
  return(data)
}
Province_centra_scale <- function(data){
  
  #以下进行提取省市的归一化
  
  data_label = data.frame(data[,"happiness"])
  colnames(data_label) = "happiness"
  data = subset(data,select = -c(happiness))
  data_province = data.frame(data[,"province"])
  colnames(data_province) = "province"
  data = subset(data,select = -c(province))
  
  data = scale(data,center = T,scale = T)
  data = data.frame(cbind(data_label,data))
  data = data.frame(cbind(data,data_province))
  return(data)
}

delete_features_for_linear <- function(data){
  data = subset(data,select = -c(survey_type,province,nationality,edu,income,
                                 political,floor_area,health_problem,hukou,
                                 hukou_loc,leisure_12,socialize,learn,socia_outing,
                                 work_exper,family_income,son,f_political,f_work_14,
                                 m_birth,m_political,inc_exp,invest,inc_income,trust_familar,
                                 property_own,property_other,media_old,media_new,insurance,effort,f_birth,
                                 religion_freq,house,marital))
  return(data)
}

Bag_inter <- function(data){
  
  require(caret)
  source("DataProcessFunctions.R",encoding = "utf-8")
  # data = Outlier_delete(data)
  
  data_invest = Combine_invest_feature(data)
  data = data.frame(data,data_invest)
  
  data = Process_col(data)
  for(i in 1:nrow(data)){
    for (j in 1:ncol(data)){
      if(data[i,j] < 0 || is.na(data[i,j])){
        data[i,j] = NA
      }
    }
  }
  tem_data = preProcess(data,method = "bagImpute")
  data_tem = predict(tem_data,data)
  data_bag_inter = data.frame(data_tem)
  return(data_bag_inter)
}

Happiness_inter <- function(data){
  
  data = Outlier_delete(data)
  
  data_invest = Combine_invest_feature(data)
  data = cbind(data,data_invest)
  data = Process_col(data)
  
  col_need_inter = c()
  for (i in 1:ncol(data)){
    index = which(data[,colnames(data)[i]] < 0)
    if(length(index)>0){
      col_need_inter = c(col_need_inter,colnames(data)[i])
    }
  }
  
  data = Happiness_inter_less_0(data,col_need_inter)
  need_inter_col = 	c("hukou_loc","family_income")
  data = Happiness_na_inter(data,need_inter_col)
  return(data.frame(data))
  
}

Mean_inter <- function(data){
  source("Mean_inter_function.R",encoding = "utf-8")
  data = Outlier_delete(data)
  
  data_invest = Combine_invest_feature(data)
  data = cbind(data,data_invest)
  data = Process_col(data)
  
  col_need_inter = c()
  for (i in 1:ncol(data)){
    index = which(data[,colnames(data)[i]] < 0)
    if(length(index)>0){
      col_need_inter = c(col_need_inter,colnames(data)[i])
    }
  }
  
  data = Mean_interpolate_Less_0(data,col_need_inter)
  need_inter_col = 	c("hukou_loc","family_income")
  data = Mean_na_inter(data,need_inter_col)
  return(data.frame(data))
  
}

Outlier_num <- function(data){
  count = 0
  for(i in 1:nrow(data)){
    for (j in 1:ncol(data)){
      if(data[i,j] < 0 || is.na(data[i,j])){
        count = count + 1
      }
    }
  }
}