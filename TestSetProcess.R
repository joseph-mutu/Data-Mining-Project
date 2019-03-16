processTestdata <- function(data,col_names){
  source("DataProcessFunctions.R",encoding = "utf-8")
  
  data = Test_Process_col(data)
  data = Test_carpet_inter(data)
  data_invest = Combine_invest_feature(data)
  data = cbind(data,data_invest)
  data = Test_Process_invest(data)
  
  
  data = interpolate_outlier_Test(data,col_names)
  need_inter_col = 	c("hukou_loc","family_income")
  data = interpolate_Test(data,need_inter_col)
  data = data[,col_names]
  #-----------------------------------------------------------------------
  #new feature inc_exp - income
  data_inc_income = data.frame(abs(data[,"inc_exp"] - data[,"income"]))
  colnames(data_inc_income) = "inc_income"
  data = cbind(data,data_inc_income)
  
  #new feature BMI
  data_BMI = data.frame(abs(data[,"weight_jin"] / data[,"height_cm"]^2))
  colnames(data_BMI) = "BMI"
  data = cbind(data,data_BMI)
  #delete weight and height
  weight_height = c("weight_jin","height_cm")
  data = data[,-which(names(data) %in% weight_height )]
  
  feature_leisure = c("leisure_1","leisure_2","leisure_3","leisure_4","leisure_5",
                      "leisure_6","leisure_7","leisure_8","leisure_9","leisure_10",
                      "leisure_11")
  data_leisure = data[,feature_leisure]
  data_leisure_combine = round(data.frame(apply(data_leisure,1,mean)))
  data_leisure_combine = data.frame(data_leisure_combine)
  colnames(data_leisure_combine) = "leisure"
  data = cbind(data,data_leisure_combine)
  data = data[,-which(names(data) %in% feature_leisure )]
  dim(data)
  
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
  data = data[,-which(names(data) %in% feature_trust_stranger )]
  data = data[,-which(names(data) %in% feature_trust_familar )]
  
  property_own = c("property_1","property_2")
  data_property_own = data[,property_own]
  data_property_own_combine = ceiling(data.frame(apply(data_property_own,1,mean)))
  data_property_own_combine = data.frame(data_property_own_combine)
  colnames(data_property_own_combine) = "property_own"
  data = cbind(data,data_property_own_combine)
  dim(data)
  data = data[,-which(names(data) %in% property_own )]
  
  property_other = c("property_0","property_3","property_4","property_5",
                     "property_8","property_7","property_6")
  data_property_other = data[,property_other]
  
  data_property_other_combine = ceiling(data.frame(apply(data_property_other,1,mean)))
  data_property_other_combine = data.frame(data_property_other_combine)
  colnames(data_property_other_combine) = "property_other"
  data = cbind(data,data_property_other_combine)
  data = data[,-which(names(data) %in% property_other )]
  
  
  feature_society = c("public_service_1","public_service_2","public_service_3",
                      "public_service_4","public_service_5","public_service_6",
                      "public_service_7","public_service_8","public_service_9")
  data_feature_society = data[,feature_society]
  data_feature_society_combine = round(data.frame(apply(data_feature_society,1,mean)))
  data_feature_society_combine = data.frame(data_feature_society_combine)
  colnames(data_feature_society_combine) = "society_service"
  data = cbind(data,data_feature_society_combine)
  data = data[,-which(names(data) %in% feature_society )]
  tem_data_public_delete = data
  
  feature_media_old = c("media_1","media_2","media_3","media_4")
  data_feature_media_old = data[,feature_media_old]
  data_feature_media_old_combine = round(data.frame(apply(data_feature_media_old,1,mean)))
  data_feature_media_old_combine = data.frame(data_feature_media_old_combine)
  colnames(data_feature_media_old_combine) = "media_old"
  data = cbind(data,data_feature_media_old_combine)
  data = data[,-which(names(data) %in% feature_media_old )]
  
  feature_media_new = c("media_5","media_6")
  data_feature_media_new = data[,feature_media_new]
  data_feature_media_new_combine = round(data.frame(apply(data_feature_media_new,1,mean)))
  data_feature_media_new_combine = data.frame(data_feature_media_new_combine)
  colnames(data_feature_media_new_combine) = "media_new"
  data = cbind(data,data_feature_media_new_combine)
  data = data[,-which(names(data) %in% feature_media_new )]
  
  data_age = data.frame(data[,"birth"])
  data_age = 2015 - data_age 
  colnames(data_age) = "age"
  data = cbind(data,data_age)
  data = subset(data,select = -c(birth))
  

  feature_change = c("class","class_10_before")
  data_change = data[,feature_change]
  data_change_combine = data_change[,1] - data_change[,2]
  data_change_combine = data.frame(data_change_combine)
  colnames(data_change_combine) = "change"
  data = cbind(data,data_change_combine)
  
  #class_10_after - class is attitude
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
  #delete class,class_10_before，class_10_after，class_14
  data = subset(data,select = -c(class,class_14,class_10_after,class_10_before))
  dim(data)
  colnames(data)
  
  #combine status_peer and status_3_before
  feature_effort = c("status_3_before","status_peer")
  data_effort = data[,feature_effort]
  data_effort_combine = data_effort[,1] - data_effort[,2]
  data_effort_combine = data.frame(data_effort_combine)
  colnames(data_effort_combine) = "effort"
  data = cbind(data,data_effort_combine)
  #delete class,class_10_before，class_10_after，class_14
  data = subset(data,select = -c(status_3_before,status_peer))
  
  #combine insurance
  
  feature_insur = c("insur_1","insur_2","insur_3","insur_4")
  data_insur = data[,feature_insur]
  data_insur_combine = ceiling(data.frame(apply(data_insur,1,mean)))
  data_insur_combine = data.frame(data_insur_combine)
  colnames(data_insur_combine) = "insurance"
  data = cbind(data,data_insur_combine)
  data = data[,-which(names(data) %in% feature_insur )]
  
  feature_f_m_edu = c("f_edu","m_edu")
  data_f_m_edu = data[,feature_f_m_edu]
  data_f_m_edu = data.frame(round(data.frame(apply(data_f_m_edu,1,mean))))
  colnames(data_f_m_edu) = "f_m_edu"
  data = cbind(data,data_f_m_edu)
  data = data[,-which(names(data) %in% feature_f_m_edu )]
  
  return(data.frame(data))
}

getTest_id <- function(){
  dataset = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv")
  test_id = dataset[,"id"]
  return(data.frame(test_id))
}

Test_carpet_inter <- function(data){
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


getTestdata <- function(col_names){
  
  require(caret)
  
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
    #Using mean value to interpolate the test data
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

Test_Process_col <- function(data){
  data = subset(data, select = -c(id,edu_yr,s_work_type,
                                  s_work_status,invest_other,work_manage,
                                  work_type,work_yr,work_status,join_party,
                                  edu_other,city,county,survey_time,
                                  property_other,invest_other))
  return(data)
}
Test_Process_invest <- function(data){
  data = subset(data, select = -c(invest_0,invest_1,invest_2,
                                  invest_3,invest_4,invest_5,invest_6,invest_7,
                                  invest_8))
  return(data)
}