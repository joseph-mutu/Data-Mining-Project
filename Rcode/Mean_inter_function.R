Mean_interpolate_Less_0 <- function(data,need_inter_col){

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
  
  #以上是以平均值进行插值
}


Mean_na_inter <- function(data,need_inter_col){
  
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
  
  #以上是以平均值进行插值
  
}