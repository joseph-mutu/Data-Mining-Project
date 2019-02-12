s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')

# 删除id列
s = subset(s, select = -c(id))
col_not_NA = c()
#提取所有没有 NA 的列
for (i in 1:dim(s)[2]){
  if(sum(is.na(s[,i])) == 0 && s[1,i]!=""){
    col_not_NA = c(col_not_NA,colnames(s[i]))
  }
}
length(col_not_NA)
new_data = s[,col_not_NA]
colnames(new_data)
new_data = subset(new_data, select = -c(survey_time))
new_data = subset(new_data, select = -c(edu_other))


Model_linear = lm(formula = happiness ~. +survey_type:public_service_9, data = new_data )

test_data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_complete.csv')
test_data = subset(test_data, select = -c(id))

dim(test_data)
dim(new_data)
length(col_not_NA)
col_not_NA
dim(test_data)
dim(new_data)
col_not_NA = col_not_NA[-1]
length(col_not_NA)
test_data = test_data[,col_not_NA]
dim(test_data)
result = predict(Model_linear, newdata = test_data)
result = round(result)
length(result)
dim(new_result)

new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
new_result[,2] = result
new_result[,2]
save_path = "D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv"
write.csv(new_result,save_path,row.names = FALSE)
