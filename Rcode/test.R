
# 读取数据
s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_abbr.csv')
# 删除id列
s = subset(s, select = -c(id))
col
#提取所有没有 NA 的列
for (i in 1:dim(s)[2]){
  if(sum(is.na(s[,i])) == 0 && s[1,i]!=""){
    col_not_NA = c(col_not_NA,colnames(s[i]))
  }
}
length(col_not_NA)
rownames(s)
length(col_not_NA)
#将所有没有 NA 的列提取并做一个新的数据表
new_data = s[,col_not_NA]
dim(new_data)
colnames(new_data)
rownames(new_data)
#删除 Survey Time
new_data = subset(new_data, select = -c(survey_time))
dim(new_data)

Model_linear = lm(formula = happiness ~. +survey_type:inc_ability, data = new_data )

#读取测试数据，进行输出happiness_test_abbr
test_data = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_test_abbr.csv')
test_data = subset(test_data, select = -c(id))
test_data = subset(test_data, select = -c(survey_time))
col_not_NA = col_not_NA[-5]
length(col_not_NA)
test_data_not_na = test_data[,col_not_NA]
dim(test_data_not_na)
dim(new_data)
happy_pre = predict(Model_linear, newdata = test_data_not_na)

id = seq(1:2968)
result = data.frame(id,happy_pre)
dim(result)
colnames(result) = c("id","happiness")
colnames(result)
rownames(result)
save_path = "D:/Study/Jean Monnet/Data Mining/Project/Data/result.csv"
write.csv(result,save_path,row.names = FALSE)

# 写入happiness_submit
new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
dim(new_result)
new_result[,2] = result[,2]
new_result[,2]
save_path = "D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv"
write.csv(new_result,save_path,row.names = FALSE)

source('processData.R')
process(1,2)
