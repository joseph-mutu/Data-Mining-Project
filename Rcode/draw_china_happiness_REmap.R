rm(list=ls())
source("DataProcessFunctions.R")
source("TestSetProcess.R")
source("writeResult.R")
source("test_functions.R")
library(REmap)

s = read.csv('D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_train_complete.csv')
data = Outlier_delete(s)

# 1 = 上海市; 2 = 云南省; 3 = 内蒙古自治区; 4 = 北京市; 5 = 吉林省; 6 = 四川省; 7 = 天津市;
# 8 = 宁夏回族自治区; 9 = 安徽省; 10 = 山东省; 11 = 山西省; 12 = 广东省; 13 = 广西壮族自治区; 
# 14 = 新疆维吾尔自治区; 15 = 江苏省; 16 = 江西省; 17 = 河北省; 18 = 河南省; 19 = 浙江省; 
# 20 = 海南省; 21 = 湖北省; 22 = 湖南省; 23 = 甘肃省; 24 = 福建省; 25 = 西藏自治区; 
# 26 = 贵州省; 27 = 辽宁省; 28 = 重庆市; 29 = 陕西省; 30 = 青海省; 31 = 黑龙江省; 
data_province = data[,"province"]
data_province_name = as.data.frame(matrix(nrow = 31,ncol = 3,0))

dim(data_province_name)
for (i in 1:nrow(data)){
  if(data_province[i] == 1){
    data_province_name[1,1] = "上海"
    data_province_name[1,2] = data_province_name[1,2] + data[i,"happiness"]
    data_province_name[1,3] = data_province_name[1,3] + 1
  }
  else if(data_province[i] == 2){
    data_province_name[2,1] = "云南"
    data_province_name[2,2] = data_province_name[2,2] + data[i,"happiness"]
    data_province_name[2,3] = data_province_name[2,3] + 1
  }
  else if(data_province[i] == 3){
    data_province_name[3,1] = "内蒙古"
    data_province_name[3,2] = data_province_name[3,2] + data[i,"happiness"]
    data_province_name[3,3] = data_province_name[3,3] + 1
  }
  else if(data_province[i] == 4){
    data_province_name[4,1] = "北京"
    data_province_name[4,2] = data_province_name[4,2] + data[i,"happiness"]
    data_province_name[4,3] = data_province_name[4,3] + 1
  }
  else if(data_province[i] == 5){
    data_province_name[5,1] = "吉林"
    data_province_name[5,2] = data_province_name[5,2] + data[i,"happiness"]
    data_province_name[5,3] = data_province_name[5,3] + 1
  }
  else if(data_province[i] == 6){
    data_province_name[6,1] = "四川"
    data_province_name[6,2] = data_province_name[6,2] + data[i,"happiness"]
    data_province_name[6,3] = data_province_name[6,3] + 1
  }
  else if(data_province[i] == 7){
    data_province_name[7,1] = "天津"
    data_province_name[7,2] = data_province_name[7,2] + data[i,"happiness"]
    data_province_name[7,3] = data_province_name[7,3] + 1
  }
  else if(data_province[i] == 8){
    data_province_name[8,1] = "宁夏"
    data_province_name[8,2] = data_province_name[8,2] + data[i,"happiness"]
    data_province_name[8,3] = data_province_name[8,3] + 1
  }
  else if(data_province[i] == 9){
    data_province_name[9,1] = "安徽"
    data_province_name[9,2] = data_province_name[9,2] + data[i,"happiness"]
    data_province_name[9,3] = data_province_name[9,3] + 1
  }
  else if(data_province[i] == 10){
    data_province_name[10,1] = "山东"
    data_province_name[10,2] = data_province_name[10,2] + data[i,"happiness"]
    data_province_name[10,3] = data_province_name[10,3] + 1
  }
  else if(data_province[i] == 11){
    data_province_name[11,1] = "山西"
    data_province_name[11,2] = data_province_name[11,2] + data[i,"happiness"]
    data_province_name[11,3] = data_province_name[11,3] + 1
  }
  else if(data_province[i] == 12){
    data_province_name[12,1] = "广东"
    data_province_name[12,2] = data_province_name[12,2] + data[i,"happiness"]
    data_province_name[12,3] = data_province_name[12,3] + 1
  }
  else if(data_province[i] == 13){
    data_province_name[13,1] = "广西"
    data_province_name[13,2] = data_province_name[13,2] + data[i,"happiness"]
    data_province_name[13,3] = data_province_name[13,3] + 1
  }
  else if(data_province[i] == 14){
    data_province_name[14,1] = "新疆"
    data_province_name[14,2] = data_province_name[14,2] + data[i,"happiness"]
    data_province_name[14,3] = data_province_name[14,3] + 1
  }
  else if(data_province[i] == 15){
    data_province_name[15,1] = "江苏"
    data_province_name[15,2] = data_province_name[15,2] + data[i,"happiness"]
    data_province_name[15,3] = data_province_name[15,3] + 1
  }
  else if(data_province[i] == 16){
    data_province_name[16,1] = "江西"
    data_province_name[16,2] = data_province_name[16,2] + data[i,"happiness"]
    data_province_name[16,3] = data_province_name[16,3] + 1
  }
  else if(data_province[i] == 17){
    data_province_name[17,1] = "河北"
    data_province_name[17,2] = data_province_name[17,2] + data[i,"happiness"]
    data_province_name[17,3] = data_province_name[17,3] + 1
  }
  else if(data_province[i] == 18){
    data_province_name[18,1] = "河南"
    data_province_name[18,2] = data_province_name[18,2] + data[i,"happiness"]
    data_province_name[18,3] = data_province_name[18,3] + 1
  }
  else if(data_province[i] == 19){
    data_province_name[19,1] = "浙江"
    data_province_name[19,2] = data_province_name[19,2] + data[i,"happiness"]
    data_province_name[19,3] = data_province_name[19,3] + 1
  }
  else if(data_province[i] == 20){
    data_province_name[20,1] = "海南"
    data_province_name[20,2] = data_province_name[20,2] + data[i,"happiness"]
    data_province_name[20,3] = data_province_name[20,3] + 1
  }
  else if(data_province[i] == 21){
    data_province_name[21,1] = "湖北"
    data_province_name[21,2] = data_province_name[21,2] + data[i,"happiness"]
    data_province_name[21,3] = data_province_name[21,3] + 1
  }
  else if(data_province[i] == 22){
    data_province_name[22,1] = "湖南"
    data_province_name[22,2] = data_province_name[22,2] + data[i,"happiness"]
    data_province_name[22,3] = data_province_name[22,3] + 1
  }
  else if(data_province[i] == 23){
    data_province_name[23,1] = "甘肃"
    data_province_name[23,2] = data_province_name[23,2] + data[i,"happiness"]
    data_province_name[23,3] = data_province_name[23,3] + 1
  }
  else if(data_province[i] == 24){
    data_province_name[24,1] = "福建"
    data_province_name[24,2] = data_province_name[24,2] + data[i,"happiness"]
    data_province_name[24,3] = data_province_name[24,3] + 1
  }
  else if(data_province[i] == 25){
    data_province_name[25,1] = "西藏"
    data_province_name[25,2] = data_province_name[25,2] + data[i,"happiness"]
    data_province_name[25,3] = data_province_name[25,3] + 1
  }
  else if(data_province[i] == 26){
    data_province_name[26,1] = "贵州"
    data_province_name[26,2] = data_province_name[26,2] + data[i,"happiness"]
    data_province_name[26,3] = data_province_name[26,3] + 1
  }
  else if(data_province[i] == 27){
    data_province_name[27,1] = "辽宁"
    data_province_name[27,2] = data_province_name[27,2] + data[i,"happiness"]
    data_province_name[27,3] = data_province_name[27,3] + 1
  }
  else if(data_province[i] == 28){
    data_province_name[28,1] = "重庆"
    data_province_name[28,2] = data_province_name[28,2] + data[i,"happiness"]
    data_province_name[28,3] = data_province_name[28,3] + 1
  }
  else if(data_province[i] == 29){
    data_province_name[29,1] = "陕西"
    data_province_name[29,2] = data_province_name[29,2] + data[i,"happiness"]
    data_province_name[29,3] = data_province_name[29,3] + 1
  }
  else if(data_province[i] == 30){
    data_province_name[30,1] = "青海"
    data_province_name[30,2] = data_province_name[30,2] + data[i,"happiness"]
    data_province_name[30,3] = data_province_name[30,3] + 1
  }
  else if(data_province[i] == 31){
    data_province_name[31,1] = "黑龙江"
    data_province_name[31,2] = data_province_name[31,2] + data[i,"happiness"]
    data_province_name[31,3] = data_province_name[31,3] + 1
  }
  
}
data_province_name = data_province_name[-c(14,20,25),]
data_store = data_province_name
data_province_name[,2] = data.frame(data_province_name[,2]/data_province_name[,3])
# data_province_name[,2] = scale(data_province_name[,2],center = T,scale = T)
dim(data_province_name[,2])
maxs <- apply(data.frame(data_province_name[,2]), 2, max) 
mins <- apply(data.frame(data_province_name[,2]), 2, min)
data_province_name[,2] <- as.data.frame(scale(data_province_name[,2], center = mins, scale = maxs - mins))
city_value = data_province_name[,1:2]
colnames(city_value) = c("city","value")
city_value


# city_value[,"value"] = tem_value
# city_value
# city_name  = city_value[,"city"]
# city_name
# temp_name = c("shang hai","yun nan","nei meng gu","bei jing","ji lin","si chuan",
#               "tian jin","ning xia","an hui","shan dong","shan xi","guang dong",
#               "guang xi","jiang su","jiang xi","he bei","he nan","zhe jiang",
#               "hu bei","hu nan","gan su","fu jian","gui zhou","liao ning","chong qing",
#               "shan xi","qing hai","hei long jiang")
# temp_coor = get_geo_position(as.vector(temp_name))  #获取坐标
# temp_coor
# heatdata = data.frame(temp_coor$lon,temp_coor$lat,city_value[,"value"])

remapC(city_value,maptype = "China",color = 'red')

# color = c('cyan', 'lime', 'yellow', 'red')