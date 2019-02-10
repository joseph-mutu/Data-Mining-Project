s = read.csv('D:/Study/DATA/data mining/happiness_train_complete.csv)
dim(s)
sum(is.na(s[0]))
dim(s[0])
dim(s)[2]
count = 0
for (i in 1:dim(s)[2]){
  if(sum(is.na(s[i]))>0){
    count = count + 1
  }
}
print(count)
