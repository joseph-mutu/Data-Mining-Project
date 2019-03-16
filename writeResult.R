writeResult <- function(result){
  
  result = data.frame(result)
  new_result = read.csv("D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv")
  new_result[,2] = result
  new_result[,2]
  save_path = "D:/Study/Jean Monnet/Data Mining/Project/Data/happiness_submit.csv"
  write.csv(new_result,save_path,row.names = FALSE)
  
}