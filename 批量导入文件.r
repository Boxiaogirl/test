#主要通过list.files函数获取文件名，用for循环进行读取，现在展示一段批量读取400个病人的基因测序文件，文件类型为txt文本。#


library(dplyr)
nameList <- list.files()
matrix <- read.table(nameList[1],quote = "",sep = "\t",check.names = F,header = T)[c(1:2)]for (i in 2:length(nameList)){
  matrix <- inner_join(matrix,read.table(nameList[i],quote = "",sep = "\t",check.names = F,header = T)[c(1:2)],by="miRNA_ID")
}
names(matrix)[2:ncol(matrix)] <- nameList[1:length(nameList)]
