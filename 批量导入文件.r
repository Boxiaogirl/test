library(dplyr)
nameList <- list.files()
matrix <- read.table(nameList[1],quote = "",sep = "\t",check.names = F,header = T)[c(1:2)]for (i in 2:length(nameList)){
  matrix <- inner_join(matrix,read.table(nameList[i],quote = "",sep = "\t",check.names = F,header = T)[c(1:2)],by="miRNA_ID")
}
names(matrix)[2:ncol(matrix)] <- nameList[1:length(nameList)]
