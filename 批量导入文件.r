#主要通过list.files函数获取文件名，用for循环进行读取，现在展示一段批量读取400个病人的基因测序文件，文件类型为txt文本。#


library(dplyr)
nameList <- list.files()
matrix <- read.table(nameList[1],quote = "",sep = "\t",check.names = F,header = T)[c(1:2)]for (i in 2:length(nameList)){
  matrix <- inner_join(matrix,read.table(nameList[i],quote = "",sep = "\t",check.names = F,header = T)[c(1:2)],by="miRNA_ID")
}
names(matrix)[2:ncol(matrix)] <- nameList[1:length(nameList)]


#lapply 的用法
lapply ：list+apply
lapply(X,FUN): X是需要批量处理的因素，FUN表示施加的功能
#批量读取数据
  #先将文件名读出来
  files=list.files(pattern="*.csv")
  files
  fscv=lapply(files,read.csv)
  names(fscv)=files
  #批量读取完数据框后按行合并
  A<-plyr::ldply(fcsv,data.frame)
  #或
  B<-data.frame(do.call(rbind,fcsv))
#批量读取Rdata数据
  #假如我的工作目录有以.Rdata结尾的数据，用load函数读入,但是读取的不是内容而是名字
   load(".Rdata")
   files=list.files(pattern="*.Rdata)
   files
   fload1=lapply(files,load)
   fload2=(files,load,environment())
  #数据被存入list，用get函数显示出来
   fload=lapply(files,function(x)get(load(x)))


#批量生存分析
  rt=survival_data
  test=rt[1:10,1:10]
 #创建空的数据集
 res2=data.frame()
 #获取基因列表
  genes=colnames(rt)[-c(1:2)]
 for (i in 1:length(genes)){
  print (i)
 #中位数分组
group= ifelse(rt[,genes[i]>median(genes(rt[,genes[i]]),"high","low")
surv=as.formula(paste("Surv(time,status)~",group)
#生存分析求差异
x=survdiff(surv,data=data)
#获取P值
pvalue=1-pchisq(x$chisq,df=1)
#第一列基因名称
res2[i,1]=genes[i]
res2[i,2]=pvalue
}
#这个基因，无法分为两组，也就是说，这个基因的表达量都一样，这在真实状态下是不可能的，应该是本身表达量很低，标准化后变成一样的了。
if (length(table(group))==1) next
  #lapply循环生存分析
  genes <- colnames(rt)[-c(1:2)]
  system.time(res3 <- lapply(1:length(genes), function(i){
  group = ifelse(rt[,genes[i]]>median(rt[,genes[i]]),"high","low")
  if(length(table(group))==1) return(NULL)
  surv =as.formula(paste('Surv(futime, fustat)~', "group"))
  data = cbind(rt[,1:2],group)
  x = survdiff(surv, data = data)
  pValue=1-pchisq(x$chisq,df=1) 
  return(c(genes[i],pValue))
}))

#按照p从小到大排序，选取出p值小于0.05的基因,有1453个，绘制生存曲线
library(dplyr)
res5 <- res4 %>%
        filter(pValue_log < 0.05)%>%
        arrange(pValue_log)
#绘制生存曲线
index <- res5$ID[2]
group = ifelse(rt[,index]>median(rt[,index]),"high","low")
surv =as.formula(paste('Surv(futime, fustat)~', "group"))
data = cbind(rt[,1:2],group)
my.surv <- Surv(rt$futime, rt$fustat)
fit <- survfit(my.surv ~ group)
library(survminer)
ggsurvplot(fit, data = data)
 
