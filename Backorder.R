SKU_AGE=read.csv("C:/Users/Lovebonito/Downloads/Backorder/Backorder 20140401-20141130.csv",header=TRUE,sep=",")
QTC=SKU_AGE

#cleaning data
indicatorQTC=substring(QTC$Product.SKU,1,1)
QTC_1=cbind(QTC,indicatorQTC)
QTC_2=with(QTC_1,QTC_1[indicatorQTC=="H"|indicatorQTC=="S"|indicatorQTC=="M"|indicatorQTC=="W"|indicatorQTC=="L"|indicatorQTC=="A",])
QTC_3=QTC_2[order(QTC_2$Product.SKU),]
#
#find product name
TEST=strsplit(as.vector(QTC_3$Product),"-")
TEST_1=data.frame(matrix(unlist(TEST),ncol=2,byrow=T))
#testing
table(TEST_1$X1)

#find design
findDesign=function(x){
  a=substring(x,1,1)
  if(a=="S")
    return(substring(x,1,5))
  else if(a=="A")
    return(substring(x,1,7))
  else
    return(substring(x,1,6))
}
Design=sapply(QTC_3$Product.SKU, function(x) findDesign(x))
table(Design)
QTC_4=cbind(QTC_3,Design,TEST_1$X1)
library(data.table)
QTC_4=data.table(QTC_4)
most_BOed=QTC_4[,sum(Quantity),by=c("Design","TEST_1$X1","Product.Category")]
most_BOed=most_BOed[order(-most_BOed$V1),]
write.csv(most_BOed,file="most_BOed_201404_201411.csv")
write.csv(table(most_BOed$Product.Category),file="SSS.csv")
