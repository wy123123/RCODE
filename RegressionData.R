Test=read.csv("C:/Users/Lovebonito/Downloads/123123.csv",header=FALSE,sep=",")
QTC=Test
#
#cleaning data
QTC=subset(QTC,select=c(V5,V6,V7))
colnames(QTC)=c("Product.SKU","Sales","Quantity")
indicatorQTC=substring(QTC$Product.SKU,1,1)
QTC_1=cbind(QTC,indicatorQTC)
QTC_2=with(QTC_1,QTC_1[indicatorQTC=="H"|indicatorQTC=="S"|indicatorQTC=="M"|indicatorQTC=="W",])
QTC_3=QTC_2[order(QTC_2$Product.SKU),]
#
#find Product Name
TEST=strsplit(as.vector(QTC_3$Product.SKU),"-")
SKU=sapply(TEST, "[[", 1)
COLOURCODE=formatC(as.numeric(sapply(TEST, "[[", 2)),width=3,format ="d",flag="0")
#changecolour
colourCode_1=read.csv("C:/Users/Lovebonito/Downloads/colourCode.csv",header=TRUE,sep=",")
colourCode_2=formatC(colourCode_1$code,width=3,format ="d",flag="0")
colourCode_3=cbind(colourCode_1,colourCode_2)
changeColour=function(x){
  for(i in 1 :400){
    if(as.vector(x)==as.vector(colourCode_3$colourCode_2[i]))
      return(as.vector(colourCode_3$colour[i]))
  }
}
colour=sapply(as.vector(COLOURCODE),function(x) changeColour(x))
colour=data.frame(unlist(colour),ncol=1)
colour=colour$unlist.colour.
SIZE=sapply(TEST, "[[", 3)
PRODUCTNAME=substring(sapply(TEST, "[[", 4),2,nchar(sapply(TEST, "[[", 4)))
QTC_4=cbind(QTC_3,SKU,COLOURCODE,SIZE,PRODUCTNAME,colour)
###################
####check here#####
###################
QTC_4[,3]=as.numeric(QTC_4[,3])
#------------------------------
library(data.table)
QTC_5=data.table(QTC_4)
QTC_6=QTC_5[,sum(Quantity),by=c("SKU")]
write.csv(QTC_6,file="dress09.csv")

hist(QTC_6$V1,)
shapiro.test(log(QTC_6$V1))
mean(QTC_6$V1)
sd(QTC_6$V1)
upper=mean(QTC_6$V1)+3*sd(QTC_6$V1)
lower=mean(QTC_6$V1)-3*sd(QTC_6$V1)
QTC_7=QTC_6[QTC_6$V1>=lower&QTC_6$V1<=upper,]
hist(QTC_7$V1,breaks=60)
#-------------------------------------------
#
#find new items
d09=read.csv("C:/Users/Lovebonito/Documents/dress09.csv",header=TRUE,sep=",")
d10=read.csv("C:/Users/Lovebonito/Documents/dress10.csv",header=TRUE,sep=",")
l_9=length(d09$V1)
l_10=length(d10$V1)
nI=rep(1,l_10)
findN=function(x){
  for(i in 1 : l_9){
    if(x==as.vector(d09$SKU[i]))
      return(as.vector(d09))
  }
}
nI=sapply(as.vector(d10$SKU),function(x) findN(x))
table(nI)
summary(QTC_6$V1)
sd(QTC_6$V1)
hist(QTC_6$V1,breaks=20)
#upper=mean(QTC_6$V1)+3*sd(QTC_6$V1)
#lower=mean(QTC_6$V1)-3*sd(QTC_6$V1)
#QTC_7=QTC_6[QTC_6$V1>=lower&QTC_6$V1<=upper,]
QTC_8=QTC_6[QTC_6$V1>20,]

hist(QTC_8$V1)
hist(log(QTC_8$V1))
shapiro.test(QTC_6$V1)
