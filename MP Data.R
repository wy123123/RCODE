SKU_AGE_1=read.csv("C:/Users/Lovebonito/Documents/201403NewIn.csv",header=TRUE,sep=",")
SKU_AGE=read.csv("C:/Users/Lovebonito/Downloads/201404/201404.csv",header=TRUE,sep=",")
#SKU_AGE=read.csv("C:/Users/Lovebonito/Downloads/201312/201312dress.csv",header=TRUE,sep=",")

QTC=SKU_AGE
colnames(QTC)
QTC=subset(QTC,select=-c(Category.Performance.Analysis.with.charges...201404,X,X.1))
colnames(QTC)=c("ProductCategory","Product.SKU","Sales","Quantity")

#cleaning data
indicatorQTC=substring(QTC$Product.SKU,1,1)
QTC_1=cbind(QTC,indicatorQTC)
QTC_2=with(QTC_1,QTC_1[indicatorQTC=="H"|indicatorQTC=="S"|indicatorQTC=="M"|indicatorQTC=="W"|indicatorQTC=="L",])
QTC_3=QTC_2[order(QTC_2$Product.SKU),]
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
#check 
table(Design)
QTC_4=cbind(QTC_3,Design)
library(data.table)
QTC_4=data.table(QTC_4)


QTC_6=QTC_4[,sum(Quantity),by=c("Design")]


Q=length(TT_1$V1)
findN=function(x){
  for(i in 1 : Q){
    if(x==as.vector(TT_1$Design[i]))
      return(1)
  }
  
}
H=sapply(as.vector(QTC_6$Design),function(x) findN(x))
H[lapply(H,length)==0]="NO name"
QTC_7=cbind(QTC_6,unlist(H))
QTC_8=QTC_7[QTC_7$V2=="NO name",]
QTC_9=QTC_7[QTC_7$V2==1]
QTC_9=subset(QTC_9,select=-c(V2))
#add month

#
#
#
QTC_9=data.frame(QTC_9,MONTH=03)
QTC_8=data.frame(QTC_8,MONTH=04)
QTC_8=subset(QTC_8,select=-c(V2))
SKU_AGE_1=subset(SKU_AGE_1,select=-X)
TT=data.table(rbind(SKU_AGE_1,QTC_8,QTC_9))
TT_1=TT[,sum(V1),by=c("Design","MONTH")]
lastTT=TT_1

write.csv(TT_1,file="201403NewIn.csv")
#












findN=function(x){
  for(i in 1 : Q){
    if(x==as.vector(QTC_5$Design[i]))
      return(1)
  }
  
}
H=sapply(as.vector(QTC_6$Design),function(x) findN(x))
H[lapply(H,length)==0]="NO name"
QTC_7=cbind(QTC_6,unlist(H))
QTC_8=QTC_7[QTC_7$V2=="NO name",]
QTC_9=QTC_7[QTC_7$V2==1,]
write.csv(QTC_8,file="201312NewIn.csv")



QTC_6=QTC_5[order(-QTC_5$V1)]
QTC_7=QTC_6[-c(90:126),]
hist(TT_1$V1,breaks=40)



mean(TT_1$V1)
sd(TT_1$V1)
upper=3*sd(TT_1$V1)+mean(TT_1$V1)
lower=-3*sd(TT_1$V1)+mean(TT_1$V1)
QTC_8=TT_1[TT_1$V1<=upper&TT_1$V1>=lower,]

hist(log(QTC_8$V1),breaks=20)
shapiro.test(log(QTC_8$V1))



