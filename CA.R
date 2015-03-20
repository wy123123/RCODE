CA=read.csv("C:/Users/Lovebonito/Downloads/Correlation analysis/correlation analysis.csv",header=TRUE,sep=",")
data1205=read.csv("C:/Users/Lovebonito/Downloads/CA_20141205.csv",header=TRUE,sep=",")
data10=read.csv("C:/Users/Lovebonito/Downloads/201410/201410.csv",header=TRUE,sep=",")
data11=read.csv("C:/Users/Lovebonito/Downloads/201411/201411.csv",header=TRUE,sep=",")

QTC=data1205
colnames(QTC)
QTC=subset(QTC,select=-c(Category.Performance.Analysis.with.charges...201411,X,X.1))
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
write.csv(QTC_4,file="QTC_4_TST.csv")
#find newIn
newIn_1=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141205/NewIn.csv",header=FALSE,sep=",")
newIn=as.vector(newIn_1$V1)
l=length(newIn)
tl=length(QTC_4$Product.SKU)
tempi=as.vector(QTC_4$Design)
newinIndicator=rep(0,tl)
for(i in 1:tl){
  for(s in 1 :l){   
    if(tempi[i]==as.vector(newIn[s]))
      newinIndicator[i]=1     
  }
}
#checking result
table(newinIndicator)
QTC_12=cbind(QTC_4,newinIndicator)
QTC_13=QTC_12[QTC_12$newinIndicator==1,]
table(as.vector(QTC_13$Design))
library(data.table)
QTC_13=data.table(QTC_13)
QTC_14=QTC_13[,sum(Quantity),by="Design"]
write.csv(QTC_13,file="lastlaunchitemsale_1.csv")







QTC_4=read.csv("C:/Users/Lovebonito/Documents/QTC_4_TST.csv")
#find name and other things
TEST=strsplit(as.vector(QTC_4$Product.SKU),"-")
TEST_2=data.frame(matrix(unlist(TEST),ncol=4,byrow=T))

QTC_5=cbind(QTC_4,TEST_2)
library(data.table)
QTC_5=data.table(QTC_5)
data_11=QTC_5[,sum(Quantity),by=c("Design","X4")]

data_all=rbind(data_09,data_10,data_11)

dataall=data_all[,sum(V1),by=c("Design","X4")]

findQ=function(x){
  for(i in 1 : 173)
    if(as.vector(x)==as.vector(dataall$Design[i]))
      return(as.vector(dataall$V1[i]))
}
S=sapply(as.vector(CA$Design),function(x) findQ(x))
S[lapply(S,length)==0]="NO VALUE"
dataall_1=cbind(CA,unlist(S))

Q=length(TT_1$V1)
findN=function(x){
  for(i in 1 : Q){
    if(x==as.vector(TT_1$Design[i]))
      return(1)
  }
}

cor(CA$fb.likes,CA$Quantity)
plot(CA$fb.likes,CA$Quantity,col="dark red",xlab="FB likes",ylab="Quantity")
