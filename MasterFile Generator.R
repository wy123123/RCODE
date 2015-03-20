#Masterfile Generator
QTC=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141121/QPCS/Analytics All Web Site Data Product Performance 20141121-20141125.csv",header=TRUE,sep=",")
#cleaning data
indicatorQTC=substring(QTC$Product.SKU,1,1)
QTC_1=cbind(QTC,indicatorQTC)
QTC_2=with(QTC_1,QTC_1[indicatorQTC=="H"|indicatorQTC=="S"|indicatorQTC=="A"|indicatorQTC=="M"|indicatorQTC=="W",])
QTC_3=QTC_2[order(QTC_2$Product.SKU),]
#find productnames
TEST=strsplit(as.vector(QTC_3$Product),"-")
TEST=as.vector(unlist(TEST))
tl=length(TEST)/2
ProductNames=as.character(rep(NA,length(QTC_3$Product.SKU)))
for(i in 1 : tl){
  ProductNames[i]=TEST[2*i-1]
}
QTC_4=cbind(QTC_3,ProductNames)
#find colour
findColour=function(x){
  a=substring(x,1,1)
  if(a=="S")
    return(substring(x,7,9))
  else if(a=="A")
    return("NA")
  else
    return(substring(x,8,10))
}
colourIndicator=sapply(QTC_4$Product.SKU,function(x) findColour(x))
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
Design=sapply(QTC_4$Product.SKU, function(x) findDesign(x))
table(Design)
#check for results
table(colourIndicator)
colourIndicator=formatC(colourIndicator,width=3,format ="d",flag="0")
QTC_5=cbind(QTC_4,colourIndicator,Design)
#find newIn
newIn_0=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141121/NewIn.csv",header=FALSE,sep=",")
newIn=as.vector(newIn_0$V1)
l=length(newIn)
tl=length(QTC_5$Product.SKU)
tempi=as.vector(QTC_5$Design)
newinIndicator=rep(0,tl)
for(i in 1:tl){
  for(s in 1 :l){   
    if(tempi[i]==as.vector(newIn[s]))
      newinIndicator[i]=1     
  }
}
#checking result
table(newinIndicator)
QTC_5_a=cbind(QTC_5,newinIndicator)
#change colour
colourCode_1=read.csv("C:/Users/Lovebonito/Downloads/colourCode.csv",header=TRUE,sep=",")
colourCode_2=formatC(colourCode_1$code,width=3,format ="d",flag="0")
colourCode_3=cbind(colourCode_1,colourCode_2)
colL=length(QTC_5$Product.SKU)
colour=rep("NA",colL)
for(i in 1 :400){
  for(s in 1 : colL){
    if(as.vector(QTC_5$colourIndicator[s])==colourCode_3$colourCode_2[i])
      colour[s]=as.vector(colourCode_3$colour[i])
  }
}
#check result
table(colour)
QTC_6=cbind(QTC_5_a,colour)
#find the new product
class(QTC_6$newinIndicator)
QTC_6_NEW=QTC_6[QTC_6$newinIndicator==1,]
table(newinIndicator)

#QTC-----------------------------------------------------------------------------------------------------------
QPC=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141121/QTC/Analytics All Web Site Data Product Performance 20141121-20141125.csv",header=TRUE,sep=",")
lQPC=length(QPC$Quantity)
#cleaning data
indicatorQPC=substring(QPC$Product.SKU,1,1)
QPC_1=cbind(QPC,indicatorQPC)
QPC_2=with(QPC_1,QPC_1[indicatorQPC=="H"|indicatorQPC=="S"|indicatorQPC=="A"|indicatorQPC=="M"|indicatorQPC=="W",])
QPC_3_A=QPC_2[order(QPC_2$Product.SKU),]
Product.Category=QPC_3_A$Product.Category
MF=cbind(QTC_6,Product.Category)
#============================================================================
#Full data
write.csv(MF,file="MasterFile20141121.csv")
#Newin
MF_NEW=MF[QTC_6$newinIndicator==1,]
table(newinIndicator)
