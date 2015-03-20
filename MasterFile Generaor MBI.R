#Masterfile Generator
SKU_AGE=read.csv("C:/Users/Lovebonito/Downloads/201501/201501all.csv",header=TRUE,sep=",")
QTC=SKU_AGE

#cleaning data
QTC_QTC=QTC
QTC_QTC=subset(QTC_QTC,select=-c(Category.Performance.Analysis.with.charges...201501,X,X.1))
colnames(QTC_QTC)=c("ProductCategory","Product.SKU","Sales","Quantity","Cost")
QTC=QTC_QTC
indicatorQTC=substring(QTC$Product.SKU,1,1)
QTC_1=cbind(QTC,indicatorQTC)
QTC_2=with(QTC_1,QTC_1[indicatorQTC=="H"|indicatorQTC=="M"|indicatorQTC=="W"|indicatorQTC=="A"|indicatorQTC=="S",])
QTC_3=QTC_2[order(QTC_2$Product.SKU),]

#minor changes
write.csv(QTC_3,file="QTC_3.csv")
QTC_3=read.csv("C:/Users/Lovebonito/Documents/QTC_3.csv",header=TRUE,sep=",")

#find colour/design/product name
TEST=strsplit(as.vector(QTC_3$Product.SKU),"-")
TEST_1=data.frame(matrix(unlist(TEST),ncol=4,byrow=T))
#testing
table(TEST_1$X1)
QTC_4=cbind(QTC_3,TEST_1)
#find Product name
Qlength=length(QTC_4$X4)
x4=rep(NA,Qlength)
QTC_4$X4=as.character(QTC_4$X4)
for(i in 1 : Qlength){
  x4[i]=substring(QTC_4$X4[i], 2, nchar(QTC_4$X4[i]))
}
QTC_4$X4=x4
#find category
TEST2=strsplit(as.vector(QTC_4$ProductCategory),"-")
TEST2_1=data.frame(matrix(unlist(TEST2),ncol=2,byrow=T))
#testing
table(TEST2_1$X2)
#minor changes##change category!!
write.csv(QTC_4,file="QTC_4.csv")
QTC_4=read.csv("C:/Users/Lovebonito/Documents/QTC_4.csv",header=TRUE,sep=",")
x5=rep(NA,Qlength)
length(as.vector(TEST2_1$X2[3]))
for(i in 1 : Qlength){
  x5[i]=substring(TEST2_1$X2[i], 2, nchar(as.character(TEST2_1$X2[i])))
}
QTC_5=cbind(QTC_4,x5)
#check
#CHANGE PRODUCT NAMES
#changePN=function(x){
#    if(x=="P109")
#      return("COVET")
#    else if(x=="P108")
#      return("Basics")
#    else if(x=="P105")
#      return("One Piece Suits")
#    else if(x=="P107")
#      return("Bridesmaids")
#}
write.csv(QTC_5,file="QTC_5.csv")
QTC_5=read.csv("C:/Users/Lovebonito/Documents/QTC_5.csv",header=TRUE,sep=",")
table(unlist(QTC_5$x5))
QTC_6=subset(QTC_5,select=c(ProductCategory,Product.SKU,Sales,Quantity,Cost,X1,X2,X3,X4,x5))
colnames(QTC_6)[6:10]=c("ProductCode","colourcode","Size","ProductName","Category")
#changecolour
QTC_6$colourcode=formatC(QTC_6$colourcode,width=3,format ="d",flag="0")
colourCode_1=read.csv("C:/Users/Lovebonito/Downloads/colourCode.csv",header=TRUE,sep=",")
colourCode_2=formatC(colourCode_1$code,width=3,format ="d",flag="0")
colourCode_3=cbind(colourCode_1,colourCode_2)
changeColour=function(x){
  for(i in 1 :400){
    if(as.vector(x)==as.vector(colourCode_3$colourCode_2[i]))
      return(as.vector(colourCode_3$colour[i]))
  }
}
colour=sapply(as.vector(QTC_6$colourcode),function(x) changeColour(x))
table(unlist(colour))
QTC_7=cbind(QTC_6,colour)
#check MISSING COLOUR
write.csv(QTC_7,file="QTC_7.csv")
QTC_7=read.csv("C:/Users/Lovebonito/Documents/QTC_7.csv",header=TRUE,sep=",")
#
#

#sort by colour
library(data.table)
QTC_7=data.table(QTC_7)
QTC_8=QTC_7[,sum(Quantity),by=c("colour")]
temp=as.vector(QTC_8$colour)
Alength=length(QTC_8$V1)
for(i in 1 : Alength){
  if(as.numeric(QTC_8$V1[i])<=50){
    temp[i]="Other colours"
  } 
}
QTC_9=cbind(QTC_8,temp)
QTC_9=data.table(QTC_9)
QTC_10=QTC_9[,sum(V1),by=c("temp")]
write.csv(QTC_10,file="C:/Users/Lovebonito/Downloads/201501/201501COLOUR.csv")
#
#
#line category
indicatorQTC=substring(QTC_7$ProductCode,1,1)
temp2=as.vector(QTC_7$Category)
Zlength=length(QTC_7$Category)
for(i in 1 : Zlength){
  if(indicatorQTC[i]=="A")
    temp2[i]="Accessories"
  else if(indicatorQTC[i]=="W")
    temp2[i]="Bags"
  else if(indicatorQTC[i]=="S")
    temp2[i]="Shoes"
  }
QTC_12=cbind(QTC_7,temp2)
temp3=as.vector(QTC_12$temp2)
for(i in 1 : Zlength){
  if(temp2[i]=="Skirt")
    temp3[i]="Bottoms"
  else if(temp2[i]=="Bridesmaids"|temp2[i]=="One Piece Suits")
    temp3[i]="Dresses"
  else if(temp2[i]=="Outerwear")
    temp3[i]="Tops"
}

QTC_13=cbind(QTC_12,temp3)
write.csv(QTC_13,file="C:/Users/Lovebonito/Downloads/201501/MF01.csv",row.names=FALSE)
QTC_13=read.csv("C:/Users/Lovebonito/Downloads/201501/MF01.csv",header=TRUE,sep=",")
#
#
#Quantity sold
library(data.table)
QTC_13=data.table(QTC_13)
QTC_14=QTC_13[,sum(Quantity),by="temp3"]
#Revenue generated
QTC_15=QTC_13[,sum(Sales),by="temp3"]
#Cost by each line
QTC_16=QTC_13[,sum(Cost),by="temp3"]

#generate file
QTC_17=cbind(QTC_14,QTC_15$V1,QTC_16$V1)
setnames(QTC_17,c("Line","Quantity","Revenue","Cost"))
########
write.csv(QTC_17,file="C:/Users/Lovebonito/Downloads/201501//Cost_revenue_eachline_201501.csv")
