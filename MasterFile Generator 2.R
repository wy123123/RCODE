library(data.table)
library(xlsx)
####change directory here
month="Mar"
filename="11 Mar"
#cost report dir
dirc="C:/Users/Lovebonito/Downloads/cost report/2015/"
#launching report dir
dirl="C:/Users/Lovebonito/Downloads/Launching Report/2015/"

#Masterfile Generator
SKU_AGE=read.xlsx(paste(dirl,month,"/",filename,"/SKU+AGE.xlsx",sep=""),header=TRUE,sheetIndex=2)
QTC=SKU_AGE

#cleaning data
indicatorQTC=substring(QTC$Product.SKU,1,1)
QTC_1=cbind(QTC,indicatorQTC)
QTC_2=with(QTC_1,QTC_1[indicatorQTC=="H"|indicatorQTC=="S"|indicatorQTC=="M"|indicatorQTC=="W"&indicatorQTC=="L"|indicatorQTC=="A",])
QTC_3=QTC_2[order(QTC_2$Product.SKU),]
QTC_3=QTC_3[!is.na(QTC_3$indicatorQTC),]

#find colour
findColour=function(x){
  a=substring(x,1,1)
  if(a=="S")
    return(substring(x,7,9))
  else if(a=="A")
    return("No Colour")
  else
    return(substring(x,8,10))
}
colourIndicator = sapply(as.vector(QTC_3$Product.SKU),function(x) findColour(x))
QTC_4=cbind(QTC_3,colourIndicator)

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
colour=sapply(as.vector(QTC_4$colourIndicator),function(x) changeColour(x))
#check result
table(unlist(colour))
colour[lapply(colour,length)==0]="No Colour"
colour=unlist(colour)
QTC_5=cbind(QTC_4,colour)
#check again
QTC_5[QTC_5$colour=="No Colour",]

#findAge
findAge=function(x){
  t=as.numeric(substring(x,1,2))
  return(t)
}
age=sapply(as.vector(QTC_5$Custom.Variable..Value.01.),function(x) findAge(x))
#check
QTC_6=cbind(QTC_5,age)
#find data with age data
QTC_7=QTC_6[!is.na(QTC_6$age),]
QTC_7_1=QTC_6[is.na(QTC_6$age),]
temp_1=rep("Unknown",length(QTC_7_1$age))
QTC_7_2=cbind(QTC_7_1,temp_1)
colnames(QTC_7_2)[15]="ageGroup"
#findAgeGroup
findAgeGroup=function(x){
  if(x<18)
    return("Under 18")
  else if(x>=18&x<=23)
    return("18 - 23")
  else if(x>=24&x<=29)
    return("24 - 29")
  else if(x>=30&x<=35)
    return("30 - 35")
  else 
    return("Above 35")  
}
ageGroup=sapply(as.vector(QTC_7$age),function(x) findAgeGroup(x))
#checking result
table(ageGroup)
QTC_8_1=cbind(QTC_7,ageGroup)
QTC_8=data.frame(rbind(QTC_8_1,QTC_7_2))

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
Design=sapply(QTC_8$Product.SKU, function(x) findDesign(x))
#check 
table(Design)
QTC_9=cbind(QTC_8,Design)

#Product Names-----------------------------------------------------------------------------------
SKU_PRODUCT=read.xlsx(paste(dirl,month,"/",filename,"/SKU+PRODUCT.xlsx",sep=""),header=TRUE,sheetIndex=2)
#clearn data
indicatorP=substring(SKU_PRODUCT$Product.SKU,1,1)
SKU_PRODUCT_1=cbind(SKU_PRODUCT,indicatorP)
SKU_PRODUCT_2=with(SKU_PRODUCT_1,SKU_PRODUCT_1[indicatorP=="H"|indicatorP=="S"|indicatorP=="M"|indicatorP=="W"|indicatorP=="L"|indicatorP=="A",])
SKU_PRODUCT_3=SKU_PRODUCT_2[order(SKU_PRODUCT_2$Product.SKU),]
#
#find Product Name
TEST=strsplit(as.vector(SKU_PRODUCT_3$Product),"-")
TEST_1=sapply(TEST, "[[", 1)
SKU_PRODUCT_4=cbind(SKU_PRODUCT_3,TEST_1)
SKU_PRODUCT_4=SKU_PRODUCT_4[!is.na(SKU_PRODUCT_4$Product.SKU),]
#
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
Design_1=sapply(SKU_PRODUCT_4$Product.SKU, function(x) findDesign(x))
#check 
table(Design_1)
SKU_PRODUCT_5=cbind(SKU_PRODUCT_4,Design_1)
#
#find Poduct Name + SKU
SKU_PRODUCT_6=data.table(SKU_PRODUCT_5)
SKU_PRODUCT_7=SKU_PRODUCT_6[,sum(Quantity),by=c("Design_1","TEST_1")]
#
#find AGE+PRODUCT NAME
No_Of_Product=length(SKU_PRODUCT_7$V1)
age_product=function(x){
  for(i in 1 : No_Of_Product)
  if(as.vector(x)==as.vector(SKU_PRODUCT_7$Design_1[i]))
    return(as.character(SKU_PRODUCT_7$TEST_1[i]))
}
productName=sapply(QTC_9$Design,function(x) age_product(x))
productName[lapply(productName,length)==0]="NO name"
QTC_10=data.frame(QTC_9,unlist(productName))
#
#Product Category-----------------------------------------------------------------------------------
#SKU_CATEGORY=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150122/SKU+CATEGORY.csv",header=TRUE,sep=",")
##
##clearn data
#indicatorC=substring(SKU_CATEGORY$Product.SKU,1,1)
#SKU_CATEGORY_1=cbind(SKU_CATEGORY,indicatorC)
#SKU_CATEGORY_2=with(SKU_CATEGORY_1,SKU_CATEGORY_1[indicatorC=="H"|indicatorC=="S"|indicatorC=="M"|indicatorC=="W"|indicatorC=="L"|indicatorC=="A",])
#SKU_CATEGORY_3=SKU_CATEGORY_2[order(SKU_CATEGORY_2$Product.SKU),]
#
#find design
#findDesign=function(x){
#  a=substring(x,1,1)
#  if(a=="S")
#    return(substring(x,1,5))
#  else if(a=="A")
#    return(substring(x,1,7))
#  else
#    return(substring(x,1,6))
#}
#Design_2=sapply(SKU_CATEGORY_3$Product.SKU, function(x) findDesign(x))
##check 
#table(Design_2)
#SKU_CATEGORY_4=cbind(SKU_CATEGORY_3,Design_2)
##
##find Poduct Categoty + SKU
#library(data.table)
#SKU_CATEGORY_5=data.table(SKU_CATEGORY_4)
#SKU_CATEGORY_6=SKU_CATEGORY_5[,sum(Quantity),by=c("Design_2","Product.Category")]
##
##find AGE+PRODUCT NAME
#age_product_category=function(x){
#  for(i in 1 : No_Of_Product)
#    if(as.vector(x)==as.vector(SKU_CATEGORY_6$Design_2[i]))
#      return(as.vector(SKU_CATEGORY_6$Product.Category[i]))
#}
#productCategory=sapply(as.vector(QTC_10$Design),function(x) age_product_category(x))
#productCategory[lapply(productCategory,length)==0]="NOCategory"
#QTC_11=data.frame(QTC_10,unlist(productCategory))

#find newIn
newIn_0=read.csv(paste(dirl,month,"/",filename,"/NewIn.csv",sep=""),header=T,sep=",")
newIn=as.vector(newIn_0$x)
l=length(newIn)
tl=length(QTC_10$Product.SKU)
tempi=as.vector(QTC_10$Design)
newinIndicator=rep(0,tl)
for(i in 1:tl){
  for(s in 1 :l){   
    if(tempi[i]==as.vector(newIn[s]))
      newinIndicator[i]=1     
  }
}
#checking result
table(newinIndicator)
QTC_12=cbind(QTC_10,newinIndicator)
QTC_13=QTC_12[QTC_12$newinIndicator==1,]
colnames(QTC_13)[17]=c("productName")
write.csv(QTC_13,file=paste(dirl,month,"/",filename,"/MasterFile.csv",sep=""))
#