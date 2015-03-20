#dataset construction
#QAT-------------------------------------------------------------------------------------------------
QAT=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141121/QAT/Analytics All Web Site Data Product Performance 20141121-20141125.csv",header=TRUE,sep=",")
lQAT=length(QAT$Age)
#DELETING LAST row
QAT_1=QAT[-lQAT,]


#QPC-------------------------------------------------------------------------------------------------
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

#find colour
colourIndicatorQPC=sapply(QPC_3$Product.SKU,function(x) findColour(x))
QPC_4=cbind(QPC_3,colourIndicatorQPC)
#change colour
colourCode_1=read.csv("C:/Users/Lovebonito/Downloads/colourCode.csv",header=TRUE,sep=",")
colourCode_2=formatC(colourCode_1$code,width=3,format ="d",flag="0")
colourCode_3=cbind(colourCode_1,colourCode_2)
colL=length(QPC_4$Product.SKU)
colourQPC=rep("NA",colL)
for(i in 1 :400){
  for(s in 1 : colL){
    if(as.vector(QPC_4$colourIndicatorQPC[s])==colourCode_3$colourCode_2[i])
      colourQPC[s]=as.vector(colourCode_3$colour[i])
  }
}
#check result
table(colourQPC)
QPC_5=cbind(QPC_4,colourQPC)
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
Design=sapply(QPC_5$Product.SKU, function(x) findDesign(x))
QPC_6=cbind(QPC_5,Design)
#find newIn
newIn_0=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141121/NewIn.csv",header=FALSE,sep=",")
newIn=as.vector(newIn_0$V1)
l=length(newIn)
tl=length(QPC_6$Product.SKU)
tempi=as.vector(QPC_6$Design)
newinIndicatorQPC=rep(0,tl)
for(i in 1:tl){
  for(s in 1 :l){   
    if(tempi[i]==as.vector(newIn[s]))
      newinIndicatorQPC[i]=1     
  }
}
QPC_7=cbind(QPC_6,newinIndicatorQPC)
table(newinIndicatorQPC)
QPC_8=QPC_7[QPC_7$newinIndicatorQPC==1,]



#-----------------------------------------------------------------
#=================================================================
#-----------------------------------------------------------------
#Quantity-age-type
#MORE DATA RECONSTRUCT
library(data.table)
QAT_1_DT=data.table(QAT_1)
QAT_2=QAT_1_DT[,sum(Quantity),by=c("Product.Category","Age")]
library(ggplot2)
count <- data.frame(Product.Category=factor(QAT_2$Product.Category), 
                    Quantity=QAT_2$V1, 
                    AgeGroup=factor(QAT_2$Age))
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(9, "Set1"))
ngroups <- length(unique(count$Product.Category))

p <- qplot(AgeGroup, Quantity, 
           data = count, 
           geom = "histogram", 
           stat = "bin2d",
           fill = Product.Category,
           xlab = "AgeGroup", 
           ylab = "Quantity",
           
)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 14))
p <- p + scale_fill_manual(values = cols(ngroups))
p
#-----------------------------------------------------------------


#-----------------------------------------------------------------
#Quantity-type-colour
#MORE DATA RECONSTRUCT
library(data.table)
QPC_9_DT=data.table(QPC_8)
QPC_10=QPC_9_DT[,sum(Quantity),by=c("Product.Category","colourQPC")]

library(ggplot2)
count <- data.frame(Product.Category=factor(QPC_10$Product.Category), 
                    Quantity=QPC_10$V1, 
                    Colour=factor(QPC_10$colourQPC))
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(9, "Set1"))
ngroups <- length(unique(count$Colour))

p <- qplot(Product.Category, Quantity, 
           data = count, 
           geom = "histogram", 
           stat = "bin2d",
           fill = Colour,
           xlab = "Product.Category", 
           ylab = "Quantity",
           
)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 14))
p <- p + scale_fill_manual(values = cols(ngroups))
p

#-----------------------------------------------------------------


#-----------------------------------------------------------------
#Quantity-Product-colour
#MORE DATA RECONSTRUCT
library(data.table)
QTC_6_DT=data.table(QTC_6_NEW)
QTC_7=QTC_6_DT[,sum(Quantity),by=c("ProductNames","colour")]

library(ggplot2)
count <- data.frame(Product.Category=factor(QTC_7$ProductNames), 
                    Quantity=QTC_7$V1, 
                    Colour=factor(QTC_7$colour))
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(9, "Set1"))
ngroups <- length(unique(count$Colour))

p <- qplot(Product.Category, Quantity, 
           data = count, 
           geom = "histogram", 
           stat = "bin2d",
           fill = Colour,
           xlab = "ProductNames", 
           ylab = "Quantity",
           
)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 14))
p <- p + scale_fill_manual(values = cols(ngroups))
p


#-----------------------------------------------------------------

#-----------------------------------------------------------------
#Product Ranking

Rank=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141121/MasterFile20141121.csv",header=FALSE,sep=",")

#by Product
library(data.table)
rankQTC=data.table(QTC_6_NEW)
rankQTC_1=rankQTC[,sum(Quantity),by=c("Average.Price","ProductNames")]
rankQTC_sort=rankQTC_1[order(-rankQTC_1$V1),]
rankQTC_sort
write.csv(rankQTC_sort,file="20141121 Type rank.csv")
#colourRanking
rankQTC_1=rankQTC[,sum(Quantity),by=c("colour")]
rankQTC_sort=rankQTC_1[order(-rankQTC_1$V1),]
rankQTC_sort
write.csv(rankQTC_sort,file="20141118 colour rank.csv")






