rdata=read.csv("C:/Users/Lovebonito/Downloads/201410/dress/dress10.csv",header=TRUE,sep=",")
colnames(rdata)
#backup a data set
data=rdata
#sorting data according to SKU
datas=data[order(data$Product.SKU),]
length(datas$Product)
colour=substring(datas$Product.SKU,8,10)#extract colour code
colour=data.frame(colour)
colnames(colour)="COLOUR"
class(colour)
colour[1,]
data_1=cbind(datas,colour)#add colour variables
colnames(data_1)
table(data_1$COLOUR)
DESIGN=substring(data_1$Product.SKU,1,6)#extract design code
DESIGN=data.frame(DESIGN)
colnames(DESIGN)="DESIGN"
data_2=cbind(data_1,DESIGN)#add design variables
colnames(data_2)
##data_4[1:40,]
table(data_2$COLOUR)
#find shoes bags and other accessories
lable=substring(data_2$Product.SKU,1,1)
lable=data.frame(lable)
colnames(lable)="Lable"
data_3=cbind(data_2,lable)#add lable variabel
colnames(data_3)
data_4=data_3[data_3$Lable=="H"|data_3$Lable=="M",]
#lable
cc=substring(data_4$Product.SKU,9,11)
#add level
data_5=data_4
levels(data_5$COLOUR)=c(levels(data_5$COLOUR),"334")
#changing the wrong value
data_5$COLOUR[1:8]=cc[1:8]
#Removing the 0-levels
data_6=data_5
data_6$COLOUR=droplevels(data_6$COLOUR)
data_6$DESIGN=droplevels(data_6$DESIGN)
#find sum of each design
library(data.table)
data_7=data_6
data_7=data.table(data_7)
data_8=data_7[,sum(Quantity),by=c("DESIGN","Average.Price")]
