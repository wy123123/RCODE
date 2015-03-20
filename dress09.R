rdata=read.csv("C:/Users/Lovebonito/Downloads/201409/dress/dress09.csv",header=TRUE,sep=",")
colnames(rdata)
data=rdata#backup a data set
datas=data[order(data$Product.SKU),]#sorting data according to SKU
length(datas$Product)
#deleteing first line
datas=datas[-1,]
#extract colour code
colour=substring(datas$Product.SKU,8,10)
colour=data.frame(colour)
colnames(colour)="COLOUR"
class(colour)
colour[1,]
#add colour variables
data_1=cbind(datas,colour)
colnames(data_1)
table(data_1$COLOUR)
data_1[1:8,]
#extract design code
DESIGN=substring(data_1$Product.SKU,1,6)
DESIGN=data.frame(DESIGN)
colnames(DESIGN)="DESIGN"
data_2=cbind(data_1,DESIGN)#add design variables
colnames(data_2)
table(data_2$COLOUR)
#lable
cc=substring(data_2$Product.SKU,9,11)
#add level
data_3=data_2
levels(data_3$COLOUR)=c(levels(data_5$COLOUR),"334")
#changing the wrong value
data_3=data_2
data_3$COLOUR[1:7]=cc[1:7]
#Removing the 0-levels
data_4=data_3
data_4$COLOUR=droplevels(data_4$COLOUR)
data_4$DESIGN=droplevels(data_4$DESIGN)
#find sum of each design
library(data.table)
data_5=data_4
data_6=data.table(data_5)
data_7=data_6[,sum(Quantity),by=c("DESIGN","Average.Price")]
#write data to a csv file
write.csv(data_7,file="dress09DC.csv")
