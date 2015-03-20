rdata=read.csv("C:/Users/Lovebonito/Downloads/201404/dress/dress04.csv",header=TRUE,sep=",")
colnames(rdata)
data=rdata#backup a data set
datas=data[order(data$Product.SKU),]#sorting data according to SKU
length(datas$Product)
colour=substring(datas$Product.SKU,8,10)#extract colour code
colour=data.frame(colour)
colnames(colour)="COLOUR"
class(colour)
colour[1,]
data_1=cbind(datas,colour)#add colour variables
colnames(data_1)
table(data_1$COLOUR)
data_1[1:7,]
#deleting delivery purchases
data_2=data_1[-1,]
#data_2=data_1[-c(1:6),]
#data_3=data_2[-c(1:13),]#deleting accessories
data_2[1:12,]
DESIGN=substring(data_2$Product.SKU,1,6)#extract design code
DESIGN=data.frame(DESIGN)
colnames(DESIGN)="DESIGN"
data_3=cbind(data_2,DESIGN)#add design variables
colnames(data_3)
table(data_3$COLOUR)
#find shoes bags and other accessories
#lable=substring(data_4$Product.SKU,1,1)
#lable=data.frame(lable)
#colnames(lable)="Lable"
#data_7=cbind(data_4,lable)#add lable variabel
#colnames(data_7)
#data_8=data_7[data_7$Lable=="H"|data_7$Lable=="M",]
#lable 
cc=substring(data_3$Product.SKU,9,11)
#add level
data_4=data_3
levels(data_4$COLOUR)=c(levels(data_3$COLOUR),"334","074","094")
#changing the wrong value
data_4$COLOUR[1:9]=cc[1:9]
#Removing the 0-levels
data_9=data_4
data_9$COLOUR=droplevels(data_9$COLOUR)
data_9$DESIGN=droplevels(data_9$DESIGN)
#find sum of each design
library(data.table)
data_10=data_9
data_10=data.table(data_10)
data_11=data_10[,sum(Quantity),by=c("DESIGN","Average.Price")]
colnames(data_11)
write.csv(data_11,file="dress04_d.csv")
hist(log(data_11$V1))
shapiro.test(log(data_11$V1))
