data08_f=read.csv("C:/Users/Lovebonito/Downloads/201408/dress/dress08_sorted_1.csv",header=TRUE,sep=",")
data10_f=read.csv("C:/Users/Lovebonito/Downloads/201410/dress/dress201410_sorted_dontnotdelete_1.csv",header=TRUE,sep=",")
data09_f=read.csv("C:/Users/Lovebonito/Downloads/201409/dress/dress09_sorted_2.csv",header=TRUE,sep=",")
data07_f=read.csv("C:/Users/Lovebonito/Downloads/201407/dress/dress07_sorted_2.csv",header=TRUE,sep=",")
data06_f=read.csv("C:/Users/Lovebonito/Downloads/201406/dress/dress06_sorted_2.csv",header=TRUE,sep=",")
data05_f=read.csv("C:/Users/Lovebonito/Downloads/201405/dress/dress05_sorted_2.csv",header=TRUE,sep=",")
data04_f=read.csv("C:/Users/Lovebonito/Downloads/201404/dress/dress04_sorted_2.csv",header=TRUE,sep=",")

#check colnames
colnames(data10_f)
data10_f=subset(data10_f,select=-STREAMLINE)
colnames(data09_f)
data09_f=subset(data09_f,select=-X.1)
data09_f=subset(data09_f,select=-STREAMLINE)
colnames(data08_f)
colnames(data07_f)
data07_f=subset(data07_f,select=-X.1)
colnames(data06_f)
data06_f=subset(data06_f,select=-X.1)
colnames(data05_f)
data05_f=subset(data05_f,select=-X.1)
colnames(data04_f)
data04_f=subset(data04_f,select=-X.1)

#change colnames
colnames(data04_f)=c("X","DESIGN","Average.Price","Quantity","DesignCode")
colnames(data07_f)=c("X","DESIGN","Average.Price","Quantity","DesignCode")
colnames(data06_f)=c("X","DESIGN","Average.Price","Quantity","DesignCode")
colnames(data05_f)=c("X","DESIGN","Average.Price","Quantity","DesignCode")
colnames(data08_f)=c("X","DESIGN","Average.Price","Quantity","DesignCode")
#adding data
#backup
data_f=rbind(data10_f,data09_f)
data_2f=rbind(data_f,data08_f)
data_3f=rbind(data_2f,data07_f)
#backup
data_4f=data_3f
data_5f=rbind(data_4f,data06_f)
data_6f=rbind(data_5f,data05_f)
data_7f=rbind(data_6f,data04_f)

#----------------------------------------------------------------------------

#building the model
library(data.table)
data_2=data_7f
data_3=data.table(data_2)
colnames(data_3)
data_4=data_3[,sum(Quantity),by=c("DESIGN","DesignCode")]
#model building
data_5=data_4
table(data_5$DesignCode)
library(data.table)
#backup data
data_6=data_5
colnames(data_6)
#check variables&find variables
variables=as.vector(table(data_6$DesignCode))
variablesNames=as.vector(data.frame(table(data_6$DesignCode))[,1])
#creat qualititive variables
length_DesignCodeName=length(variablesNames)
length_data=length(data_6$DesignCode)
temp=matrix(nrow=length_data, ncol=length_DesignCodeName)
for(i in 1 : length_DesignCodeName)
{
  for(s in 1: length_data)
  {
    if (data_6$DesignCode[s] == variablesNames[i])
    {temp[s,i]=1}
    else
    {temp[s,i]=0}
  }
}
#adding variable names
colnames(temp)=variablesNames
#adding variables into dataframe
data_7=cbind(data_6,temp)
colnames(data_7)
#finalising data
#drop one qualititive variable
data_8=subset(data_7,select=-DCV)
#build a linear model
model=lm(log(V1)~DPE+DSH+DSK+DSL+DST+DTB+DTG+MAX+MLS+MSL,data=data_7)
#Model selection
library(MASS)
model_1=stepAIC(model,direction=c("both"),trace=FALSE)
summary(model_1)

hist(data_7$V1,breaks=100,xlim=c(0,600),ylim=c(0,250),xlab="Quantity",col="blue")

plot(model_1$fitted.values,log(data_7$V1))
hist
#---------------------------------------
#analysis
#what happens to the product sells less than 20 pieces a year
QL20=data_7[data_7$V1<=20,]
QL20=data.table(QL20)
Percentage=with(QL20,(QL20[,sum(V1),by=DesignCode]))$V1
percentage=sapply(Percentage,function(x) x/sum(Percentage)*100)
QL20=cbind(with(QL20,(QL20[,sum(V1),by=DesignCode])),round(percentage,2))
QL20=QL20[order(QL20$DesignCode),]
#---------------------------------------
#OverALL analysis Dress
OA=data.table(data_7)
Percentage=with(OA,(OA[,sum(V1),by=DesignCode]))
percentage=with(Percentage,sapply(V1,function(x) x/sum(V1)*100))
Percentage=cbind(Percentage,percentage)
Percentage=Percentage[order(Percentage$DesignCode),]
QL20=cbind(QL20,round(Percentage$percentage[1:9],2))

#---------------------------------------
t1=data.frame(table(data_7$DesignCode))
t2=as.vector(t1$Freq)
percentage=sapply(t2,function(x) x/sum(t2)*100)
p2=round(percentage,2)
t1=cbind(t1,p2)
t1=t1[order(t1$Var1),]
QL20=cbind(QL20,t1$p2[1:9])
colnames(QL20)=c("DesignCode","Quantity"," %totalQuantity"," Overall%of Quantity","Overall % DesignType")

hist(QL20$V1)
hist(as.vector(QL20$DesignCode))
table(QL20,by=QL20$DesignCode)
