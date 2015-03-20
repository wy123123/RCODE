data10=read.csv("C:/Users/Lovebonito/Downloads/201410/dress/dress201410_sorted_dontnotdelete_1.csv",header=TRUE,sep=",")
data09=read.csv("C:/Users/Lovebonito/Downloads/201409/dress/data09_sorted_1.csv",header=TRUE,sep=",")
data_1=rbind(data10,subset(data09,select=-X.1))
library(data.table)
data_2=data_1
data_3=data.table(data_2)
colnames(data_4)
data_4=data_3[,sum(Quantity),by=c("DESIGN","Average.Price","DesignCode")]
#model building
data_5=data_4
table(data_5$DesignCode)
table(datas_5STREAMLINE)
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
model=lm(V1~Average.Price+DPE+DSH+DSK+DSL+DST+DTB+DTG+MAX+MLS+MSL,data=data_7)
#Model selection
library(MASS)
model_1=stepAIC(model,direction=c("both"),trace=FALSE)
summary(model_1)
hist(data_8$V1)
hist(log(data_8$V1),breaks=48)
