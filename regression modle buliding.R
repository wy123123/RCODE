rdata=read.csv("C:/Users/Lovebonito/Documents/dddd.csv",header=T)
datas=rdata
table(datas$DesignCode)
table(datas$STREAMLINE)
library(data.table)
#backup data
data_1=datas
colnames(data_1)
#check variables&find variables
variables=as.vector(table(data_1$X.1))
variablesNames=as.vector(data.frame(table(data_1$X.1))[,1])
#creat qualititive variables
length_DesignCodeName=length(variablesNames)
length_data=length(data_1$X.1)
temp=matrix(nrow=length_data, ncol=length_DesignCodeName)
for(i in 1 : length_DesignCodeName)
{
  for(s in 1: length_data)
  {
    if (data_1$X.1[s] == variablesNames[i])
      {temp[s,i]=1}
    else
      {temp[s,i]=0}
  }
}
#adding variable names
colnames(temp)=variablesNames
#adding variables into dataframe
data_2=cbind(data_1,temp)
colnames(data_2)
data_3=data_2[data_2$X.1=="Bottoms"|data_2$X.1=="Coat"|data_2$X.1=="Dress"|data_2$X.1=="OnePiece"|data_2$X.1=="Tops"|data_2$X.1=="Bottoms"]
#finalising data
#drop unnecessary terms
#drop one qualititive variable
data_4=subset(data_3,select=-DCV)
#attach data
attach(data_2)
model=rlm(Quantity.Sold~Price+Coat+Dress+OnePiece+Tops)
#Model selection
library(MASS)
model_1=stepAIC(model,direction=c("both"),trace=FALSE)
summary(model_1)
