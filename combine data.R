#combine the two set of data and find the new items
data04=read.csv("C:/Users/Lovebonito/Downloads/201404/dress/dress04_d.csv",header=TRUE,sep=",")
data05=read.csv("C:/Users/Lovebonito/Downloads/201405/dress/dress05_sorted_2.csv",header=TRUE,sep=",")
colnames(data04)
colnames(data05)
#drop column 1
data05=subset(data05,select=-X.1)
#ADD A COL DesignCode
l1=length(data05$DESIGN)
l2=length(data04$DESIGN)
tempCol=matrix(ncol=1,nrow=l2)
colnames(tempCol)="DesignCode"
data04=cbind(data04,tempCol)
#check
colnames(data04)
a=as.vector(data05$DESIGN)
c=as.vector(data05$DesignCode)
b=as.vector(data04$DESIGN)

for(i in 1 : l1){
  for(s in 1 : l2){
    if(b[s]==a[i])
      data04$DesignCode[s]=c[i]
  }
}
table(data04$DesignCode)
colnames(data04)
write.csv(data04,file="dress04_sorted_1.csv")

