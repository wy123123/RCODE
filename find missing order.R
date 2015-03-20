library(data.table)
dt=data.table(read.csv("C:/Users/Lovebonito/Desktop/missing orders/11 feb.csv",header=F))
dt2=data.table(read.csv("C:/Users/Lovebonito/Downloads/11 feb.csv",header=T))
dt4=dt2[!is.na(dt2$Transaction.ID),]
dt3=dt[as.character(substring(dt$V6,1,1))!="C",]
findMissingOrder= function(x){
  for(i in 1: length(dt4$Transaction.ID)){
    if(x==dt4$Transaction.ID[i])
      return (1)
    else 
      return (0)
  }
}

#a=rep(0,length(dt3$V1))
#for(s in 1:length(dt3$V1)){
#  for(i in 1: length(dt4$Transaction.ID)){
#    if(dt3$V1[s]==as.vector(dt4$Transaction.ID[i]))
#      a[s]=1
#  }
#}

a=data.table(dt3[!dt3$V1 %in% dt4$Transaction.ID])
b=dt3[dt3$V1 %in% dt4$Transaction.ID]

s=sample(b$V1,5)
d=b[match(s,b$V1),]
write.csv(d,file="C:/Users/Lovebonito/Desktop/missing orders/m.csv")

table(b$V6)
DT=cbind(dt3,a)
missingorder=DT[a==0,]
write.csv(missingorder,file="missingorder.csv")
table(dt$V5)
