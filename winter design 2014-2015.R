#
dt=read.csv("C:/Users/Lovebonito/Desktop/2014-2015 winter season/dt.csv")
newItem=read.csv("C:/Users/Lovebonito/Desktop/2014-2015 winter season/newItem.csv",header=F)
a=as.vector(rep(0,length(dt$ProductName)))
newItem$V1=as.vector(newItem$V1)
class(newItem$V1)
class(dt$ProductName)
dt$ProductName=as.vector(dt$ProductName)

for(i in 1 :length(newItem$V1)){
  for(s in 1: length(dt$ProductName)){
    if(newItem$V1[i]==dt$ProductName[s]){
      a[s]= 1
    }
  }
}
dt.1=dt[a==1,]

#############################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2015/5 jan.csv",header=F))
d=rbind(d,as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2015/9 jan.csv",header=F)))
d=rbind(d,as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2015/15 jan.csv",header=F)))
d=rbind(d,as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2015/22 jan.csv",header=F)))
d=rbind(d,as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2015/27 jan.csv",header=F)) )            

d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total Rev.",]

a=d[d$V6=="Quantity"|d$V6=="Q.Sold",]
a.1=subset(a,select=c(V7,V8,V9,V10,V11))
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V8=as.numeric(as.character(a.1$V8))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1$V10=as.numeric(as.character(a.1$V10))
a.1$V11=as.numeric(as.character(a.1$V11))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
k=rowSums(a.1)
k=matrix(k,ncol=2,byrow=T)
d.jan=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=k[,1],Q.Sold=k[,2],Per_sold=round(k[,2]/k[,1]*100,1),Month=rep("Jan",length(d.9j$V3)))
#############################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Dec 2014/1 Dec.csv",header=F))
d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total Rev.",]
a=d[d$V6!=""&d$V6!="Size",]
a.1=subset(a,select=c(V6,V7,V9))
a.1$V6=as.character(a.1$V6)
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
a.3=a.1[1:120,]
a.3=t(a.3)
a.3.Quantity=matrix(as.numeric(as.character(a.3[2,])),ncol=5,byrow=T)
a.3.Sold=matrix(as.numeric(as.character(a.3[3,])),ncol=5,byrow=T)
a.3.Quantity.sum=rowSums(a.3.Quantity)
a.3.Sold.sum=rowSums(a.3.Sold)
a.4=a.1[121:134,]
a.4=t(a.4)
a.4.Quantity=matrix(as.numeric(as.character(a.4[2,])),ncol=7,byrow=T)
a.4.Sold=matrix(as.numeric(as.character(a.4[3,])),ncol=7,byrow=T)
a.4.Quantity.sum=rowSums(a.4.Quantity)
a.4.Sold.sum=rowSums(a.4.Sold)
Quantity=c(as.vector(a.3.Quantity.sum),as.vector(a.4.Quantity.sum),49);Q.Sold=c(a.3.Sold.sum,a.4.Sold.sum,49)
Q
d.D.1=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=c(a.3.Quantity.sum,a.4.Quantity.sum,49),Q.Sold=c(a.3.Sold.sum,a.4.Sold.sum,49),Per_sold=round(Q.Sold/Quantity*100,1),Month=rep("Dec",length(d.9j$V3)))
#############################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Dec 2014/5 Dec.csv",header=F))
d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total:",]
a=d[d$V6!=""&d$V6!="Size",]
a.1=subset(a,select=c(V6,V7,V9))
a.1$V6=as.character(a.1$V6)
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
a.2=a.1[1:145,]
a.2=t(a.2)
Quantity=matrix(as.numeric(as.character(a.2[2,])),ncol=5,byrow=T)
Quantity.sum=rowSums(Quantity)
Sold=matrix(as.numeric(as.character(a.2[3,])),ncol=5,byrow=T)
Sold.sum=rowSums(Sold)
d.D.5=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=c(Quantity.sum,49,49,9),Q.Sold=c(Sold.sum,30,15,9),Per_sold=round(c(Sold.sum/Quantity.sum,30/49,15/49,1)*100,1),Month=rep("Dec",length(d.9j$V3)))
#############################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Dec 2014/11 Dec.csv",header=F))
d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total:",]
a=d[d$V6!=""&d$V6!="Size",]
a.1=subset(a,select=c(V6,V7,V9))
a.1$V6=as.character(a.1$V6)
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
a.1=t(a.1)
Quantity=matrix(as.numeric(as.character(a.1[2,])),ncol=5,byrow=T)
Quantity.sum=rowSums(Quantity)
Sold=matrix(as.numeric(as.character(a.1[3,])),ncol=5,byrow=T)
Sold.sum=rowSums(Sold)
d.D.11=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=c(Quantity.sum),Q.Sold=c(Sold.sum),Per_sold=round(c(Sold.sum/Quantity.sum)*100,1),Month=rep("Dec",length(d.9j$V3)))
################################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Dec 2014/17 Dec.csv",header=F))
d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total:",]
a=d[d$V6!=""&d$V6!="Size"&d$V6!="Total",]
a.1=subset(a,select=c(V6,V7,V9))
a.1$V6=as.character(a.1$V6)
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
a.2=a.1[1:140,]
a.2=t(a.2)
Quantity=matrix(as.numeric(as.character(a.2[2,])),ncol=5,byrow=T)
Quantity.sum=rowSums(Quantity)
Sold=matrix(as.numeric(as.character(a.2[3,])),ncol=5,byrow=T)
Sold.sum=rowSums(Sold)
d.D.17=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=c(Quantity.sum,140),Q.Sold=c(Sold.sum,118),Per_sold=round(c(Sold.sum/Quantity.sum,118/140)*100,1),Month=rep("Dec",length(d.9j$V3)))
################################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Dec 2014/22 Dec.csv",header=F))
d=rbind(d,as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Dec 2014/30 Dec.csv",header=F)))
d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total Rev.",]

a=d[d$V6=="Quantity"|d$V6=="Q.Sold",]
a.1=subset(a,select=c(V7,V8,V9,V10,V11))
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V8=as.numeric(as.character(a.1$V8))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1$V10=as.numeric(as.character(a.1$V10))
a.1$V11=as.numeric(as.character(a.1$V11))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
k=rowSums(a.1)
k=matrix(k,ncol=2,byrow=T)
d.D.22=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=k[,1],Q.Sold=k[,2],Per_sold=round(k[,2]/k[,1]*100,1),Month=rep("Dec",length(d.9j$V3)))
d.dec=rbind(d.D.1,d.D.5,d.D.11,d.D.17,d.D.22)
d.Dec.Jan=rbind(d.dec,d.jan)
write.csv(d.Dec.Jan,file="C:/Users/Lovebonito/Desktop/2014-2015 winter season/Dec-jan.csv",row.names=F)
################################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Nov 2014/Sales Report_Nov 2014_5.csv",header=F))
d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total:",]
a=d[d$V6!=""&d$V6!="Size"&d$V6!="Total",]
a.1=subset(a,select=c(V6,V7,V9))
a.1$V6=as.character(a.1$V6)
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
a.2=a.1[1:150,]
a.2=t(a.2)
Quantity=matrix(as.numeric(as.character(a.2[2,])),ncol=5,byrow=T)
Quantity.sum=rowSums(Quantity)
Sold=matrix(as.numeric(as.character(a.2[3,])),ncol=5,byrow=T)
Sold.sum=rowSums(Sold)
d.n.5=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=c(Quantity.sum,39,36,28,38),Q.Sold=c(Sold.sum,17,13,2,12),Per_sold=round(c(Sold.sum/Quantity.sum,17/39,13/36,2/28,12/38)*100,1),Month=rep("Nov",length(d.9j$V3)))
####################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Nov 2014/Sales Report_Nov 2014_11.csv",header=F))
d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total:",]
a=d[d$V6!=""&d$V6!="Size"&d$V6!="Total",]
a.1=subset(a,select=c(V6,V7,V9))
a.1$V6=as.character(a.1$V6)
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
a.2=a.1[1:130,]
a.2=t(a.2)
Quantity=matrix(as.numeric(as.character(a.2[2,])),ncol=5,byrow=T)
Quantity.sum=rowSums(Quantity)
Sold=matrix(as.numeric(as.character(a.2[3,])),ncol=5,byrow=T)
Sold.sum=rowSums(Sold)
d.n.11=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=c(Quantity.sum,49,49),Q.Sold=c(Sold.sum,15,15),Per_sold=round(c(Sold.sum/Quantity.sum,15/49,15/49)*100,1),Month=rep("Nov",length(d.9j$V3)))
####################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Nov 2014/Sales Report_Nov 2014_18.csv",header=F))
d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total:",]
a=d[d$V6!=""&d$V6!="Size"&d$V6!="Total",]
a.1=subset(a,select=c(V6,V7,V9))
a.1$V6=as.character(a.1$V6)
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
a.2=a.1
a.2=t(a.2)
Quantity=matrix(as.numeric(as.character(a.2[2,])),ncol=5,byrow=T)
Quantity.sum=rowSums(Quantity)
Sold=matrix(as.numeric(as.character(a.2[3,])),ncol=5,byrow=T)
Sold.sum=rowSums(Sold)
d.n.18=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=c(Quantity.sum),Q.Sold=c(Sold.sum),Per_sold=round(c(Sold.sum/Quantity.sum)*100,1),Month=rep("Nov",length(d.9j$V3)))
####################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Nov 2014/Sales Report_Nov 2014_21.csv",header=F))
d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total:",]
a=d[d$V6!=""&d$V6!="Size"&d$V6!="Total",]
a.1=subset(a,select=c(V6,V7,V9))
a.1$V6=as.character(a.1$V6)
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
a.2=a.1[1:120,]
a.2=t(a.2)
Quantity=matrix(as.numeric(as.character(a.2[2,])),ncol=5,byrow=T)
Quantity.sum=rowSums(Quantity)
Sold=matrix(as.numeric(as.character(a.2[3,])),ncol=5,byrow=T)
Sold.sum=rowSums(Sold)
a.3=a.1[121:141,]
a.3=t(a.3)
a.3.Quantity=matrix(as.numeric(as.character(a.3[2,])),ncol=7,byrow=T)
a.3.Sold=matrix(as.numeric(as.character(a.3[3,])),ncol=7,byrow=T)
a.3.Quantity.sum=rowSums(a.3.Quantity)
a.3.Sold.sum=rowSums(a.3.Sold)
d.n.21=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=c(Quantity.sum,a.3.Quantity.sum),Q.Sold=c(Sold.sum,a.3.Sold.sum),Per_sold=round(c(Sold.sum/Quantity.sum,a.3.Sold.sum/a.3.Quantity.sum)*100,1),Month=rep("Nov",length(d.9j$V3)))
####################################################################################
d=as.data.frame(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/2014/Nov 2014/Sales Report_Nov 2014_26.csv",header=F))
d.9j=d[d$V3!="",]
d.9j=d.9j[d.9j$V3!="Item No.",];d.9j=d.9j[d.9j$V3!="Total:",]
a=d[d$V6!=""&d$V6!="Size"&d$V6!="Total",]
a.1=subset(a,select=c(V6,V7,V9))
a.1$V6=as.character(a.1$V6)
a.1$V7=as.numeric(as.character(a.1$V7))
a.1$V9=as.numeric(as.character(a.1$V9))
a.1=as.data.frame(a.1);a.1[is.na(a.1)]=0
a.2=a.1
a.2=t(a.2)
Quantity=matrix(as.numeric(as.character(a.2[2,])),ncol=5,byrow=T)
Quantity.sum=rowSums(Quantity)
Sold=matrix(as.numeric(as.character(a.2[3,])),ncol=5,byrow=T)
Sold.sum=rowSums(Sold)
d.n.26=data.frame(SKU=d.9j$V3,color=d.9j$V4,Quantity=c(Quantity.sum),Q.Sold=c(Sold.sum),Per_sold=round(c(Sold.sum/Quantity.sum)*100,1),Month=rep("Nov",length(d.9j$V3)))
####################################################################################
t=rbind(d.n.5,d.n.11,d.n.18,d.n.21,d.n.26)
write.csv(t,file="C:/Users/Lovebonito/Desktop/2014-2015 winter season/Nov.csv",row.names=F)
####################################################################################
dt=read.csv("C:/Users/Lovebonito/Desktop/2014-2015 winter season/Nov-Jan.csv")

write.csv(dt,file="C:/Users/Lovebonito/Desktop/2014-2015 winter season/Nov-Jan.csv",row.names=F)


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
Design=sapply(dt$SKU, function(x) findDesign(x))
dt=cbind(Design,dt)
table(droplevels(dt$color))
#find names
n=read.csv("C:/Users/Lovebonito/Downloads/launch sales report/TEST/DTTT.csv")
Design=sapply(n$Product.SKU, function(x) findDesign(x))
n=cbind(Design,n)
Name=droplevels(n$Product.name[match(dt$Design,n$Design)])
dt=cbind(dt,Name)
#find Category
findCategory=strsplit(as.vector(dt$Name)," ")
a=unlist(lapply(findCategory, length))
Type=rep(NA,length(findCategory))
for(i in 1 : length(findCategory)){
  Type[i]=findCategory[[i]][a[i]]
}
require(data.table)
dt=data.table(cbind(dt,Type))
#########################
#overall
dt=data.table(dt)
overall=dt[,lapply(.SD,sum,na.rm=TRUE),by=Type,.SDcols=c("Quantity","Q.Sold")]
overall1=dt[,lapply(.SD,sum,na.rm=TRUE),by=list(Type,Design),.SDcols=c("Quantity","Q.Sold")]
overall=overall[order(overall$Type)];overall1=overall1[order(overall1$Type)]
overall=cbind(overall,per_sold=round(overall$Q.Sold/overall$Quantity*100,1),No_of_item=unlist(table(overall1$Type)),Avg_per_design=round(overall$Q.Sold/unlist(table(overall1$Type))))
overall=overall[order(-overall$per_sold)]
write.csv(overall,file="C:/Users/Lovebonito/Desktop/2014-2015 winter season/overall.csv",row.names=F)
