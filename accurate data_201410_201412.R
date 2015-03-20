DT10_1=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Oct 2014_1.csv",header=T)
DT10_8=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Oct 2014_8.csv",header=T)
DT10_14=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Oct 2014_14.csv",header=T)
DT10_20=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Oct 2014_20.csv",header=T)
DT10_24=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Oct 2014_24.csv",header=T)
DT10_27=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Oct 2014_27.csv",header=T)
DT10_30=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Oct 2014_30.csv",header=T)
DT11_5=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Nov 2014_5.csv",header=T)
DT11_11=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Nov 2014_11.csv",header=T)
DT11_18=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Nov 2014_18.csv",header=T)
DT11_21=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Nov 2014_21.csv",header=T)
DT11_26=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Nov 2014_26.csv",header=T)
DT12_1=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Dec 2014_1.csv",header=T)
DT12_5=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Dec 2014_5.csv",header=T)
DT12_11=read.csv("C:/Users/Lovebonito/Downloads/Sales Report_Dec 2014_11.csv",header=T)
#change name
colnames(DT10_1)=letters[1:13]
colnames(DT10_8)=letters[1:13]
colnames(DT10_14)=letters[1:13]
colnames(DT10_20)=letters[1:13]
colnames(DT10_24)=letters[1:13]
colnames(DT10_27)=letters[1:13]
colnames(DT10_30)=letters[1:13]
library(data.table)
DT_10=(rbind(DT10_1,DT10_8,DT10_14,DT10_20,DT10_24,DT10_27,DT10_30))
DT_10=DT_10[as.character(DT_10$b)!="",]
DT_10=DT_10[,c(2,3,6,8,10)]
DT_10=DT_10[c(-1,-174),]
write.csv(DT_10,file="dt_10.csv")
DT_10=read.csv("C:/Users/Lovebonito/Documents/DT_10.csv",header=T)
DT_10=DT_10[,c(1:7)]
hist(DT_10$Quantity.Sold,breaks=10)
##################################
colnames(DT11_5)=letters[1:12]
colnames(DT11_11)=letters[1:12]
colnames(DT11_18)=letters[1:12]
colnames(DT11_21)=letters[1:12]
colnames(DT11_26)=letters[1:12]
DT_11=(rbind(DT11_5,DT11_11,DT11_18,DT11_21,DT11_26))
DT_11=DT_11[as.character(DT_11$b)!="",]
DT_11=DT_11[,c(2,3,7,9,11)]
write.csv(DT_11,file="dt_11.csv")
DT_11=read.csv("C:/Users/Lovebonito/Documents/DT_11.csv",header=T)
##################################
colnames(DT12_1)=letters[1:12]
colnames(DT12_5)=letters[1:12]
colnames(DT12_11)=letters[1:12]
DT_12=(rbind(DT12_1,DT12_5,DT12_11))
DT_12=DT_12[as.character(DT_12$b)!="",]
DT_12=DT_12[,c(2,3,7,9,11)]
write.csv(DT_12,file="dt_12.csv")
DT_12=read.csv("C:/Users/Lovebonito/Documents/DT_12.csv",header=T)
DT_all=rbind(DT_10,DT_11,DT_12)
write.csv(DT_all,file="DTTT.csv")
#########################################
#find design
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
Design=sapply(DT_all$Product.SKU, function(x) findDesign(x))
DT_all=cbind(DT_all,Design)
SKU_CATEGORY=read.csv("C:/Users/Lovebonito/Downloads/DT_ALL.csv",header=TRUE,sep=",")
#
#clearn data
indicatorC=substring(SKU_CATEGORY$Product.SKU,1,1)
SKU_CATEGORY_1=cbind(SKU_CATEGORY,indicatorC)
SKU_CATEGORY_2=with(SKU_CATEGORY_1,SKU_CATEGORY_1[indicatorC=="H"|indicatorC=="S"|indicatorC=="M"|indicatorC=="W"|indicatorC=="L"|indicatorC=="A",])
SKU_CATEGORY_3=SKU_CATEGORY_2[order(SKU_CATEGORY_2$Product.SKU),]
#
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
Design_2=sapply(SKU_CATEGORY_3$Product.SKU, function(x) findDesign(x))
#check 
table(Design_2)
SKU_CATEGORY_4=cbind(SKU_CATEGORY_3,Design_2)
#
#find Poduct Categoty + SKU
library(data.table)
SKU_CATEGORY_5=data.table(SKU_CATEGORY_4)
SKU_CATEGORY_6=SKU_CATEGORY_5[,sum(Quantity),by=c("Design_2","Product.Category")]

#find AGE+PRODUCT NAME
age_product_category=function(x){
  for(i in 1 : length(SKU_CATEGORY_6$Design_2))
    if(as.vector(x)==as.vector(SKU_CATEGORY_6$Design_2[i]))
      return(as.vector(SKU_CATEGORY_6$Product.Category[i]))
}
productCategory=sapply(as.vector(DT_all$Design),function(x) age_product_category(x))
productCategory[lapply(productCategory,length)==0]="NOCategory"
DT_all=data.frame(DT_all,unlist(productCategory))
write.csv(DT_all,file="dddd.csv")




####################################################################################
####################################################################################
####################################################################################
library(data.table)
DT_alll=data.table(read.csv("C:/Users/Lovebonito/Documents/ddddddd.csv",header=T))

qplot(Price,Quantity.Sold,colour=unlist.productCategory.,data=DT_alll)

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
Design=sapply(DT_alll$Product.SKU, function(x) findDesign(x))
DT_alll$Design=Design
#find category
findCat=strsplit(as.vector(DT_alll$Product.name)," ")
a=unlist(lapply(findCat, length))
Type=rep(NA,length(findCat))
for(i in 1 : length(findCat)){
  Type[i]=findCat[[i]][a[i]]
}
table(Type)
DT_alll$unlist.productCategory.=Type
DT_alll[DT_alll$unlist.productCategory.=="\nTop",]$unlist.productCategory.="Top"
table(DT_alll$unlist.productCategory.)
write.csv(DT_alll,file="ddddddd.csv",row.names = FALSE)

#########################################
library(ggplot2)
DT_alll=data.table(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/TEST/DTTT.csv",header=T))
#find category
findCat=strsplit(as.vector(DT_alll$Product.name)," ")
a=unlist(lapply(findCat, length))
Type=rep(NA,length(findCat))
for(i in 1 : length(findCat)){
        Type[i]=findCat[[i]][a[i]]
}
table(Type)
DT_alll$unlist.productCategory.=Type
DT_alll[DT_alll$unlist.productCategory.=="\nTop",]$unlist.productCategory.="Top"
table(DT_alll$unlist.productCategory.)

dt_tops=DT_alll[DT_alll$unlist.productCategory.=="Tank"|DT_alll$unlist.productCategory.=="Tee"|DT_alll$unlist.productCategory.=="Top"|DT_alll$unlist.productCategory.=="Vest"|DT_alll$unlist.productCategory.=="Blouse",]
dt_tops=DT_alll[DT_alll$unlist.productCategory.=="Dress",]
dt_tops=DT_alll[DT_alll$unlist.productCategory.=="Romper"|DT_alll$unlist.productCategory.=="Jumpsuit",]
dt_tops=DT_alll[DT_alll$unlist.productCategory.=="Blazer"|DT_alll$unlist.productCategory.=="Coat"|DT_alll$unlist.productCategory.=="Jacket",]
dt_tops=DT_alll[DT_alll$unlist.productCategory.=="Skirt"|DT_alll$unlist.productCategory.=="Shorts"|DT_alll$unlist.productCategory.=="Pants"|DT_alll$unlist.productCategory.=="Jeans"|DT_alll$unlist.productCategory.=="Skorts",]

class(dt_tops$Quantity.Sold)
t.test(dt_tops$Quantity.Sold,DT_alll[DT_alll$unlist.productCategory.=="Pants",]$Quantity.Sold)

ggplot(dt_tops,aes(x=dt_tops$Quantity.Sold)+ 
    geom_histogram(aes(y = ..density..),colour="black",fill="white",binwidth = 20) +
    geom_density(alpha=0.3,fill="blue")+
    xlab("Q.Sold")

shapiro.test(dt_tops$Quantity.Sold)
hist(dt_tops$Quantity.Sold,breaks=20)
shapiro.test(log10(dt_tops$Quantity.Sold))
hist(log10(dt_tops$Quantity.Sold),breaks=20)
ggplot(dt_tops,aes(x=log10(dt_tops$Quantity.Sold)))+ 
               geom_histogram(aes(y = ..density..),colour="black",fill="green",binwidth=0.05) +
               geom_density(alpha=0.3,fill="blue")+
               xlab("Q.Sold")

dt_tops$month=as.factor(dt_tops$month)
p=ggplot(data=dt_tops,aes(y=Quantity.Sold,x=seq(from=1,to=length(dt_tops$month))
                          ,colour=month,shape=month))+geom_point(size=5)+
        xlab("Launch Index")
        
p=p+scale_fill_discrete(name="month",
                        breaks=c("10", "11", "12","1","2"),
                        labels=c("2014-10", "2014-11", "2014-12","2015-1","2015-2"))
p=p+geom_hline(yintercept=200,size=1.5,color="green")
p=p+geom_hline(yintercept=250,size=1.5,color="red")
p=p+ theme(legend.title = element_text(size=16, face="bold"))+
        theme(legend.text = element_text(size = 16, face = "bold"))
p
#if data is normal
m=mean(log10(dt_tops$Quantity.Sold))
sd=sd(log10(dt_tops$Quantity.Sold))
#confideceinteval
CI=c(m-1.96*sd/(sqrt(length(dt_tops$Quantity.Sold))),m+1.96*sd/(sqrt(length(dt_tops$Quantity.Sold))))
10^CI
10^m
mean(dt_tops$Quantity.Sold)
log10(200)
median(dt_tops$Quantity.Sold)
mode(dt_tops$Quantity.Sold)
upper=m+3*sd
dt_tops=dt_tops[dt_tops$Quantity.Sold<=upper,]
hist(dt_tops$Quantity.Sold,breaks=20)
shapiro.test(dt_tops$Quantity.Sold)

require(vcd)
require(MASS)
fit1 <- fitdistr(dt_tops$Quantity.Sold, "exponential") 
ks.test(dt_tops$Quantity.Sold, "pexp") # p-value > 0.05 -> distribution not refused
shapiro.test(dt_tops$Quantity.Sold)
fit1


# estimate the parameters
fit1 <- fitdistr(dt_tops$Quantity.Sold, "gamma",start=list(shape=1,rate=0.1),lower=0.001) 
Q=fit1$estimate[1]
R=fit1$estimate[2]
# goodness of fit test
ks.test(dt_tops$Quantity.Sold, "pgamma",shape=Q,rate=R) # p-value > 0.05 -> distribution not refused
# plot a graph
hist(dt_tops$Quantity.Sold, freq = FALSE, breaks = 20, xlim = c(0, quantile(dt_tops$Quantity.Sold, 0.99)),main="Dresses",xlab="Quantity")
curve(dgamma(x, shape=Q,rate =R), col = "red", add = TRUE)
Q*(1/R)
sqrt(Q)*(1/R)
Tops=qgamma(c(0.2,0.3,0.4,0.5,0.6,0.65,0.7,0.75,0.8,0.85),shape=Q,rate=R)
Tops
#Expectations
#pgamma(230,shape=Q,rate=R)
qgamma(c(0.1,0.95),shape=Q,rate=R)





