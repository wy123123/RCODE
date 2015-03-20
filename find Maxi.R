#read data
data11=read.csv("C:/Users/Lovebonito/Downloads/201411/MF11.csv")
data10=read.csv("C:/Users/Lovebonito/Downloads/201410/MF10.csv")
data09=read.csv("C:/Users/Lovebonito/Downloads/201409/MF09.csv")
data08=read.csv("C:/Users/Lovebonito/Downloads/201408/MF08.csv")
data07=read.csv("C:/Users/Lovebonito/Downloads/201407/MF07.csv")
data06=read.csv("C:/Users/Lovebonito/Downloads/201406/MF06.csv")
data05=read.csv("C:/Users/Lovebonito/Downloads/201405/MF05.csv")
data04=read.csv("C:/Users/Lovebonito/Downloads/201404/MF04.csv")
data03=read.csv("C:/Users/Lovebonito/Downloads/201403/MF03.csv")
data02=read.csv("C:/Users/Lovebonito/Downloads/201402/MF02.csv")
data01=read.csv("C:/Users/Lovebonito/Downloads/201401/MF01.csv")
library(data.table)
data_MF=data.table(rbind(data11,data10,data09,data08,data07,data06,data05,data04,data03,data02,data01))
table(data_MF$Category)
QTC_4=data_MF
QTC_5=QTC_4[,sum(Quantity),by=c("ProductCode","ProductName","Category")]
QTC_P=QTC_4[,sum(Sales),by=c("ProductCode","ProductName","Category")]
Price=QTC_P$V1/QTC_5$V1
Price=round(Price*1.07)
QTC_ALL=cbind(QTC_5,Price)
ss=sapply(QTC_ALL$ProductName, function(x) substring(x,1,2))
QTC_ALL=cbind(QTC_ALL,ss)
QTC_ALL=QTC_ALL[QTC_ALL$ss!="HY",]

#QTC_6=QTC_5[Category=="Dresses",]
findMaxi=strsplit(as.vector(QTC_ALL$ProductName)," ")
a=unlist(lapply(findMaxi, length))
Type=rep(NA,length(findMaxi))
for(i in 1 : length(findMaxi)){
        Type[i]=findMaxi[[i]][a[i]]
}
QTC_7=cbind(QTC_ALL,Type)
DT=read.csv("C:/Users/Lovebonito/Documents/aaaaa.csv",header=T)
table(DT$Type)
QTC_8=DT[DT$Type=="Blazer"|DT$Type=="Jacket"|DT$Type=="Coat",]
QTC_8=QTC_8[QTC_8$Quantity<600,]
hist(QTC_8$Quantity,breaks=50)
shapiro.test(QTC_8$Quantity)

# estimate the parameters
fit1 <- fitdistr(QTC_8$Quantity, "gamma",start=list(shape=1,rate=0.1),lower=0.001) 
Q=fit1$estimate[1]
R=fit1$estimate[2]
# goodness of fit test
ks.test(QTC_8$Quantity, "pgamma",shape=Q,rate=R) # p-value > 0.05 -> distribution not refused
# plot a graph
hist(QTC_8$Quantity, freq = FALSE, breaks = 20, xlim = c(0, quantile(QTC_8$Quantity, 0.99)),main="Dresses",xlab="Quantity")
curve(dgamma(x, shape=Q,rate =R), col = "red", add = TRUE)
Q*(1/R)
sqrt(Q)*(1/R)
Dress=qgamma(c(0.2,0.3,0.4,0.5,0.6,0.65,0.7,0.75,0.8,0.85),shape=Q,rate=R)


288.70+1.96*(160/(sqrt(24)))
288.70-1.96*(160/(sqrt(24)))
mean(QTC_8$Quantity)
sd(QTC_8$Quantity)
max(QTC_8$Quantity)
############
#further breakdown
#dress
DT_DRESS=DT[DT$Type=="Dress",]
findType=strsplit(as.vector(DT_DRESS$ProductName)," ")
a=unlist(lapply(findType, length))-1
TypeD=rep(NA,length(findType))
for(i in 1 : length(findType)){
        TypeD[i]=findType[[i]][a[i]]
}
table(TypeD)
DT_DRESS=cbind(DT_DRESS,TypeD)
