#read data
data12=read.csv("C:/Users/Lovebonito/Downloads/201412/MF12.csv")
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
data_MF=data.table(rbind(data12,data11,data10,data09,data08,data07,data06,data05,data04,data03,data02,data01))
table(data_MF$Category)
QTC_4=data_MF[data_MF$Category=="Tops",]
QTC_4=data_MF[data_MF$Category=="Flats"|data_MF$Category=="Heels"|data_MF$Category=="Wedges"|data_MF$Category=="Boots",]
QTC_4=data.table(QTC_4)
QTC_5=QTC_4[,sum(Quantity),by=c("ProductCode")]
m=mean(QTC_5$V1)
sd=sd(QTC_5$V1)
median(QTC_5$V1)
QTC_5=QTC_5[QTC_5$V1<m+3*sd,]
hist(QTC_5$V1,breaks=50,freq=T,xlim=c(0,500))
hist(QTC_5$V1,breaks=50,freq=T)
QTC_5=QTC_5[QTC_5$V1>20&QTC_5$V1<1000,]
hist(QTC_5$V1,breaks=50,freq=F)

shapiro.test(QTC_5$V1)

require(vcd)
require(MASS)
fit1 <- fitdistr(QTC_5$V1, "exponential",lower=0.001) 
shapiro.test(QTC_5$V1)
ks.test(QTC_5$V1, "pexp",rate=fit1$estimate) # p-value > 0.05 -> distribution not refused
qexp(0.65,rate=fit1$estimate)
#Expectations
1-pgamma(591,shape=Q,rate=R)


# estimate the parameters
fit1 <- fitdistr(QTC_5$V1, "gamma",start=list(shape=1,rate=0.1),lower=0.001) 
Q=fit1$estimate[1]
R=fit1$estimate[2]
# goodness of fit test
ks.test(QTC_5$V1, "pgamma",shape=Q,rate=R) # p-value > 0.05 -> distribution not refused
# plot a graph
hist(QTC_5$V1, freq = FALSE, breaks = 30, xlim = c(0, quantile(QTC_5$V1, 0.99)),main="Dresses",xlab="Quantity")
curve(dgamma(x, shape=Q,rate =R), col = "red", add = TRUE)
Q*(1/R)
sqrt(Q)*(1/R)
Tops=qgamma(c(0.2,0.3,0.4,0.5,0.6,0.65,0.7,0.75,0.8,0.85),shape=Q,rate=R)
#Expectations
1-pgamma(c(100,150,200,250,270,290,310,330,350,370,400),shape=Q,rate=R)
qgamma(c(0.1,0.80),shape=Q,rate=R)
