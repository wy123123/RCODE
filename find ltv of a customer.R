dt.12=read.csv("C:/Users/Lovebonito/Downloads/newcustomers/12.csv", row.names = NULL)
dt.11=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/11.csv", row.names = NULL)
dt.10=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/10.csv", row.names = NULL)
dt.9=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/9.csv", row.names = NULL)
dt.8=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/8.csv", row.names = NULL)
dt.7=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/7.csv", row.names = NULL)
dt.6=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/6.csv", row.names = NULL)
dt.5=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/5.csv", row.names = NULL)
dt.4=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/4.csv", row.names = NULL)
dt.3=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/3.csv", row.names = NULL)
dt.2=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/2.csv", row.names = NULL)
dt.1=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/1.csv", row.names = NULL)
dt.0=read.csv("c:/Users/Lovebonito/Downloads/newcustomers/0.csv", row.names = NULL)
tail(dt.10s,2)
tail(dt.9s,1)
class(dt.9s$ID.Customer)
a
library(data.table)
setnames(dt.12,c(a,"A"))
setnames(dt.10,c(a,"A"))
setnames(dt.11,c(a,"A"))
setnames(dt.9,c(a,"A"))
setnames(dt.8,c(a,"A"))
setnames(dt.7,c(a,"A"))
setnames(dt.6,c(a,"A"))
setnames(dt.5,c(a,"A"))
setnames(dt.4,c(a,"A"))
setnames(dt.3,c(a,"A"))
setnames(dt.2,c(a,"A"))
setnames(dt.1,c(a,"A"))
dt.12[is.na(dt.12$Money.Spent),]$Money.Spent=0
dt.11[is.na(dt.11$Money.Spent),]$Money.Spent=0
dt.10[is.na(dt.10$Money.Spent),]$Money.Spent=0
dt.9[is.na(dt.9$Money.Spent),]$Money.Spent=0
dt.8[is.na(dt.8$Money.Spent),]$Money.Spent=0
dt.7[is.na(dt.7$Money.Spent),]$Money.Spent=0
dt.6[is.na(dt.6$Money.Spent),]$Money.Spent=0
dt.5[is.na(dt.5$Money.Spent),]$Money.Spent=0
dt.4[is.na(dt.4$Money.Spent),]$Money.Spent=0
dt.3[is.na(dt.3$Money.Spent),]$Money.Spent=0
dt.2[is.na(dt.2$Money.Spent),]$Money.Spent=0
dt.1[is.na(dt.1$Money.Spent),]$Money.Spent=0
dt.1s=dt.1[order(as.numeric(dt.1$ID.Customer)),]
dt.2s=dt.1[order(as.numeric(dt.2$ID.Customer)),]
dt.3s=dt.1[order(as.numeric(dt.3$ID.Customer)),]
dt.4s=dt.1[order(as.numeric(dt.4$ID.Customer)),]
dt.5s=dt.1[order(as.numeric(dt.5$ID.Customer)),]
dt.6s=dt.1[order(as.numeric(dt.6$ID.Customer)),]
dt.7s=dt.1[order(as.numeric(dt.7$ID.Customer)),]
dt.8s=dt.1[order(as.numeric(dt.8$ID.Customer)),]
dt.9s=dt.1[order(as.numeric(dt.9$ID.Customer)),]
dt.10s=dt.1[order(as.numeric(dt.10$ID.Customer)),]
dt.11s=dt.1[order(as.numeric(dt.11$ID.Customer)),]
dt.12s=dt.1[order(as.numeric(dt.12$ID.Customer)),]
#-------------------------------#
dt.1s=dt.1[dt.1$Money.Spent!=0,]
dt.2s=dt.2[dt.2$Money.Spent!=0,]
dt.3s=dt.3[dt.3$Money.Spent!=0,]
dt.4s=dt.4[dt.4$Money.Spent!=0,]
dt.5s=dt.5[dt.5$Money.Spent!=0,]
dt.6s=dt.6[dt.6$Money.Spent!=0,]
dt.7s=dt.7[dt.7$Money.Spent!=0,]
dt.8s=dt.8[dt.8$Money.Spent!=0,]
dt.9s=dt.9[dt.9$Money.Spent!=0,]
dt.10s=dt.10[dt.10$Money.Spent!=0,]
dt.11s=dt.11[dt.11$Money.Spent!=0,]
dt.12s=dt.12[dt.12$Money.Spent!=0,]
#-------------------------------#

table(dt.1s$Active)
#
##table(dt.12$Money.Spent-dt.11$Money.Spent>0)
#table(dt.11$Money.Spent-dt.10$Money.Spent>0)
#table(dt.10$Money.Spent-dt.9$Money.Spent>0)
#table(dt.9$Money.Spent-dt.8$Money.Spent>0)
#table(dt.8$Money.Spent-dt.7$Money.Spent>0)
#table(dt.7$Money.Spent-dt.6$Money.Spent>0)
#table(dt.6$Money.Spent-dt.5$Money.Spent>0)
#table(dt.5$Money.Spent-dt.4$Money.Spent>0)
#table(dt.4$Money.Spent-dt.3$Money.Spent>0)
##table(dt.3$Money.Spent-dt.2$Money.Spent>0)
##table(dt.2$Money.Spent-dt.1$Money.Spent>0)
dt=data.frame(Jan=table(dt.1$Money.Spent!=0)[2],
              Feb=table(dt.2$Money.Spent!=0)[2],
              Mar=table(dt.3$Money.Spent!=0)[2],
              Apr=table(dt.4$Money.Spent!=0)[2],
              May=table(dt.5$Money.Spent!=0)[2],
              Jun=table(dt.6$Money.Spent!=0)[2],
              Jul=table(dt.7$Money.Spent!=0)[2],
              Aug=table(dt.8$Money.Spent!=0)[2],
              Sep=table(dt.9$Money.Spent!=0)[2],
              Oct=table(dt.10$Money.Spent!=0)[2],
              Nov=table(dt.11$Money.Spent!=0)[2],
              Dec=table(dt.12$Money.Spent!=0)[2],row.names="No.of Active Customers")
library(xlsx)
write.xlsx(dt,"c:/Users/Lovebonito/Downloads/newcustomers/table.xlsx",sheetName="table")
Jan=table(dt.12$Money.Spent!=0)[2],str(table)
Jan=table(dt.12$Money.Spent!=0)[2],s=dt.11[(dt.11$Money.Spent-dt.10$Money.Spent>0),]

head(dt.1s)
dt.1s$Active[]=1
dt.1s$Active[match(dt.2s$ID.Customer,dt.1s$ID.Customer)]=2
dt.1s[dt.1s$Active==2,]$Active[match(dt.3s$ID.Customer,dt.1s[dt.1s$Active==2,]$ID.Customer)]=3
dt.1s[dt.1s$Active==3,]$Active[match(dt.4s$ID.Customer,dt.1s[dt.1s$Active==3,]$ID.Customer)]=4
dt.1s[dt.1s$Active==4,]$Active[match(dt.5s$ID.Customer,dt.1s[dt.1s$Active==4,]$ID.Customer)]=5
dt.1s[dt.1s$Active==5,]$Active[match(dt.6s$ID.Customer,dt.1s[dt.1s$Active==5,]$ID.Customer)]=6
dt.1s[dt.1s$Active==6,]$Active[match(dt.7s$ID.Customer,dt.1s[dt.1s$Active==6,]$ID.Customer)]=7
dt.1s[dt.1s$Active==7,]$Active[match(dt.8s$ID.Customer,dt.1s[dt.1s$Active==7,]$ID.Customer)]=8
dt.1s[dt.1s$Active==8,]$Active[match(dt.9s$ID.Customer,dt.1s[dt.1s$Active==8,]$ID.Customer)]=9
dt.1s[dt.1s$Active==9,]$Active[match(dt.10s$ID.Customer,dt.1s[dt.1s$Active==9,]$ID.Customer)]=10
dt.1s[dt.1s$Active==10,]$Active[match(dt.11s$ID.Customer,dt.1s[dt.1s$Active==10,]$ID.Customer)]=11
dt.1s[dt.1s$Active==11,]$Active[match(dt.12s$ID.Customer,dt.1s[dt.1s$Active==11,]$ID.Customer)]=12

#starting 2nd month
dt.2s$Active[]=1
dt.2s$Active[match(dt.1s$ID.Customer,dt.2s$ID.Customer)]=2
table(dt.2s$Active)
hist(dt.1s$Active)
dt.2ss=dt.2s[dt.2s$Active==1,]
dt.2ss$Active[match(dt.3s$ID.Customer,dt.2ss$ID.Customer)]=2
dt.2ss[dt.2ss$Active==2,]$Active[match(dt.4s$ID.Customer,dt.2ss[dt.2ss$Active==2,]$ID.Customer)]=3
dt.2ss[dt.2ss$Active==3,]$Active[match(dt.5s$ID.Customer,dt.2ss[dt.2ss$Active==3,]$ID.Customer)]=4
dt.2ss[dt.2ss$Active==4,]$Active[match(dt.6s$ID.Customer,dt.2ss[dt.2ss$Active==4,]$ID.Customer)]=5
dt.2ss[dt.2ss$Active==5,]$Active[match(dt.7s$ID.Customer,dt.2ss[dt.2ss$Active==5,]$ID.Customer)]=6
dt.2ss[dt.2ss$Active==6,]$Active[match(dt.8s$ID.Customer,dt.2ss[dt.2ss$Active==6,]$ID.Customer)]=7
dt.2ss[dt.2ss$Active==7,]$Active[match(dt.9s$ID.Customer,dt.2ss[dt.2ss$Active==7,]$ID.Customer)]=8
dt.2ss[dt.2ss$Active==8,]$Active[match(dt.10s$ID.Customer,dt.2ss[dt.2ss$Active==8,]$ID.Customer)]=9
dt.2ss[dt.2ss$Active==9,]$Active[match(dt.11s$ID.Customer,dt.2ss[dt.2ss$Active==9,]$ID.Customer)]=10
dt.2ss[dt.2ss$Active==10,]$Active[match(dt.12s$ID.Customer,dt.2ss[dt.2ss$Active==10,]$ID.Customer)]=11
table(dt.2ss$Active)
#starting 4rd month
dt.4s$Active[]=1
dt.4s$Active[match(dt.3s$ID.Customer,dt.4s$ID.Customer)]=2
table(dt.4s$Active)
dt.4ss=dt.4s[dt.4s$Active==1,]
table(dt.4ss$Active)
dt.4ss$Active[match(dt.5s$ID.Customer,dt.4ss$ID.Customer)]=2
dt.4ss[dt.4ss$Active==2,]$Active[match(dt.6s$ID.Customer,dt.4ss[dt.4ss$Active==2,]$ID.Customer)]=3
dt.4ss[dt.4ss$Active==3,]$Active[match(dt.7s$ID.Customer,dt.4ss[dt.4ss$Active==3,]$ID.Customer)]=4
dt.4ss[dt.4ss$Active==4,]$Active[match(dt.8s$ID.Customer,dt.4ss[dt.4ss$Active==4,]$ID.Customer)]=5
dt.4ss[dt.4ss$Active==5,]$Active[match(dt.9s$ID.Customer,dt.4ss[dt.4ss$Active==5,]$ID.Customer)]=6
dt.4ss[dt.4ss$Active==6,]$Active[match(dt.10s$ID.Customer,dt.4ss[dt.4ss$Active==6,]$ID.Customer)]=7
dt.4ss[dt.4ss$Active==7,]$Active[match(dt.11s$ID.Customer,dt.4ss[dt.4ss$Active==7,]$ID.Customer)]=8
dt.4ss[dt.4ss$Active==8,]$Active[match(dt.12s$ID.Customer,dt.4ss[dt.4ss$Active==8,]$ID.Customer)]=9

f=data.frame(table(dt.1s$Active))
i=sum(as.numeric(as.character(f$Var1))*as.numeric(as.character(f$Freq)))/(sum(as.numeric(as.character(f$Freq))))
t=data.frame(table(dt.2ss$Active))
o=sum(as.numeric(as.character(t$Var1))*as.numeric(as.character(t$Freq)))/(sum(as.numeric(as.character(t$Freq))))
g=data.frame(table(dt.4ss$Active))
p=sum(as.numeric(as.character(g$Var1))*as.numeric(as.character(g$Freq)))/(sum(as.numeric(as.character(g$Freq))))
mean(c(i,o,p))

