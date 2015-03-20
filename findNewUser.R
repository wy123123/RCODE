library(data.table)
n.10=data.table(read.csv("C:/Users/Lovebonito/Downloads/newcustomers/n.10.csv",header=TRUE))
n.11=data.table(read.csv("C:/Users/Lovebonito/Downloads/newcustomers/n.11.csv",header=TRUE))
n.12=data.table(read.csv("C:/Users/Lovebonito/Downloads/newcustomers/n.12.csv",header=TRUE))
n.13=data.table(read.csv("C:/Users/Lovebonito/Downloads/newcustomers/n.13.csv",header=TRUE,sep=","))
n.14=data.table(read.csv("C:/Users/Lovebonito/Downloads/newcustomers/n.14.csv",header=TRUE))
head(n.10.1)
tail(n.10)
n.10.1=unique(n.10,by="Email")
n.11.1=unique(n.11,by="Email")
n.12.1=unique(n.12,by="Email")
n.13.1=unique(n.13,by="Email")
n.14.1=unique(n.14,by="Email")
n.10.1=n.10.1[-18076,]
x=matrix(unlist(strsplit(as.vector(n.10.1$DateAdd)," ")),ncol=2,byrow=TRUE)
head(x)
table(x[,2])
y=substring(x[,2],2,8)

n.10=cbind(n.10.1)

findNU=cbind(findNewUser,y)
findNU=findNU[order(findNU$y),]
S=as.vector(table(findNU$y))
head(S)
head(y)
write.csv(S,file="C:/Users/Lovebonito/Downloads/NewUsersssss.csv")

sessions=data.table(read.csv("C:/Users/Lovebonito/Downloads/Sessions(Pageview).csv",header=TRUE,sep=",",row.names=NULL))
head(b)
tail(sessions)
sessions.1=sessions[-274,]
x=matrix(unlist(strsplit(as.vector(sessions.1$Day.Index),"/")),ncol=3,byrow=TRUE)
b=cbind(sessions.1,x)
dt=b[,sum(Sessions),by=V1]
dt=dt[order(dt$Date),]
dt
write.csv(dt,file="C:/Users/Lovebonito/Downloads/ttt.csv")

customers=data.table(read.csv("C:/Users/Lovebonito/Downloads/best_customer_2015-01-29_13-42.csv",header=TRUE,sep=",",row.names=NULL))
head(customers)
setnames(customers,c("ID.Customer","Lastname","Firstname","Email","Visits","Page.viewed","Money.Spent","s"))
customers=subset(customers,select=-c(s))
dim(customers)
cc=data.table(read.csv("C:/Users/Lovebonito/Downloads/cc.csv",header=TRUE,sep=","))
c=cc$Sessions*cc$Avg..Session.Duration/3600
cc=cbind(cc,c)
dd=cc[,lapply(.SD,sum,na.rm=TRUE),by=Hour.of.Day,.SDcols=c("Sessions","Avg..Session.Duration")]
dd=dd[order(dd$Hour.of.Day)]
dd=cc[,sum(c),by=Hour.of.Day]
write.csv(dd,file="c:/Users/Lovebonito/Downloads/Cc visits.csv",row.names=FALSE)
