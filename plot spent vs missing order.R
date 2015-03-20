library(xlsx)
library(Hmisc)
library(ggplot2)
library(gridExtra)
missed=read.csv("C:/Users/Lovebonito/Desktop/missing orders/missingorders11.csv")
all=read.csv("C:/Users/Lovebonito/Desktop/missing orders/11 feb.csv",header=F)
head(all,3)
a=sapply(as.character(missed$V4),nchar)
amount.m=as.numeric(substring(missed$V4,5,a))
amount=as.numeric(substring(all$V4,5,sapply(as.character(all$V4),nchar)))
amount[is.na(amount)]=0
missed=cbind(missed,amount.m)
all=data.frame(amount=amount,group="all")
missed=data.frame(amount=amount.m,group="missed")
ss=rbind(all,missed)
ss$group=as.factor(ss$group)
plot.new()
p1=ggplot()+geom_histogram(data=ss,aes(x=amount),binwidth = 5,fill="blue",alph=0.3)+
        geom_histogram(data=missed,aes(x=amount.m),fill="purple", alph=0.7,binwidth =5)
        #+legend("topright",c("all", "missed"), fill=c("blue", "purple"))

p2=ggplot(data=ss, aes(x = amount,colour=group)) + geom_density(alpha = 0.5,size=1)+
        scale_colour_manual(values=c("blue", "purple"))+
        theme(legend.position="bottom")+
        theme(legend.title = element_text(size=10))+
        theme(legend.text = element_text(size=10))+
        theme(legend.background = element_rect(fill="gray90", size=0.1, linetype="dotted"))
        

grid.arrange(p1, p2, nrow=2)
        
geom_histogram(data=missed,aes(x=amount.m),colour="red",fill="grey", alph=0.5,binwidth = 5)+geom_density()
geom_density(data=all,aes(x=amount,y=...density),colour="green",size=2)
