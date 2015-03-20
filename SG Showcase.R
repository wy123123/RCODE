dt=read.csv("C:/Users/Lovebonito/Downloads/new_customer_31-12-2013_to_01-03-2015_all.csv")
colnames(dt)
dt$DateAdd[1]
dates=strftime(dt$DateAdd,"%Y-%m-%d",usetz=FALSE)
dates.f=as.factor(datess)
f=data.frame(table(dates))
f
datess=strftime(f$dates,"%Y-%m",usetz=FALSE)
dates.f=as.factor(datess)
f=cbind(f,dates.f)
library(ggplot2)
library(grid)
p=ggplot(data=f.1[as.character(f.1$dates.f)=="2015-01" | as.character(f.1$dates.f)=="2015-02",], aes(x=s.1, y=Freq, group=dates.f,colour=dates.f,shape=dates.f)) + 
        geom_point(size=3)+geom_line(linetype="dashed")+ylab("New Customers")+
        geom_smooth(method="lm",se=F,size=1.2,alph=0.1)+
        xlab("Date Index")
p=p+theme(legend.title=element_blank())
p

p=ggplot(data=f.1, aes(x=s.1, y=Freq, colour=h)) + 
        geom_point()+geom_line(size=1,shape=2)+ylab("New Customers")+
        geom_smooth(method="lm",se=F,size=1.5)+xlab("Date Index")
p=p+theme(legend.title=element_blank())
p

p=ggplot(data=f.1, aes(x=s.1, y=Freq,group=dates.f,colour=h))+ 
        geom_point()+geom_line(size=1)+
        ylab("New Customers")+xlab("Date Index")
p=p+theme(legend.title=element_blank())
p

p=ggplot(data=f.1, aes(x=s.1, y=Freq,group=dates.f,colour=dates.f))+ 
        geom_point()+geom_line(size=1)+
        ylab("New Customers")+xlab("Date Index")
p=p+theme(legend.title=element_blank())
p

d=data.frame(table(dates.f))
d
s=0
for(i in 1 : 15){
        a=seq(from=1, to = d$Freq[i])
        s=c(s,a)
}
s.1=s[2:length(s)]
f.1=cbind(f,s.1)
f.1$h="Normal Month"
f.1$h[as.character(f.1$dates.f)=="2015-01"]="Jan 2015"
f.1$h[as.character(f.1$dates.f)=="2015-02"]="Feb 2015"
f.1$h[as.character(f.1$dates.f)=="2014-01"]="Jan 2014"
f.1$h[as.character(f.1$dates.f)=="2014-02"]="Feb 2014"
f.1$h=as.factor(f.1$h)

f.1$h[as.character(f.1$dates.f)=="2015-01"]="Better Month"
f.1$h[as.character(f.1$dates.f)=="2015-02"]="Better Month"
f.1$h[as.character(f.1$dates.f)=="2014-01"]="Better Month"
f.1$h[as.character(f.1$dates.f)=="2014-02"]="Better Month"
f.1$h[as.character(f.1$dates.f)=="2014-10"]="Better Month"
f.1$h[as.character(f.1$dates.f)=="2014-08"]="Better Month"
f.1$h=as.factor(f.1$h)

