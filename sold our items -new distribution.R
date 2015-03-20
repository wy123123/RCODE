dt=read.csv("C:/Users/Lovebonito/Downloads/launch sales report/TEST/cut master file.csv")
head(dt)
dt$per_sold=dt$Quantity.Sold/dt$Total.Quantity
head(dt)
sold_out=dt[dt$per_sold>=0.90,]
head(sold_out)

sold_out=sold_out[,colSums(is.na(sold_out))==0]
s=strsplit(as.character(sold_out$Product.name),split=" ")
s[[1]][3]
a=lapply(s,length)
a=sapply(a,'[',1)
t=0
for(i in 1 : 105){
        t[i]=s[[i]][a[i]]
}
sold_out=cbind(sold_out,t)
table(sold_out$t)
sold_out$t=as.vector(sold_out$t)
sold_out[which(sold_out$t=="Bralet" | sold_out$t=="Clutch" | sold_out$t=="Necklace"| sold_out$t=="Set"),]$t="others"
sold_out[which(sold_out$t=="Jacket" | sold_out$t=="Coat" |sold_out$t=="Heels"),]$t="others"
sold_out[which(sold_out$t=="Jumpsuit" | sold_out$t=="Overalls" | sold_out$t=="Romper"),]$t="onepiece"
sold_out[which(sold_out$t=="Tank" | sold_out$t=="Tee" | sold_out$t=="Top"),]$t="top"
sold_out[which(sold_out$t=="Skirt" | sold_out$t=="Pants" | sold_out$t=="Skorts"| sold_out$t=="Culottes"),]$t="Bottom"

sold_out[45,]
library(ggplot2)
ggplot(data=sold_out,aes(x=Total.Quantity,y=per_sold,colour=t,shape=t))+
               geom_point(size=6)


ss=strsplit(as.character(dt$Product.name),split=" ")
a=lapply(ss,length)
a=sapply(a,'[',1)
t=0
for(i in 1 : 384){
        t[i]=ss[[i]][a[i]]
}
dt=cbind(dt,t)
table(dt$t)

dress=dt[which(dt$t=="Dress" & dt$Total.Quantity<180 ),]
dres.s=sold_out[which(sold_out$t=="Dress"&sold_out$Total.Quantity<180),]

dress=dt[which(dt$t=="Dress" & dt$Total.Quantity>400 ),]
dres.s=sold_out[which(sold_out$t=="Dress"&sold_out$Total.Quantity>400),]


dress=dt[which(dt$t=="Dress" & dt$Total.Quantity>200 &dt$Total.Quantity<=400),]
dres.s=sold_out[which(sold_out$t=="Dress" & sold_out$Total.Quantity>200 &sold_out$Total.Quantity<=400),]

ggplot(data=dress,aes(x=Total.Quantity,y=per_sold,colour=as.factor(month)))+
        geom_point(size=6)
c=seq(from=100,to=400,by=20)
c[2]
c[16]
for(i in 1 : 16){
        
        x[i]=dim(sold_out[which(sold_out$t=="Dress"&sold_out$Total.Quantity<c[i]),])[1]/
                dim(dt[which(dt$t=="Dress" & dt$Total.Quantity<c[i] ),])[1]
}
z=0
for(i in 1 : 16){
        
        z[i]=dim(dt[which(dt$t=="Dress" & dt$Total.Quantity<c[i] & dt$per_sold<0.5),])[1]/
                dim(dt[which(dt$t=="Dress" & dt$Total.Quantity<c[i] ),])[1]
}
       
plot(x=c,y=z)

ggplot()+geom_line(aes(x=c,y=z),colour="red",size=2)+
        geom_line(aes(x=c,y=x),size=2,colour="blue")+
        xlab("Quantity smaller than")+ylab("Probability")+
        geom_vline(xintercept = c(150,200,250),linetype="dotted",size=1)
        

dim(dt[which(dt$t=="Dress" & dt$Total.Quantity>180 & dt$per_sold<0.5),])[1]/
dim(dt[which(dt$t=="Dress" & dt$Total.Quantity>180 ),])[1]


