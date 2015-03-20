#Quantity sold by line by season
#Read data
##
library(data.table)
library(ggplot2)
#find the best seller
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
data_MF=data.table(rbind(data12,data11,data10,data09,data08,data07,data06,data05,data04,data03,data02,data01))
data_MF$price=round(data_MF$price*1.07)
Q_S=data.table(data_MF[,list(Q.sum=sum(Quantity),S.sum=sum(Sales)),by=c("ProductCode","ProductName","Category","colour")])
price=(round(as.vector(Q_S$S.sum)/Q_S$Q.sum)*1.07)
Q_S[,price:=round(price*1.07)]

dress=data_MF[data_MF$Category=="Dresses",]
Top=data_MF[data_MF$Category=="Tops",]
d_sum=Top[,sum(Quantity),by=c("ProductCode","ProductName","Category","colour","price")]
d_sum=d_sum[d_sum$V1!=0&(d_sum$colour=="White"|d_sum$colour=="Black"),]
qplot(price,Quantity,colour=Category,data=d_sum)
qplot(Quantity,colour=colour,data=dress,geom="density")

best_seller=data_MF[,sum(Quantity),by=c("ProductCode","ProductName","Category")]
best_seller=best_seller[order(-best_seller$V1),]
write.csv(best_seller,file="best_seller2014.csv")
best_seller_category=data_MF[,sum(Quantity),by=c("ProductCode","Category")]
best_seller_category=best_seller_category[order(-best_seller_category$Category),]
write.csv(best_seller_category,file="best_seller_bycategory_2014.csv")

data12=read.csv("C:/Users/Lovebonito/Downloads/201412/Cost_revenue_eachline_201412.csv")
data11=read.csv("C:/Users/Lovebonito/Downloads/201411/Cost_revenue_eachline_201411.csv")
data10=read.csv("C:/Users/Lovebonito/Downloads/201410/Cost_revenue_eachline_201410.csv")
data09=read.csv("C:/Users/Lovebonito/Downloads/201409/Cost_revenue_eachline_201409.csv")
data08=read.csv("C:/Users/Lovebonito/Downloads/201408/Cost_revenue_eachline_201408.csv")
data07=read.csv("C:/Users/Lovebonito/Downloads/201407/Cost_revenue_eachline_201407.csv")
data06=read.csv("C:/Users/Lovebonito/Downloads/201406/Cost_revenue_eachline_201406.csv")
data05=read.csv("C:/Users/Lovebonito/Downloads/201405/Cost_revenue_eachline_201405.csv")
data04=read.csv("C:/Users/Lovebonito/Downloads/201404/Cost_revenue_eachline_201404.csv")
data03=read.csv("C:/Users/Lovebonito/Downloads/201403/Cost_revenue_eachline_201403.csv")
data02=read.csv("C:/Users/Lovebonito/Downloads/201402/Cost_revenue_eachline_201402.csv")
data01=read.csv("C:/Users/Lovebonito/Downloads/201401/Cost_revenue_eachline_201401.csv")
#Breaks for background rectangles
library(data.table)
library(ggplot2)
Dataall=data.frame(data04$Line,data01$Quantity,data02$Quantity,data03$Quantity,data04$Quantity,data05$Quantity,data06$Quantity,data07$Quantity,data08$Quantity,data09$Quantity,data10$Quantity,data11$Quantity,data12$Quantity)
month_index=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month=seq(1:12)
Dataall2=(t(Dataall))
colnames(Dataall2)=Dataall2[1,]
Dataall3=Dataall2[-1,]
Dataall4=data.table(cbind(Dataall3,month_index))
colnames(Dataall4)[c(3,5)]=c("Basic","COVET")
#Breaks for background rectangles
Basics=as.numeric(Dataall4[,Basic])
Bottoms=as.numeric(Dataall4[,Bottoms])
COVET=as.numeric(Dataall4[,COVET])
Dresses=as.numeric(Dataall4[,Dresses])
Petite=as.numeric(Dataall4[,Petite])
Tops=as.numeric(Dataall4[,Tops])
rects <- data.frame(xstart = c(1,4,7,10),xend = c(4,7,10,12),season = c("spring","summer","autumn","winter"))
ggplot()+
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = season,xlab=F),alpha=0.3)+
  ylab("Quantity Sold")+
  scale_x_discrete(breaks=c(1:12), labels=c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_fill_discrete(name="Season",breaks=c("spring","summer","autumn","winter"))+
  geom_line(aes(month,Basics,colour = "Basics"),size=1.5)+geom_point(aes(month,Basics),colour="blue",size=3,shape=0)+
  geom_line(aes(month,Bottoms,colour="Bottoms"),size=1.5)+geom_point(aes(month,Bottoms),colour="red",size=3,shape=21)+
  geom_line(aes(month,COVET,colour="COVET"),size=1.5)+geom_point(aes(month,COVET),colour="green",size=3,shape=22)+
  geom_line(aes(month,Dresses,colour="Dresses"),size=1.5)+geom_point(aes(month,Dresses),colour="yellow",size=3,shape=23)+
  geom_line(aes(month,Petite,colour="Petite"),size=1.5)+geom_point(aes(month,Petite),colour="black",size=3,shape=24)+
  geom_line(aes(month,Tops,colour="Tops"),size=1.5)+geom_point(aes(month,Tops),colour="purple",size=3,shape=25)+
  theme_bw()+theme(legend.position="top")

#performance of colour&print through 2014 and each season
data01=read.csv("C:/Users/Lovebonito/Downloads/201501/201501COLOUR.csv")
data12=read.csv("C:/Users/Lovebonito/Downloads/201412/201412COLOUR.csv")
data11=read.csv("C:/Users/Lovebonito/Downloads/201411/201411COLOUR.csv")
data10=read.csv("C:/Users/Lovebonito/Downloads/201410/201410COLOUR.csv")
data09=read.csv("C:/Users/Lovebonito/Downloads/201409/201409COLOUR.csv")
data08=read.csv("C:/Users/Lovebonito/Downloads/201408/201408COLOUR.csv")
data07=read.csv("C:/Users/Lovebonito/Downloads/201407/201407COLOUR.csv")
data06=read.csv("C:/Users/Lovebonito/Downloads/201406/201406COLOUR.csv")
data05=read.csv("C:/Users/Lovebonito/Downloads/201405/201405COLOUR.csv")
data04=read.csv("C:/Users/Lovebonito/Downloads/201404/201404COLOUR.csv")
data03=read.csv("C:/Users/Lovebonito/Downloads/201403/201403COLOUR.csv")
data02=read.csv("C:/Users/Lovebonito/Downloads/201402/201402COLOUR.csv")
colour_spring=rbind(data04,data03,data02)
colour_summer=rbind(data07,data06,data05)
colour_fall=rbind(data10,data09,data08)
colour_winter=rbind(data01,data12,data11)
colour_spring=data.table(colour_spring)
colour_summer=data.table(colour_summer)
colour_fall=data.table(colour_fall)
colour_winter=data.table(colour_winter)
colour_1=colour_spring[,sum(V1),by="temp"]
colour_2=colour_summer[,sum(V1),by="temp"]
colour_3=colour_fall[,sum(V1),by="temp"]
colour_4=colour_winter[,sum(V1),by="temp"]
write.csv(colour_1,file="colour_1.csv")
write.csv(colour_2,file="colour_2.csv")
write.csv(colour_3,file="colour_3.csv")
write.csv(colour_4,file="colour_4.csv")
library(reshape2)
#% of sales of each line per season
data12=read.csv("C:/Users/Lovebonito/Downloads/201412/Cost_revenue_eachline_201412.csv")
data11=read.csv("C:/Users/Lovebonito/Downloads/201411/Cost_revenue_eachline_201411.csv")
data10=read.csv("C:/Users/Lovebonito/Downloads/201410/Cost_revenue_eachline_201410.csv")
data09=read.csv("C:/Users/Lovebonito/Downloads/201409/Cost_revenue_eachline_201409.csv")
data08=read.csv("C:/Users/Lovebonito/Downloads/201408/Cost_revenue_eachline_201408.csv")
data07=read.csv("C:/Users/Lovebonito/Downloads/201407/Cost_revenue_eachline_201407.csv")
data06=read.csv("C:/Users/Lovebonito/Downloads/201406/Cost_revenue_eachline_201406.csv")
data05=read.csv("C:/Users/Lovebonito/Downloads/201405/Cost_revenue_eachline_201405.csv")
data04=read.csv("C:/Users/Lovebonito/Downloads/201404/Cost_revenue_eachline_201404.csv")
data03=read.csv("C:/Users/Lovebonito/Downloads/201403/Cost_revenue_eachline_201403.csv")
data02=read.csv("C:/Users/Lovebonito/Downloads/201402/Cost_revenue_eachline_201402.csv")
spring=data.table(rbind(data02,data03,data04))
summer=data.table(rbind(data05,data06,data07))
fall=data.table(rbind(data08,data09,data10))
winter=data.table(rbind(data11,data12))
spring_sales=spring[,sum(Revenue),by="Line"]
summer_sales=summer[,sum(Revenue),by="Line"]
fall_sales=fall[,sum(Revenue),by="Line"]
winter_sales=winter[,sum(Revenue),by="Line"]
sale_percentage_spring=matrix(spring_sales$V1/(sum(spring_sales$V1))*100)
sale_percentage_summer=matrix(summer_sales$V1/(sum(summer_sales$V1))*100)
sale_percentage_fall=matrix(fall_sales$V1/(sum(fall_sales$V1))*100)
sale_percentage_winter=matrix(winter_sales$V1/(sum(winter_sales$V1))*100)

sale_percentage_winter=sale_percentage_winter[-11,]

DT=data.frame(Lines=data06$Line,spring=sale_percentage_spring,summer=sale_percentage_summer,fall=sale_percentage_fall,winter=sale_percentage_winter)
write.csv(DT, file="SSS.csv")
mdata <- melt(DT, id=c("Lines"))
ggplot(data=mdata, aes(x=factor(variable), y=value, group=Lines, colour=Lines)) + geom_line(size=1) + geom_point()+
  xlab("season")+
  ylab("Percentage of the season")+
  theme(legend.position="top")

#cost of each line VS revenue
spring_cost=spring[,sum(Cost),by="Line"]
summer_cost=summer[,sum(Cost),by="Line"]
fall_cost=fall[,sum(Cost),by="Line"]
winter_cost=winter[,sum(Cost),by="Line"]
revenue_sum=spring_sales$V1+summer_sales$V1+fall_sales$V1+winter_sales$V1
cost=spring_cost$V1+summer_cost$V1+fall_cost$V1+winter_cost$V1
DT=data.frame(Lines=data06$Line,Revenue=revenue_sum,Cost=cost)
dt_bar=melt(DT,id=c("Lines"))
ggplot(data=dt_bar, aes(x=factor(Lines), y=value, fill=variable)) + geom_bar(stat="identity", position=position_dodge())+
  xlab("Lines")+
  ylab("Sales")+
  scale_fill_discrete(name="Revenue VS Cost")+
  geom_text(aes(label = value), size = 4)+theme_bw()


