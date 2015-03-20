##findpageview
library(data.table)
DT=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141222/pageview.csv",header=TRUE,sep=",")
DT_1=data.table(read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141222/Sale20141222.csv",header=TRUE,sep=","))
DT_1=DT_1[!is.na(DT_1$X),]
pageviews=rep(NA,length(DT_1$productName))
for(i in 1:length(DT_1$productName)){
  for( s in 1 : length(DT$Page.Title)){
    if(grepl(DT_1$productName[i],DT$Page.Title[s]))
      pageviews[i]=DT$Pageviews[s]
  }
}
DT_2=cbind(DT_1,pageviews)
conversionRate=round(DT_2$Q.Sold/DT_2$pageviews*100,2)
DT.3=cbind(DT_2,conversionRate)
dt.all=DT.3
dt.all=subset(dt.all,select=-c(X.3,X.1,X.2))
setcolorder(dt.all,c("X","productName","Design","productCategory","Average.Price","Q.Total","Q.Sold","X..Sold","pageview","pageviews","conversionRate"))
colnames(dt.all)[8]="X.Sold"

DT=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141230/pageview.csv",header=TRUE,sep=",")
DT_1=data.table(read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141230/Sale20141230.csv",header=TRUE,sep=","))
pageviews=rep(NA,length(DT_1$productName))
for(i in 1:length(DT_1$productName)){
  for( s in 1 : length(DT$Page.Title)){
    if(grepl(DT_1$productName[i],DT$Page.Title[s]))
      pageviews[i]=DT$Pageviews[s]
  }
}
DT_2=cbind(DT_1,pageviews)
conversionRate=round(DT_2$Q.Sold/DT_2$pageviews*100,2)
DT.3=cbind(DT_2,conversionRate)
DT.3=subset(DT.3,select=-X.1)
dt.all=rbind(dt.all,DT.3)

DT=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150105/pageview.csv",header=TRUE,sep=",")
DT_1=data.table(read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150105/Sale.csv",header=TRUE,sep=","))
pageviews=rep(NA,length(DT_1$productName))
for(i in 1:length(DT_1$productName)){
  for( s in 1 : length(DT$Page.Title)){
    if(grepl(DT_1$productName[i],DT$Page.Title[s]))
      pageviews[i]=DT$Pageviews[s]
  }
}
DT_2=cbind(DT_1,pageviews)
conversionRate=round(DT_2$Q.Sold/DT_2$pageviews*100,2)
DT.3=cbind(DT_2,conversionRate)
dt.all=DT.3


DT=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150109/pageview.csv",header=TRUE,sep=",")
DT_1=data.table(read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150109/Sale.csv",header=TRUE,sep=","))
pageviews=rep(NA,length(DT_1$productName))
for(i in 1:length(DT_1$productName)){
  for( s in 1 : length(DT$Page.Title)){
    if(grepl(DT_1$productName[i],DT$Page.Title[s]))
      pageviews[i]=DT$Pageviews[s]
  }
}
DT_2=cbind(DT_1,pageviews)
conversionRate=round(DT_2$Q.Sold/DT_2$pageviews*100,2)
DT.3=cbind(DT_2,conversionRate)
colnames(DT.3)[8:9]=c("X.Sold","pageview")
dt.all=dt.all[!is.na(dt.all$X),]
dt.all=rbind(dt.all,DT.3)

DT=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150115/pageview.csv",header=TRUE,sep=",")
DT_1=data.table(read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150115/Sale1.csv",header=TRUE,sep=","))
setcolorder(DT_1,c("X","ProductName","SKU","Type","Price","Q.Total","Q.Sold","per_sold","pageview"))
colnames(DT_1)=c("X","productName","Design","productCategory","Average.Price","Q.Total","Q.Sold","X.Sold","pageview")

pageviews=rep(NA,length(DT_1$productName))
for(i in 1:length(DT_1$productName)){
  for( s in 1 : length(DT$Page.Title)){
    if(grepl(DT_1$productName[i],DT$Page.Title[s]))
      pageviews[i]=DT$Pageviews[s]
  }
}
DT_2=cbind(DT_1,pageviews)
conversionRate=round(DT_2$Q.Sold/DT_2$pageviews*100,2)
DT.3=cbind(DT_2,conversionRate)
dt.all=dt.all[!is.na(dt.all$X),]
dt.all=rbind(dt.all,DT.3)

DT=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150122/pageview.csv",header=TRUE,sep=",")
DT_1=data.table(read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150122/Sale1.csv",header=TRUE,sep=","))
setcolorder(DT_1,c("X","ProductName","SKU","Type","Price","Q.Total","Q.Sold","per_sold","pageview"))
colnames(DT_1)=c("X","productName","Design","productCategory","Average.Price","Q.Total","Q.Sold","X.Sold","pageview")


pageviews=rep(NA,length(DT_1$productName))
for(i in 1:length(DT_1$productName)){
  for( s in 1 : length(DT$Page.Title)){
    if(grepl(DT_1$productName[i],DT$Page.Title[s]))
      pageviews[i]=DT$Pageviews[s]
  }
}
DT_2=cbind(DT_1,pageviews)
conversionRate=round(DT_2$Q.Sold/DT_2$pageviews*100,2)
DT.3=cbind(DT_2,conversionRate)
dt.all=dt.all[!is.na(dt.all$X),]
dt.all=rbind(dt.all,DT.3)

DT=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150127/pageview.csv",header=TRUE,sep=",")
DT_1=data.table(read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150127/Sale1.csv",header=TRUE,sep=","))
setcolorder(DT_1,c("X","ProductName","SKU","Type","Price","Q.Total","Q.Sold","per_sold","pageview"))
colnames(DT_1)=c("X","productName","Design","productCategory","Average.Price","Q.Total","Q.Sold","X.Sold","pageview")


pageviews=rep(NA,length(DT_1$productName))
for(i in 1:length(DT_1$productName)){
  for( s in 1 : length(DT$Page.Title)){
    if(grepl(DT_1$productName[i],DT$Page.Title[s]))
      pageviews[i]=DT$Pageviews[s]
  }
}
DT_2=cbind(DT_1,pageviews)
conversionRate=round(DT_2$Q.Sold/DT_2$pageviews*100,2)
DT.3=cbind(DT_2,conversionRate)
dt.all=dt.all[!is.na(dt.all$X),]
dt.all=rbind(dt.all,DT.3)
write.csv(dt.all,file="C:/Users/Lovebonito/Downloads/Launching Report/JanData.csv",row.names=FALSE)
#########################################################################

DT=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20141217/pageview.csv",header=TRUE,sep=",")
DT_1=data.table(read.csv("C:/Users/Lovebonito/Downloads/Launching Report/20150122/Sale1.csv",header=TRUE,sep=","))
setcolorder(DT_1,c("X","ProductName","SKU","Type","Price","Q.Total","Q.Sold","X..sold","pageview"))
colnames(DT_1)=c("X","productName","Design","productCategory","Average.Price","Q.Total","Q.Sold","X.Sold","pageview")

pageviews=rep(NA,length(DT_1$productName))
for(i in 1:length(DT_1$productName)){
  for( s in 1 : length(DT$Page.Title)){
    if(grepl(DT_1$productName[i],DT$Page.Title[s]))
      pageviews[i]=DT$Pageviews[s]
  }
}
DT_2=cbind(DT_1,pageviews)
conversionRate=round(DT_2$Q.Sold/DT_2$pageviews*100,2)
DT.3=cbind(DT_2,conversionRate)
dt.all=dt.all[!is.na(dt.all$X),]
dt.all=rbind(dt.all,DT.3)
#---------------------------------------------------------------------------------------------------#
dt.all=dt.all[order(dt.all$Average.Price)]
class(dt.all$Average.Price)
dt.a=dt.all[as.numeric(as.character(dt.all$Q.Sold))/as.numeric(as.character(dt.all$Q.Total))!=1,]
dt.a$conversionRate[30]=2
mean(as.numeric(as.character(dt.a$conversionRate)))
p1=ggplot(data=dt.a,aes(x=as.numeric(as.character(Average.Price)),y=conversionRate))+geom_point()
p1=p1+xlab("Price")+geom_smooth(method=loess) +labs(title="Overall vs Price")
p1

dt.dress=dt.a[dt.a$productCategory=="Dress"|dt.a$productCategory=="Maxi",]
mean(as.numeric(as.character(dt.dress$conversionRate)))
p2=ggplot(data=dt.dress,aes(x=as.numeric(as.character(Average.Price)),y=conversionRate))+geom_point()
p2=p2+geom_smooth(method=loess) 
p2=p2+labs(title="Dress vs Price")+xlab("Price")
p2

dt.dress=dt.a[dt.a$productCategory=="Skirt"|dt.a$productCategory=="Shorts"|dt.a$productCategory=="Pants"|dt.a$productCategory=="Skorts",]
p3=ggplot(data=dt.dress,aes(x=as.numeric(as.character(Average.Price)),y=conversionRate))+geom_point()
p3=p3+xlab("Price")+geom_smooth(method=loess)  +labs(title="Bottoms vs Price")
p3

dt.dress=dt.a[dt.a$productCategory=="Top"|dt.a$productCategory=="Bustier",]
p4=ggplot(data=dt.dress,aes(x=as.numeric(as.character(Average.Price)),y=conversionRate))+geom_point()
p4=p4+xlab("Price")+geom_smooth(method=loess)  +labs(title="Top vs Price")
p4

dt.dress=dt.a[dt.a$productCategory=="Romper"|dt.a$productCategory=="Jumpsuit"|dt.a$productCategory=="Overalls",]
p5=ggplot(data=dt.dress,aes(x=as.numeric(as.character(Average.Price)),y=conversionRate))+geom_point()
p5=p5+xlab("Price")+geom_smooth(method=loess)  +labs(title="One Piece vs Price")
p5

multiplot(p1, p2, p3, p4, p5,cols=2)
??multiplot
table(dt.a$productCategory)
library(ggplot2)
write.csv(DT_2,file="Sale.csv")
with(DT_2,plot(as.numeric(as.character(Q.Sold)),pageview,pch=11,col="blue"))
with(DT_2,cor(Q.Sold,pageview))
ggplot(DT_2, aes(x=pageview, y=as.numeric(as.character(Q.Sold)))) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) 



DT_2=data.table(read.csv("C:/Users/Lovebonito/Downloads/pageview analysis/pageview vs sales.csv",header=TRUE,sep=","))
C=rep(NA,length(DT_2$Q.Sold)
for(i in 1 :length(DT_2$Q.Sold)){
  if(DT_2$productCategory[i]=="Dress")
    C[i]="Dress"
  else if(DT_2$productCategory[i]=="Skirt"|DT_2$productCategory=="Shorts"|DT_2$productCategory=="")
    
}
DT_4=DT_3[!as.character(DT_3$X.Sold)=="100.00%",]
ggplot() +  
  geom_point(DT_4, mapping=aes(x=pageview, y=as.numeric(as.character(Q.Sold)),col=C,size=3))+
  geom_smooth(method=lm,se=FALSE,size=1)
# Extend the regression lines beyond the domain of the data
ggplot(DT_4, aes(x=pageview, y=as.numeric(as.character(Q.Sold)), color=C)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,size=2) # Extend regression lines

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}