library(data.table)
library(xlsx)
####change directory here
month="Mar"
filename="11 Mar"
#cost report dir
dirc="C:/Users/Lovebonito/Downloads/cost report/2015/"
#launching report dir
dirl="C:/Users/Lovebonito/Downloads/Launching Report/2015/"
#launch sales report dir
dirl2=paste("C:/Users/Lovebonito/Downloads/launch sales report/2015/Sales Report_",month," 2015.xlsx",sep="")
###########################################
###########################################

price=data.table(read.csv(paste(dirc,month,"/",filename,".csv",sep="")))

price_1=price[!is.na(price$Cost.Price...SGD....1.),]

findDesign=function(x){
        a=substring(x,1,1)
        if(a=="S")
                return(substring(x,1,5))
        else if(a=="A")
                return(substring(x,1,7))
        else
                return(substring(x,1,6))
}

Design=sapply(as.vector(price_1$X), function(x) findDesign(x))
price_1=cbind(price_1,Design)
NewIn=as.vector(price_1$Design)
write.csv(NewIn, file=paste(dirl,month,"/",filename,"/NewIn.csv",sep=""),row.names=F)
#------------------------------------------------------------------------------------------
DT=data.table(read.xlsx(dirl2,sheetName=filename))

DT=DT[-1,]
dd=(DT$NA..4=="Quantity"|DT$NA..4=="Q.Sold")
DT_1=DT[dd,]
sub=data.table(a=as.numeric(as.vector(DT_1$NA..5)),
               B=as.numeric(as.vector(DT_1$NA..6)),
               C=as.numeric(as.vector(DT_1$NA..7)),
               D=as.numeric(as.vector(DT_1$NA..8)),
               E=as.numeric(as.vector(DT_1$NA..9)))
SBU_T=t(sub)
m=matrix(colSums(SBU_T),ncol=1,byrow=TRUE)
DT_2=cbind(DT_1,m)
q_sold=as.numeric(as.vector(DT_2[DT_2$NA..4=="Q.Sold"]$V1))
DT_3=cbind(DT_2[DT_2$NA..4=="Quantity"],q_sold)
DT_4=subset(DT_3,select=c(NA..1,V1,q_sold))
setnames(DT_4,c("SKU","Q.Total","Q.Sold"))
DT_5=DT_4[,lapply(.SD,sum,na.rm=TRUE),by=SKU,]
#----------------
#----------------
#--with shoes----
library(data.table)
DT=data.table(read.xlsx("C:/Users/Lovebonito/Downloads/launch sales report/2015/11 feb.xlsx",sheetIndex=2))
DT=DT[-1,]
dd=(DT$NA..4=="Quantity"|DT$NA..4=="Q.Sold")
DT_1=DT[dd,]
sub=data.table(a=as.numeric(as.vector(DT_1$NA..5)),
               B=as.numeric(as.vector(DT_1$NA..6)),
               C=as.numeric(as.vector(DT_1$NA..7)),
               D=as.numeric(as.vector(DT_1$NA..8)),
               E=as.numeric(as.vector(DT_1$NA..9)),
               f=as.numeric(as.vector(DT_1$NA..10)),
               g=as.numeric(as.vector(DT_1$NA..11)))
SBU_T=t(sub)
SBU_T[is.na(SBU_T)]=0
m=matrix(colSums(SBU_T),ncol=1,byrow=TRUE)
DT_2=cbind(DT_1,m)
q_sold=as.numeric(as.vector(DT_2[DT_2$NA..4=="Q.Sold"]$V1))
DT_3=cbind(DT_2[DT_2$NA..4=="Quantity"],q_sold)
DT_4=subset(DT_3,select=c(NA..1,V1,q_sold))
setnames(DT_4,c("SKU","Q.Total","Q.Sold"))
DT_5=DT_4[,lapply(.SD,sum,na.rm=TRUE),by=SKU,]
#--------------------
#--------------------
#--------------------
##get category
ProductName=DT[DT$NA.!="",]$NA.
a=strsplit(as.vector(ProductName),split=" ")
k=unlist(lapply(a, length))
Type=rep(NA,length(k))
for(i in 1 : length(k)){
        Type[i]=a[[i]][k[i]]
}

DT_6=cbind(Type,DT_5)
###ITEM NAME
DT_7=cbind(ProductName,DT_6)
##% sold
per_sold=round(DT_7$Q.Sold/DT_7$Q.Total*100,1)
DT_8=cbind(DT_7,per_sold)
DT_8=DT_8[order(-DT_8$per_sold)]
###pageview

DT=data.table(read.xlsx(paste0(dirl,month,"/",filename,"/pageview.xlsx"),sheetIndex=2,header=T))

pageview=rep(NA,length(DT_8$ProductName))
for(i in 1:length(DT_8$ProductName)){
        for( s in 1 : length(DT$Page.Title)){
                if(grepl(DT_8$ProductName[i],DT$Page.Title[s]))
                        pageview[i]=DT$Unique.Pageviews[s]
        }
}

DT_9=cbind(DT_8,pageview)
#add price
Price=price_1$Selling.Price[match(DT_9$SKU,price_1$Design)]
DT_10=cbind(DT_9,Price)
write.xlsx(DT_10,file=paste(dirl,month,"/",filename,"/sale1.xlsx",sep=""),sheetName="Sheet1",row.names=FALSE)

#Predict sold out items
DT_10$Price[7]=48
DT_10$Price=as.numeric(as.character(DT_10$Price))
newdata = data.frame(matrix(DT_10[,c(pageview,Price)],ncol=2))
colnames(newdata)=c("pageview","Price")
Prediction=predict(modfit,newdata,interval="predict")
DT_11=cbind(DT_10,Prediction)

#add cost
DT_10=cbind(DT_9,Price,price_1$Cost.Price...SGD....1.[match(DT_9$SKU,price_1$Design)])
write.csv(DT_10,file="C:/Users/Lovebonito/Desktop/hh.csv")
