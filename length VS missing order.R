library(data.table)
MasterFile=read.csv("C:/Users/Lovebonito/Downloads/Launching Report/2015/Jan/20150109/MasterFile_01_09.csv",header=TRUE,sep=",")
QAT_1=MasterFile
QAT_1_DT=data.table(QAT_1)
#
#
TypeRanking=data.table(read.csv("C:/Users/Lovebonito/Downloads/Launching Report/2015/Jan/20150109/sale.csv",header=TRUE,sep=","))
#
#
#find sale in names and sku
Sales=QAT_1_DT[,sum(Quantity),by=c("productName","Design","Average.Price")]
Sales$Average.Price=round(Sales$Average.Price*1.07,0)
#########
#########
soldGA=Sales$V1[match(TypeRanking$Design,Sales$Design)]
test=data.frame(cbind(TypeRanking,soldGA))
test=subset(test,select=-X)
setnames(test,c("ProductName","SKU","Type","Price","Q.Total","Q.Sold","per_sold","pageview","soldGA"))
test=data.frame(test)
test.t=test[,c("ProductName","Type","SKU","Q.Total","Q.Sold","per_sold","pageview","Price","soldGA")]
test$soldGA[1]=442-303
test$soldGA[2]=450-217
test$soldGA[3]=445-263
test$soldGA[4]=300-127

test.2=rbind(test.1,test.t)
No.of.Missed.Item=test.2$Q.Sold-test.2$soldGA
per.missed=No.of.Missed.Item/test.2$Q.Sold
length=sapply(as.character(test.2$ProductName),nchar)
test.3=cbind(test.2,No.of.Missed.Item,per.missed,length)
write.csv(test.3,file="C:/Users/Lovebonito/Downloads/testing.csv",row.names=F)
plot(x=length,y=per.missed)
class(test$ProductName)
plot(No.of.Missed.Item,Length.of.Name)