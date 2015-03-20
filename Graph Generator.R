#-----------------------------------------------------------------
#=================================================================
#-----------------------------------------------------------------

MasterFile=read.csv(paste(dirl,month,"/",filename,"/MasterFile.csv",sep=""),header=TRUE,sep=",")
QAT_1=MasterFile
QAT_1_DT=data.table(QAT_1)

##########################
TypeRanking=data.table(read.xlsx(paste(dirl,month,"/",filename,"/sale1.xlsx",sep=""),header=TRUE,sheetIndex=1))
TypeRanking=TypeRanking[!is.na(TypeRanking$ProductName),]
TypeRanking$Q.Total=as.numeric(as.character(TypeRanking$Q.Total))
sales_2=TypeRanking[,lapply(.SD,sum,na.rm=TRUE),by=Type,.SDcol=c("Q.Total","Q.Sold")]
sales_3=cbind(sales_2,round(sales_2$Q.Sold/sales_2$Q.Total*100,2))
NoOfItems=as.vector(table(TypeRanking$Type))
sales_3=sales_3[order(sales_3$Type),]
sales_3=cbind(sales_3,NoOfItems)
setnames(sales_3,c("Category","Q.Told","Q.Sotal","% Sold","No.Of.Items"))

write.xlsx(sales_3,file=paste(dirl,month,"/",filename,"/sale3.xlsx",sep=""),sheetName="sheet1",row.names=F)

#
colour_1=QAT_1_DT[,sum(Quantity),by=c("colour")]
pie(colour_1$V1,labels=colour_1$colour)

#-----------------------------------------------------------------
#-----------------------------------------------------------------
#Quantity-Age-Colour
#MORE DATA RECONSTRUCT
QAT_3=QAT_1_DT[,sum(Quantity),by=c("colour","ageGroup")]
library(ggplot2)
require(grid)
count <- data.frame(Colour=factor(QAT_3$colour), 
                    Quantity=QAT_3$V1, 
                    AgeGroup=factor(QAT_3$ageGroup))
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(8, "Set1"))
ngroups <- length(unique(count$Colour))
p <- qplot(AgeGroup, Quantity, 
           data = count, 
           geom = "histogram", 
           stat = "bin2d",
           fill = Colour,
           xlab = "Age Group", 
           ylab = "Quantity",          
)
p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16,face="bold"))
p <- p + guides(shape=guide_legend(override.aes=list(size=14)))
p <- p + theme(legend.key.size = unit(1, "cm"))
p <- p + scale_fill_manual(values = cols(ngroups))
p
#
#-----------------------------------------------------------------
#Quantity-Product-Age
#MORE DATA RECONSTRUCT
QAT_4=QAT_1_DT[,sum(Quantity),by=c("productName","ageGroup"),]
count <- data.frame(Product.Name=factor(QAT_4$productName), 
                    Quantity=QAT_4$V1, 
                    AgeGroup=factor(QAT_4$ageGroup))
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(6, "Set1"))
ngroups <- length(unique(count$AgeGroup))
p <- qplot(Product.Name, Quantity, 
           data = count, 
           geom = "histogram", 
           stat = "bin2d",
           fill = AgeGroup,
           xlab = "Product Name", 
           ylab = "Quantity",          
)
p <- p + theme(axis.text.x = element_text(angle = 75, vjust=0.5,hjust = 0.5, size = 14,face="bold"))
p <- p + scale_fill_manual(values = cols(ngroups))
p <- p + theme(legend.key.size = unit(1.5, "cm"))
p
#
#-----------------------------------------------------------------
#-----------------------------------------------------------------
#Quantity-Product-colour
#MORE DATA RECONSTRUCT
QAT_6=QAT_1_DT[,sum(Quantity),by=c("productName","colour")]
count <- data.frame(Product.Name=factor(QAT_6$productName), 
                    Quantity=QAT_6$V1, 
                    Colour=factor(QAT_6$colour))
cols <- colorRampPalette(brewer.pal(9, "Set1"))
ngroups <- length(unique(count$Colour))
p <- qplot(Product.Name, Quantity, 
           data = count, 
           geom = "histogram", 
           stat = "bin2d",
           fill = Colour,
           xlab = "Product Name", 
           ylab = "Quantity",          
)
p <- p + theme(axis.text.x = element_text(angle = 65, hjust = 1, size = 16,face="bold"))
p <- p + scale_fill_manual(values = cols(ngroups))
p <- p + theme(legend.key.size = unit(1, "cm"))
p


