data11=data.table(read.csv("C:/Users/Lovebonito/Downloads/201411/MF11.csv"))
data10=data.table(read.csv("C:/Users/Lovebonito/Downloads/201410/MF10.csv"))
data09=data.table(read.csv("C:/Users/Lovebonito/Downloads/201409/MF09.csv"))
data08=data.table(read.csv("C:/Users/Lovebonito/Downloads/201408/MF08.csv"))
data07=data.table(read.csv("C:/Users/Lovebonito/Downloads/201407/MF07.csv"))
data06=data.table(read.csv("C:/Users/Lovebonito/Downloads/201406/MF06.csv"))
data05=data.table(read.csv("C:/Users/Lovebonito/Downloads/201405/MF05.csv"))
data04=data.table(read.csv("C:/Users/Lovebonito/Downloads/201404/MF04.csv"))
data03=data.table(read.csv("C:/Users/Lovebonito/Downloads/201403/MF03.csv"))
data02=data.table(read.csv("C:/Users/Lovebonito/Downloads/201402/MF02.csv"))
data01=data.table(read.csv("C:/Users/Lovebonito/Downloads/201401/MF01.csv"))
library(data.table)
data_MF=data.table(rbind(data10,data09,data08,data07,data06,data05,data04,data03,data02,data01))
DT=data_MF[,sum(Quantity),by="ProductCode"]
DT_1=data11[,sum(Quantity),by=c("ProductCode","ProductName")]
indicator=rep(0,length(DT_1$V1))
for(i in 1:length(DT_1$V1)){
  for(s in 1:length(DT$V1)){ 
    if(as.vector(DT$ProductCode[s])==as.vector(DT_1$ProductCode[i]))
      indicator[i]= 1
  }
}
table(indicator)
DT_2=cbind(DT_1,indicator)
DT_3=DT_2[DT_2$V1>=10&DT_2$indicator==1,]
DT_4=data.table(DT_3[order(-DT_3$V1),])
best_seller_category=data.table(best_seller_category)
ind2=rep(0,length(best_seller_category$V1)*2)
ind2=matrix(ind2,ncol=2)
for(i in 1:length(best_seller_category$V1)){
  for(s in 1 :length(DT_4$V1)){
    if(as.vector(DT_4$ProductCode[s])==as.vector(best_seller_category$ProductCode[i]))
      {
      ind2[i,1]= 1
      ind2[i,2]=as.vector(DT_4$V1[s])
    }
  }
}
table(ind2[,1])
table(ind2[,2])
yyy=cbind(best_seller_category,ind2)
colnames(yyy)=c("ProductCode","Category","Overallsale","ind2","saleonNov")
table(yyy$Category)
DD=yyy[yyy$ind2==1&yyy$Category=="Dresses",]
TT=yyy[yyy$ind2==1&yyy$Category=="Tops",]
BB=yyy[yyy$ind2==1&yyy$Category=="Bottoms",]
CC=yyy[yyy$ind2==1&yyy$Category=="COVET By Love, Bonito",]
BR=yyy[yyy$ind2==1&yyy$Category=="Bridesmaids",]
OP=yyy[yyy$ind2==1&yyy$Category=="One Piece Suits",]
DD=DD[order(-DD$saleonNov)]
TT=TT[order(-TT$saleonNov)]
BB=BB[order(-BB$saleonNov)]
CC=CC[order(-CC$saleonNov)]
BR=BR[order(-BR$saleonNov)]
OP=OP[order(-OP$saleonNov)]
write.csv(DD,file="DD.csv")
write.csv(TT,file="TT.csv")
write.csv(BB,file="BB.csv")
write.csv(CC,file="CC.csv")
write.csv(BR,file="BR.csv")
write.csv(OP,file="OP.csv")

#findMonth
DT1=data01[,sum(Quantity),by="ProductCode"]
DT2=data02[,sum(Quantity),by="ProductCode"]
DT3=data03[,sum(Quantity),by="ProductCode"]
DT4=data04[,sum(Quantity),by="ProductCode"]
DT5=data05[,sum(Quantity),by="ProductCode"]
DT6=data06[,sum(Quantity),by="ProductCode"]
DT7=data07[,sum(Quantity),by="ProductCode"]
DT8=data08[,sum(Quantity),by="ProductCode"]
DT9=data09[,sum(Quantity),by="ProductCode"]
DT10=data10[,sum(Quantity),by="ProductCode"]
DT11=cbind(DT1,1)
DT22=cbind(DT1,2)
DT33=cbind(DT1,3)
DT44=cbind(DT1,4)
DT55=cbind(DT1,5)
DT66=cbind(DT1,6)
DT77=cbind(DT1,7)
DT88=cbind(DT1,8)
DT99=cbind(DT1,9)
DT1010=cbind(DT1,10)
DTA=data.table(rbind(DT11,DT22,DT33,DT44,DT55,DT66,DT77,DT88,DT99,DT1010))
DTA1=DTA[,sum(V1),by=c("V2","ProductCode")]
