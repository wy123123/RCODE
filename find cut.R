DT_alll=data.table(read.csv("C:/Users/Lovebonito/Downloads/launch sales report/TEST/DTTT.csv",header=T))
DT_all=subset(DT_alll,select=c("Product.name","Product.SKU","Price","Total.Quantity","Quantity.Sold","month"))
###########
DT.1=DT_all[substring(DT_all$Product.SKU,1,1)=="S",]
write.csv(DT.1,file="C:/Users/Lovebonito/Downloads/shoes.csv",row.names=FALSE)
###########
i1=grepl("Pant",DT_all$Product.name)
DT_1=DT_all[i1,]
s=strsplit(as.vector(DT_1$Product.name)," ")
s=ldply(s,rbind)
write.csv(DT_1,file="C:/Users/Lovebonito/Downloads/shift.csv",row.names=FALSE)
#######
DT=data.table(read.csv("C:/Users/Lovebonito/Downloads/Mesh.csv",header=T))
i1=grepl("Midi",DT$Product.name)
DT_1=DT[!i1,]
i1=grepl("Mini",DT_1$Product.name)
DT_2=DT_1[!i1,]
i1=grepl("Pencil",DT_2$Product.name)
DT_3=DT_2[!i1,]
i1=grepl("Bodycon",DT_3$Product.name)
DT_4=DT_3[!i1,]
write.csv(DT_1,file="C:/Users/Lovebonito/Downloads/midi.csv",row.names=FALSE)

i1=grepl("Pants",DT_all$Product.name)
i2=grepl("Culottes",DT_all$Product.name)
DT_1=DT_all[i1,]
DT_2=DT_all[i2,]
DT_F=unique(rbind(DT_1,DT_2))
write.csv(DT_F,file="C:/Users/Lovebonito/Downloads/ic.csv",row.names=F)

i1=grepl("Origami",DT_all$Product.name)
i2=grepl("Peplum",DT_all$Product.name)
i3=grepl("Layer",DT_all$Product.name)
DT_1=DT_all[i1,]
DT_2=DT_all[i2,]
DT_3=DT_all[i3,]
DT_F=unique(rbind(DT_1,DT_2,DT_3))
write.csv(DT_F,file="C:/Users/Lovebonito/Downloads/CUT/Peplum.csv")
