library(xlsx)
library(data.table)
####################################################
month=c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec")
for(i in 1 : 12){
        url=paste("C:/Users/Lovebonito/Desktop/wan yin finance/",i,".csv",sep = "")
        #url=paste("C:/Users/Lovebonito/Desktop/d/12.csv")
        dt=read.csv(url)
        if(i==1){
                dt.all=dt
        }
        dt=dt[-c(length(dt[,1]),length(dt[,1])-1),]
        dt.all=rbind(dt.all,dt)
        s=strsplit(as.character(dt$Transaction.Date),split=" ")
        m=matrix(unlist(s),ncol=2,byrow=T)
        dates <- strftime(m[,1],"%Y-%m-%d",usetz = FALSE)
        
        dt.12=data.table(cbind(dt,dates))
        
        table(dt.12$Bank)
        
        dt.a=dt.12[dt.12$Bank!="AMEX",]
        a=dt.a[,sum(Amount),by=list(dates)]
        
        dt.b=dt.12[dt.12$Bank == "AMEX",]
        b=dt.b[,sum(Amount),by=list(dates)]
        
        a$AMEX=b$V1[match(a$dates,b$dates)]
        
        all=data.frame(Dates=a$dates,ENETS=a$V1,AMEX=a$AMEX)
        setnames(all,c("Dates","ENETS","AMEX"))
        
        c=data.frame(Dates=c("sum_all",sum(all$ENETS,all$AMEX,na.rm=T)),ENETS=c("sum_enets",sum(all$ENETS)),AMEX=c("sum_amex",sum(all$AMEX,na.rm=T)))
        if(sum(is.na(all$AMEX))!= 0){
                all[is.na(all$AMEX),]$AMEX = 0
        }
        
        f=rbind(all,c)
        write.xlsx(f,"C:/Users/Lovebonito/Desktop/final.xlsx",sheetName=month[i],append=T)
}
########
#AMEX
########
for( i in 1:12){
        dt=read.xlsx("C:/Users/Lovebonito/Desktop/d/final.xlsx",sheetName=month[i])
        dt.AMEX=data.frame(Dates=dt$Dates,AMEX=dt$AMEX)
        write.xlsx(dt.AMEX,"C:/Users/Lovebonito/Desktop/d/AMEX.xlsx",sheetName=month[i],append=T,row.names=F)
}
########
##Jan
########
Jan=(read.table("C:/Users/Lovebonito/Desktop/d/AMEX/AMEX Jan.txt",header=FALSE,sep=" "))
Jan.1=0
for(i in 1 :198){
        Jan.1[i]=as.vector(Jan[1,i])
}
df=data.frame(matrix(Jan.1,ncol=6,byrow=T))
setnames(df,c("Submission.Dates","Summary","Gross.AMT","Net.AMT","Settlement.AMT","Settlement.Date"))
write.xlsx(df,"C:/Users/Lovebonito/Desktop/d/AMEX/jan.xlsx",sheetName="1",row.names=F)

df.2=read.xlsx("C:/Users/Lovebonito/Desktop/d/AMEX/2paymentstatement-Feb14.xlsx",sheetIndex=1,startRow=3,header=F, stringsAsFactors=F)
df.f=data.frame(Submission.date=df.2$X5,Settlment.date=df.2$X6,Gross.AMT=df.2$X9,Discount.AMT=df.2$X12,Net.AMT=df.2$X15,ds=df.2$X17)
df.f=df.f[!is.na(df.f$Submission.date),]
write.xlsx(df.f,"C:/Users/Lovebonito/Desktop/d/AMEX/feb.xlsx",sheetName="1",row.names=F)

df.2=read.xlsx("C:/Users/Lovebonito/Desktop/d/AMEX/12paymentstatement-Dec14.xlsx",sheetIndex=1,startRow=4,header=F, stringsAsFactors=F)
df.f=data.frame(Submission.date=df.2$X5,Settlment.date=df.2$X6,Gross.AMT=df.2$X9,Discount.AMT=df.2$X12,Net.AMT=df.2$X15,ds=df.2$X17)
df.f=df.f[!is.na(df.f$Submission.date),]
df.f$Submission.date=as.Date(as.character(df.f$Submission.date),origin = "1899-12-30")
write.xlsx(df.f,"C:/Users/Lovebonito/Desktop/d/AMEX/Dec.xlsx",sheetName="1",row.names=F)


Janx=read.xlsx("C:/Users/Lovebonito/Desktop/d/AMEX.xlsx",sheetName="Jan")
Janx.1=Janx[!is.na(Janx$AMEX),]
Janx.1=Janx.1[as.numeric(as.character(Janx.1$AMEX))!=0,]
Janx.1=Janx.1[!is.na(Janx.1$AMEX),]
Janx.2=Janx.1[substring(as.character(Janx.1$Dates),1,1)=="2",]

df$Submission.Dates=strptime(df$Submission.Dates,"%d/%m/%y")
df$Settlement.Date=strptime(df$Settlement.Date,"%d/%m/%y")
Janx.2$Dates=strptime(Janx.2$Dates,"%Y-%m-%d")
Janx.2$Dates[29] == df$Submission.Dates[4]
df.1=df[!duplicated(df[,c(5,6)]),c(5,6)]

df$Dates=Janx.2$Dates[match(df$Submission.Dates,Janx.2$Dates)]
df$AMT=Janx.2$AMEX[match(df$Submission.Dates,Janx.2$Dates)]
df$Date.S=df.1$Settlement.AMT[match(df$Submission.Dates,df.1$Settlement.Date)]

table(match(Janx.2$Dates,df$Submission.Dates))
Janx.2$Dates[Janx.2$Dates %in% df$Submission.Dates]
################################################
#read bank statement
###############################################

dir="C:/Users/Lovebonito/Desktop/wan yin finance/statement"
month=c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec")
###############################################
#paypal
###############################################
dir <- "C:/Users/Lovebonito/Desktop/wan yin finance/paypal/New folder/MonthlyFinancialSummary "
month <- formatC(seq(1,12,by=1),width = 2, format = "d", flag = "0")
for(i in 1 : 2){
        dt <- read.csv(paste0(dir,month[i]," 2015.csv"),skipNul=T,header=F,stringsAsFactors =F)
        dt <- data.table(dt[5:length(dt$V1),],stringsAsFactors =F)
        dt$V1 <- as.Date(dt$V1,"%d/%m/%Y")
        dt=dt[!is.na(dt$V1),]
        dt.t=dt[is.na(as.numeric(dt$V8)),]
        dt.t1=dt[!is.na(as.numeric(dt$V8)),]
        dt.t$V7=dt.t$V8
        dt.t$V8=dt.t$V9
        dt.t$V9=dt.t$V10
        dt=rbind(dt.t,dt.t1)
        dt$V8=as.numeric(gsub(",","",dt$V8))
        dt$V9=as.numeric(gsub(",","",dt$V9))       
        dt.summary <- dt[,lapply(.SD, sum ,na.rm=T),by=list(V1,V2),.SDcols=c("V8","V9")]
        dt.summary <- dt.summary[order(dt.summary$V1),]
        setnames(dt.summary,c("Date","Description","Gross Amount","Net Amount"))
        write.xlsx(dt.summary,file=paste0(dir,"paypal",".xlsx"),sheetName=paste(month[i],"2015"),append=T,row.names=F)
}

View(table(dt$V2,dt$V1))
