library(data.table)
library(reshape2)
library(xlsx)

dir <- "C:/Users/Lovebonito/Downloads/Launch Detail/Launch Item Detail "
date <- c("11 Feb","15 Feb","25 Feb","4 March","11 March")

mf <-read.csv("C:/Users/Lovebonito/Downloads/launch sales report/TEST/cut master file.csv")
df=subset(mf,select=c("Product.name","Price","Quantity.Sold","month"))
hist(log(df$Quantity.Sold),breaks=70)
shapiro.test(log(df[df$month==2,]$Quantity.Sold))
for( i in 1 : length(date)){
        dt <- read.csv(paste0(dir,date[i],".csv"),skip=2)
        if(i == 1){
                dt.all=dt
        }
        else{
                dt.all <- rbind(dt.all,dt)
        }
}

m.d=merge(dt.all,df,by.x="Product.Name",by.y="Product.name")
md.1=m.d[,-1]#remove product name
md.1$month=as.factor(md.1$month)
dt.c=data.frame(model.matrix(~., md.1))
dt.c=dt.c[,-1]#remove interception

library(caret)
library(homals)
fit=homals(subset(dt.c,select=-Quantity.Sold))

table(subset(dt.c,select=-Quantity.Sold)[,2::6])

library(MASS)
fit=lm(Quantity.Sold~., data=dt.c)
model=stepAIC(fit,direction = c("both", "backward", "forward"),trace=0)
summary(model)
table(dt.all$Category)
library(ggplot2)
ggplot(data=dt,aes(x=V1,y=seq(1:length(V1))))+geom_point()
