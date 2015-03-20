data01=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_01.csv",header=T)
data02=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_02.csv",header=T)
data03=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_03.csv",header=T)
data04=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_04.csv",header=T)
data05=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_05.csv",header=T)
data06=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_06.csv",header=T)
data07=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_07.csv",header=T)
data08=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_08.csv",header=T)
data09=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_09.csv",header=T)
data10=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_10.csv",header=T)
data11=read.csv("C:/Users/Lovebonito/Downloads/Monthly report/New Customer/new_customer_11.csv",header=T)
library(data.table)
data01=data.table(data01)
data01_1=data01[,sum(TotalSpent),by="PhoneMobile"]

data02=data.table(data02)
data02_1=data02[,sum(TotalSpent),by="PhoneMobile"]

data03=data.table(data03)
data03_1=data03[,sum(TotalSpent),by="PhoneMobile"]

data04=data.table(data04)
data04_1=data04[,sum(TotalSpent),by="PhoneMobile"]

data05=data.table(data05)
data05_1=data05[,sum(TotalSpent),by="PhoneMobile"]

data06=data.table(data06)
data06_1=data06[,sum(TotalSpent),by="PhoneMobile"]

data07=data.table(data07)
data07_1=data07[,sum(TotalSpent),by="PhoneMobile"]

data08=data.table(data08)
data08_1=data08[,sum(TotalSpent),by="PhoneMobile"]

data09=data.table(data09)
data09_1=data09[,sum(TotalSpent),by="PhoneMobile"]

data10=data.table(data10)
data10_1=data10[,sum(TotalSpent),by="PhoneMobile"]

data11=data.table(data11)
data11_1=data11[,sum(TotalSpent),by="PhoneMobile"]
