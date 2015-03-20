#read train data
library(data.table)
dir="C:/Users/Lovebonito/Downloads/reading/getting clearn data/New folder"
train.label=read.table(paste(dir,"/UCI HAR Dataset/train/y_train.txt",sep=""))
train.x=read.table(paste(dir,"/UCI HAR Dataset/train/x_train.txt",sep=""))
subject.train=read.table(paste(dir,"/UCI HAR Dataset/train/subject_train.txt",sep=""))

#read testing data
test.label=read.table(paste(dir,"/UCI HAR Dataset/test/y_test.txt",sep=""))
test.x=read.table(paste(dir,"/UCI HAR Dataset/test/x_test.txt",sep=""))
subject.test=read.table(paste(dir,"/UCI HAR Dataset/test/subject_test.txt",sep=""))

#read feature and activity label
dt.feature=read.table(paste(dir,"/UCI HAR Dataset/features.txt",sep=""))
dt.activity=read.table(paste(dir,"/UCI HAR Dataset/activity_labels.txt",sep=""))

#combine test and training data.
dt.all=cbind(c(subject.train$V1,subject.test$V1),c(train.label$V1,test.label$V1),rbind(train.x,test.x))
colnames(dt.all)[1:30]

#name the vairables according to feature
setnames(dt.all,c("Subject","Activity",as.character(dt.feature$V2)))

# get the activity
dt.all$Activity=dt.activity$V2[match(dt.all$Activity,dt.activity$V1)]

#find mean and std
dt_mean_std=data.table(dt.all[,c(1:2,grep("mea",colnames(dt.all)),grep("std",colnames(dt.all)))])

#find average and rename variables
dt_avg=dt_mean_std[,lapply(.SD,mean,na.rm=T),by=list(Subject,Activity)]
setnames(dt_avg,3:length(colnames(dt_avg)),paste("average.",colnames(dt_avg)[3:length(colnames(dt_avg))],sep=""))
write.csv(dt_avg,file=paste(dir,"/UCI tidyData.csv",sep=""),row.names=F)

write.csv(colnames(dt_avg),file=paste(dir,"/U11.csv",sep=""),row.names=F)
unlist(t(colnames(dt_avg)))
