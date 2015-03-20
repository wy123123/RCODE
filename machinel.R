url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
file <- file.path(getwd(),"m.csv")
download.file(url,file)
dt <- read.csv(file)

url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
file <- file.path(getwd(),"t.csv")
download.file(url,file)
dtt <- read.csv(file)

colnames(dt)
head(dt$roll_belt,10)
head(dt$pitch_belt)

##################
#feature selection
##################
dt.1 <- subset(dt,select=-c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window))

dt.2=dt.1[,colSums(is.na(dt.1))<19622*0.1]

dt.3 <- nearZeroVar(dt.2,saveMetrics=T)

dt.4 <- dt.2[,!dt.3$nzv]
dt.4.v <- subset(dt.4,select=-classe)

library(caret)
library(MASS)

##################
mod.1 <- train(classe~.,method="rf",data=data.4,number=5,repeats=1,ntree=1)

prediction <- predict(mod.1,dt.4)
confusionMatrix(prediction,dt.4$classe)
varImp(mod.1)

##################
#k-fold
##################
set.seed(123)
folds <- createFolds(y=dt.4$classe,k=20,list=T,returnTrain=T)
sapply(folds,length)
folds
