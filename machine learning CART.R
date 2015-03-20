library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle,rpart.plot)
library(rpart)
inTrain=createDataPartition(y=segmentationOriginal$Case,p=0.80,list=FALSE)
training=segmentationOriginal[inTrain,]
testing=segmentationOriginal[-inTrain,]
set.seed(125)
modfit=train(Class~.,method="rpart",data=training)
fancyRpartPlot(modfit$finalModel)


library(pgmm)
data(olive)
olive = olive[,-1]
head(olive)
inTrain=createDataPartition(y=olive$Area,p=0.80,list=FALSE)
training=olive[inTrain,]
testing=olive[-inTrain,]
modfit=train(Area~.,method="rpart",data=training)
newdata = as.data.frame(t(colMeans(olive)))
predict(modfit,newdata=newdata)


library(ElemStatLearn)
data(SAheart)
set.seed(13234)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
head(trainSA)
modfit=train(chd~age+alcohol+obesity+tobacco+typea+ldl ,method="glm",family="binomial",data=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
#find miss rate of the training set
prediction=predict(modfit,trainSA)
missClass(trainSA$chd,prediction)
#find miss rate of the test set
prediction=predict(modfit,testSA)
missClass(testSA$chd,prediction)


library(ElemStatLearn)
library(randomForest)
data(vowel.train)
data(vowel.test) 
set.seed(33833)
head(vowel.train)
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
modfit=train(y~.,method="rf",data=vowel.train)
varImp(modfit)


library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
set.seed(33833)
head(vowel.train)
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
modfit=train(y~.,method="rf",data=vowel.train)
confusionMatrix(vowel.test$y,predict(modfit,vowel.test))
modfit.2=train(y~.,method="gbm",data=vowel.train,verbose=F)
confusionMatrix(vowel.test$y,predict(modfit.2,vowel.test))



library(caret)
library(gbm)
set.seed(62433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
modfit.1=train(diagnosis~.,method="rf",data=training)
modfit.2=train(diagnosis~.,method="gbm",data=training,verbose=F)
modfit.3=train(diagnosis~.,method="lda",data=training)
confusionMatrix(testing$diagnosis,predict(modfit.1,testing))
confusionMatrix(testing$diagnosis,predict(modfit.2,testing))
confusionMatrix(testing$diagnosis,predict(modfit.3,testing))
pred.1=predict(modfit.1,testing)
pred.2=predict(modfit.2,testing)
pred.3=predict(modfit.3,testing)
comb=data.frame(pred.1,pred.2,pred.3,diagnosis=testing$diagnosis)
combfit=train(diagnosis~.,method="rf",data=comb)
confusionMatrix(testing$diagnosis,predict(combfit,comb))


set.seed(233)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
modfit=train(CompressiveStrength~.,method="lasso",data=training)
plot.enet(modfit$finalModel, xvar = "penalty", use.color = TRUE)



library(lubridate)  # For year() function below
dat = read.csv("C:/Users/Lovebonito/Downloads/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
require(forecast)
a=bats(tstrain)
p=forecast(a,h=235)
str(p)
s=(testing$visitsTumblr<=p$upper[,2]) * (testing$visitsTumblr>=p$lower[,2])
table(s)
226/235*100


require(e1071)
set.seed(325)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
model <- svm(CompressiveStrength ~ ., data = training)
predict(model,testing)
require(hydroGOF)
rmse(predict(model,testing),testing$CompressiveStrength)
