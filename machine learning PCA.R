library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData=data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
dim(training);dim(testing)

data(concrete)
set.seed(975)
inTrain=createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training=mixtures[inTrain,]
testing=mixtures[-inTrain,]
library(Hmisc)
library(ggplot2)
qplot(seq(from=1,to=length(CompressiveStrength)),CompressiveStrength,data=training,color=cut2(FlyAsh))

set.seed(975)
inTrain=createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training=mixtures[inTrain,]
testing=mixtures[-inTrain,]
hist(as.numeric(training$Superplasticizer))

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

library(kernlab)
data(spam)
install.packages("e1071")
inTrain=createDataPartition(y=spam$type,p=0.75,list=FALSE)
training=spam[inTrain,]
testing=spam[-inTrain,]
preProc=preProcess(log10(spam[,-58]+1),method="pca",pcaComp=30)
spamPC=predict(preProc,log10(spam[,-58]+1))
trainPC=predict(preProc,log10(training[,-58]+1))
modelFit=train(training$type ~ .,method="glm",data=trainPC)
testPC=predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))
summary(modelFit)



library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
?grep
IL_str <- grep("^IL", colnames(training), value = TRUE)
preProc <- preProcess(training[, IL_str], method = "pca", thresh = 0.95)
preProc$rotation

set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]
IL_str <- grep("^IL", colnames(training), value = TRUE)
## make a subset of these predictors
predictors_IL <- predictors[, IL_str]
df <- data.frame(diagnosis, predictors_IL)
inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
training = df[inTrain, ]
testing = df[-inTrain, ]

## train the data using the first method
modelFit <- train(diagnosis ~ ., method = "glm", data = training)

predictions <- predict(modelFit, newdata = testing)
## get the confustion matrix for the first method
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)

A1 <- C1$overall[1]

## do similar steps with the caret package
modelFit <- train(training$diagnosis ~ ., method = "glm", preProcess = "pca", 
                  data = training, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)
