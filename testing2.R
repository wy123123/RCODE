library(outliers)
y=as.vector(data_4$V1)
summary(y)
outlierTest(y)
upper=mean(y)+3*sd(y)
lower=mean(y)-3*sd(y)
test=data_4
test_1=test[test$V1>lower&test$V1<=upper,]

hist(log(y[y$fit.cluster==2,]$y))
wss <- (nrow(y)-1)*sum(apply(y,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(y, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(y, 2)
aggregate(y,by=list(fit$cluster),FUN=mean)
y <- data.frame(y, fit$cluster)
y
