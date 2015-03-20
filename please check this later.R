score <- c(.06,2.17,2.46,2.67,2.86,3.01,3.17,3.34,3.43,3.45,
           3.46,3.48,3.5,3.52,3.54,3.56,3.58,3.6,3.62,3.65,3.67,3.69,
           3.71,3.74,3.77,3.79,3.82,3.85,3.88,3.91,3.94,3.96,4.0,4.0)
perc.ranks <- c(0,10,20,30,40,50,60,70,75,76,77,78,79,80,81,
                82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100)
fn <- ecdf(perc.ranks)
xs <- score
ys <- fn(perc.ranks)
slope <- rep(NA,length(xs))
for (i in 2:length(xs)) {
        slope[i] <- (ys[i]-ys[i-1])/(xs[i]-xs[i-1])
}
slope[1] <- 0
slope[length(xs)] <- slope[length(xs)-1]
par(mfrow=c(1,2))
plot(xs,slope,type="l",main="Estimated PDF")
plot(xs,ys,type="l",main="Estimated CDF")
dev.off()

hist(score)
fit1 <- fitdistr(score,densfun="beta") 
Q=fit1$estimate[1]
R=fit1$estimate[2]
# goodness of fit test
ks.test(score, "pbeta",shape=Q,rate=R) # p-value > 0.05 -> distribution not refused
# plot a graph
hist(score, freq = FALSE, breaks = 30, xlim = c(0, quantile(score, 0.99)),main="Dresses",xlab="Quantity")
curve(dgamma(x, shape=Q,rate =R), col = "red", add = TRUE)

x <- seq(0,4,length=100)
hx <- dstable(x, alpha=0.5, beta=0.75, gamma=1, delta=3.2)
plot(x,hx,type="l",lty=2,lwd=2)
??dstable
