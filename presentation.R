library(ecespa)
library(gurobi)

data(gypsophylous)
x <- gypsophylous$x[(1:250)]
y <- gypsophylous$y[(1:250)]
X <- cbind(x,y)




#bg=convex.incidence(X)
DIST=as.matrix(dist(X))



idx <- which(DIST[22,]<=quantile(DIST[22,],0.4))
y <- rep(0,250)
set.seed(1234567)
y[idx] <- runif(length(idx))<=0.8
y[-idx] <-runif(250-length(idx)) <=0.1

pdf("non_randomly_distributed.pdf")
plot(X,col="white",xlab="",ylab="")

points(X[which(y==0),],pch=8,col="green")
points(X[which(y==1),],pch=3,col="black")
dev.off()

## with noise
dev.new()
yy <- sample(y)

pdf("randomly_distributed.pdf")
plot(X,col="white",xlab="",ylab="")

points(X[which(yy==0),],pch=8,col="green")
points(X[which(yy==1),],pch=3,col="black")
dev.off()
