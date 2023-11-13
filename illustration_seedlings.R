library(ecespa)
library(gurobi)

data(seedlings1)
x <- seedlings1$x
y <- seedlings1$y
X <-cbind(x,y)


plot(X,col="white")

alive=which(seedlings1$marks <= median(seedlings1$marks))
dead=which(seedlings1$marks > median(seedlings1$marks))


points(X[alive,],pch=8,col="green")
points(X[dead,],pch=3,col="black")

 #legend(80,125,legend=c("lebend","tot"),col=c("green","black"),pch=c(8,3))

 v=rep(0,184)
 v[alive]=1/length(alive)
 v[dead]=-1/length(dead)

 bg=convex.incidence(X)
 DIST=as.matrix(dist(X))



 idx <- which(DIST[5,]<=quantile(DIST[5,],0.4))
 y <- rep(0,184)
 set.seed(1234567)
 y[idx] <- runif(length(idx))<=0.75
 y[-idx] <-runif(184-length(idx)) <=0.15
 plot(X,col="white")

 points(X[which(y==0),],pch=8,col="green")
 points(X[which(y==1),],pch=3,col="black")



 v <- oofos:::compute_objective(data.frame(y=y),"y","1")
 model$obj <- v
 model$A=as.matrix(model$A)
 model2 <- simplify.geometry.model(model)
 ans <- gurobi(model2,list(timelimit=600))

 model=MILP.from.generic.base.from.convex.incidence(bg,DIST=as.matrix(dist(X)),maxdist=Inf,HOP=convex.H.obj2)
# model=MILP.from.minmin.base.from.convex.incidence(context=bg$context,X=bg$X,DIST=as.matrix(dist(X)),maxdist=Inf)
 model$obj <- v
 set.seed(1234567)

 vc_model <- oofos:::
 vcdim<- gurobi(vc_model)


center_index <- 13
i <- which(DIST[center_index,] <= median(DIST[center_index,]))
j <- which(DIST[center_index,] > median(DIST[center_index,]))
model$obj[i] <- runif(length(i))<=0.83
model$obj[j] <- runif(length(j))<=0.15

#for(k in (1:N)){model$obj[k] <- runif(1) <=exp(-DIST[1,k]/15)}
#table(model$obj)
model$obj <- oofos:::compute_objective(data.frame(y=model$obj),"y","1")

plot(X,col="white")

alive=which(model$obj >0)
dead=which(model$obj <0)
points(X[alive,],pch=8,col="green")
points(X[dead,],pch=3,col="black")
#plot(X)
#points(X[which(model$obj>0),],col="red")
 ans <- gurobi(model)


h0 <- rep(0,100)
for(k in (1:100)){
  model$obj <- sample( model$obj)
  h0[k] <- gurobi(model,list(outputflag=0))$objval
  print(mean(h0[(1:k)]>=ans$objval))
  }



n_rep <- 2
k_max <- 25
ans <- array(0,c(n_rep,k_max))
for( k in (1:k_max)){
for(l in (1:n_rep)){

ans[l,k] <- estimate_size_mingen_k_geometry(bg$context,k,bg)
print(ans[l,k])

}}
n_est <- sum(colMeans(ans))
n_est*exp(-min(table(model$obj)*ans$objval^2))

 model=MILP.from.generic.base.from.convex.incidence(bg,DIST=as.matrix(dist(X)),maxdist=Inf,HOP=convex.H.obj2)
 model$obj=v
 ans=gurobi(model)

 ans$objval

 #[1] 0.372468


 # Permutationstest

 nrep=100

 a=rep(0,nrep)
 for(k in (1:nrep)){
   model$obj=sample(v,size=100)

   model2=simplify.geometry.model(model)   ## weitere Vereinfachung des Modells (Implikationen mit PrÃ¤missen, deren zug v-Werte teils negativ sind kÃ¶nnen vermutlich weggelassen werden)

   temp=gurobi(model2,list(outputflag=1))
   a[k]=temp$objval
   print(k)
   print(mean(a[(1:k)]<=0.372468))
  }

