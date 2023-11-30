union_bound <- function(n,m,eps,size){4*size*exp(-n/2*(n+m)/(m+1)*eps^2)}


library(ecespa)
library(gurobi)
library(Biobase)
library(geometry)
data(gypsophylous)
x <- gypsophylous$x
y <- gypsophylous$y
X <-cbind(as.numeric(x),as.numeric(y))

N <- 300# 150#300

#X <- X[(51:100),]
X <- X[(1:N),]




bg=convex.incidence(X)
DIST=as.matrix(dist(X))
model=MILP.from.generic.base.from.convex.incidence(bg,DIST=as.matrix(dist(X)),maxdist=Inf,HOP=convex.H.obj)



idx <- which(DIST[22,]<=quantile(DIST[22,],0.5))
y <- rep(0,300)
set.seed(1234567)
y[idx] <- runif(length(idx))<= 0.85#5 #0.8
y[-idx] <-runif(300-length(idx)) <=0.2#15 #0.1
plot(X,col="white")

points(X[which(y==0),],pch=8,col="green")
points(X[which(y==1),],pch=3,col="black")

table(y)

objective <- oofos:::compute_objective(data.frame(y=y),"y","1")
model$obj <- objective
model2 <- simplify.geometry.model(model)

result <- gurobi(model2)
n_est=5.7*10^15
union_bound(n=min(table(y)),m=max(table(y)), eps =result$objval,size=n_est)



set.seed(1234567)
model_h0 <- model
model_h0$obj <- sample(model_h0$obj)
model_h0 <- simplify.geometry.model(model_h0)

model_h0$A <- rbind(model_h0$A,matrix(model_h0$obj,nrow=1))
model_h0$rhs <- c(model_h0$rhs,result$objval)
model_h0$sense <- c(model_h0$sense,">=")
model_h0$obj <- NULL
result_h02 <- gurobi(model_h0)

if(FALSE){
#alive=which(seedlings1$marks <= median(seedlings1$marks))
#dead=which(seedlings1$marks > median(seedlings1$marks))


#points(X[alive,],pch=8,col="green")
#points(X[dead,],pch=3,col="black")

 #legend(80,125,legend=c("lebend","tot"),col=c("green","black"),pch=c(8,3))

 v=rep(0,184)
 v[alive]=1/length(alive)
 v[dead]=-1/length(dead)

 bg=convex.incidence(X)
 DIST=as.matrix(dist(X))









## under H0
n_rep <- 100
optimization_results_h0 <- list()
objvals <- rep(0,n_rep)
set.seed(1234567)
for(k in (1:n_rep)){
model$obj <- sample(model$obj)
model2 <- simplify.geometry.model(model)
optimization_results_h0[[k]] <- gurobi(model2,list(timelimit=6000))
objvals[k] <- optimization_results_h0[[k]]$objval
print(mean(objvals[(1:k)]>=optimization_result$objval))
}



 ?n_est <- 10^15
   eps <- optimization_result$objval
   n <- min(table(model$obj))
 p_value <- 2*n_est*exp(-n*eps^2)

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
model$obj <- oofos:::compute_objective(data.frame(y=y),"y","1")

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


#### estimating |S|

n_rep <- 20
k_max <- 27
estimated_sizes <- array(0,c(n_rep,k_max))
set.seed(1234567)
start_time <- Sys.time()
for( k in (1:k_max)){
  KK <- as.numeric(k)
 for(l in (1:n_rep)){
  estimated_sizes[l,k] <- estimate_size_mingen_k_geometry_new(bg$context,k,bg)
# print(estimated_sizes[l,k])

  }

  print(k)}

end_time <- Sys.time()
total_time <- Sys.time() - start_time
total_time

z <- as.vector(estimated_sizes)*k_max
n_est <- mean(z)
t.test(z,conf.level=1-10^(-10),alternative="less")
abc.ci(z,statistic=weighted.mean,conf=1-10^(-10))
n_est*exp(-min(table(model$obj)*optimization_result$objval^2))


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

}
