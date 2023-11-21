PHI=function(A,context){  ##  Ableitungsoperator Phi
  i=which(A==1)
  temp=as.matrix(context[,i])
  dim(temp)=c(dim(context)[1],length(i))
  S=rowSums(temp)
  j=which(S==length(i))
  ans=rep(0,dim(context)[1])
  ans[j]=1
  return(ans)}

PSI=function(B,context){    ## Ableitungsoperator Psi
  i=which(B==1)
  temp=as.matrix(context[i,])
  dim(temp)=c(length(i),dim(context)[2])
  S=colSums(temp)
  j=which(S==length(i))
  ans=rep(0,dim(context)[2])
  ans[j]=1
  return(ans)}

is_minimal_generator <- function(set,context){
  extent <- ddandrda:::operator_closure_obj_input(set,context)
  if(sum(set)==0){return(TRUE)}
  indexs <- which(set==1)
  for(k in indexs){
    new_set <- set;new_set[k] <- 0
    new_extent <- ddandrda:::operator_closure_obj_input(new_set,context)
    if(all(extent==new_extent)){return(FALSE)}
  }
  return(TRUE)


}





context_is_meet_distributive <- function(context){
  #context <- unique(context)
  n_objects <- nrow(context)
  for(k in seq_len(n_objects-1)){
    for(l in ((k+1):n_objects)){

     indexs <- which(context[k,]!=context[l,])

     indexs2 <- which(context[k,]==0 & context[l,]==0)
     for( m in indexs2){
      indexs3 <- which(context[,m]==1)
      if(length(indexs3)>=1){
         if(all(colSums(matrix(context[indexs3,indexs],nrow=length(indexs3)))<length(indexs3))){context[l,m]=1;m <<- m;l <<- l; k <<- k;indexs <<- indexs; indexs2 <<- indexs2; indexs3 <<- indexs3;return(list(result=FALSE,new_context=context))}
      }


      #context <<- context;print(c(k,l));print(m);print(indexs3);indexs <<- indexs;indexs2 <<- indexs2;indexs3 <<-indexs3;return(FALSE)}
    }

}


}
return(list(result=TRUE))}

estimate_concept_lattice_size=function(context,nrep){    #### Schätzt Anzahl von formalen Begriffen über Monte-Carlo-Simulation
  m=dim(context)[2]
  a=rep(0,nrep)
  for(k in (1:nrep)){
   temp=runif(m)>=0.5
   H=oofos:::operator_closure_attr_input(temp,context)
   if(all(H==temp)){a[k]=1}
  }
return(mean(a)*2^m)}


is_freely_addable <- function(set,element,context,hop=ddandrda:::operator_closure_obj_input){

if(set[element]==1){return(FALSE)}
if(all(set==0)){return(TRUE)}
if((hop(set,context))[element]==1){return(FALSE)}
for(k in which(set==1)){

new_set <- set
new_set[k] <-0
new_set[element] <- 1
if((hop(new_set,context))[k]==1){return(FALSE)}
}

return(TRUE)
}


estimate_size_mingen_k <- function(context,k){

set <- rep(0,nrow(context))
set[sample(seq_len(nrow(context)),size=1)] <- 1

size <- as.numeric(nrow(context))
while(TRUE){
if(sum(set)==k){return(size/factorial(k))}
indexs <- NULL
for(i in seq_len(nrow(context))){
   if(is_freely_addable(set,i,context)){
      indexs <- c(indexs,i)
    }
}
    if(is.null(indexs)){return(0)}
    size <- size*length(indexs)
    index <- sample(c(indexs,indexs),size=1)
    set[index] <- 1
#print(set)



}
}


## geometry


is_freely_addable_geometry <- function(set,element,context,hop=convex.H.obj,bg){

  if(set[element]==1){return(FALSE)}
  if(all(set==0)){return(TRUE)}
  if((hop(set,bg))[element]==1){return(FALSE)}
  for(k in which(set==1)){

    new_set <- set
    new_set[k] <-0
    new_set[element] <- 1
    if((hop(new_set,bg))[k]==1){return(FALSE)}
  }

  return(TRUE)
}


estimate_size_mingen_k_geometry <- function(context,k,bg){

  set <- rep(0,nrow(context))
  set[sample(seq_len(nrow(context)),size=1)] <- 1

  size <- as.numeric(nrow(context))
  while(TRUE){
    if(sum(set)==k){return(size/factorial(k))}
    indexs <- NULL
    for(i in seq_len(nrow(context))){
      if(is_freely_addable_geometry(set,i,context,bg=bg)){
        indexs <- c(indexs,i)
      }
    }
    if(is.null(indexs)){return(0)}
    size <- size*length(indexs)
    index <- sample(c(indexs,indexs),size=1)
    set[index] <- 1
    #print(set)



  }
}


convex.incidence=function(X){  ## gegeben Puntmenge von M Punkten in R^2 (uebergeben als M x 2 Matrix X), wird Kontext mit G= Menge der R^2 Punkte und M = Menge aller durch je zwei verschiedene Punkte aus G beschriebenen Halbräume sowie gIm iff Punkt g liegt in (abgeschlossenem) Halbraum m
  n=dim(X)[1]
  m=n*(n-1)/2
  I1=array(0,c(n,m))
  I2=I1
  t=1
  indexs=array(0,c(m,2))
  NAMES=rep("",m)
  NAMES2=rep("",m)
  for(k in (1:(n-1))){
    print(k)
    for(l in ((k+1):n)){
      NAMES[t]=paste(rownames(X)[c(k,l)],collapse="")
      NAMES2[t]=paste(rownames(X)[c(l,k)],collapse="")

      for(M in (1:n)){
        v1=X[k,]-X[l,]
        v2=X[M,]-X[l,]
        indexs[t,]=c(k,l)
        s=v1[1]*v2[2]-v1[2]*v2[1]
        if(s>0){I1[M,t]=1}
        if(s<0){I2[M,t]=1}
        if(s==0){I2[M,t]=1;I1[M,t]=1}
      }
      t=t+1
    }
  }
  colnames(I1)=NAMES
  colnames(I2)=NAMES2
  rownames(I1)=rownames(X)
  rownames(I2)=rownames(X)

  return(list(context=(cbind(I1,I2)),indexs=rbind(indexs,indexs),X=X))}



convex.H.obj2=function(A,bg){ ##  benötigt Package geometry und Package Biobase, macht das gleiche wie  convex.H.obj, scheint aber oft schneller zu sein als convex.H.obj
  A <<- A

  if(sum(A)==1){return(A)}
  if(sum(A) !=3){return(H.obj(A,bg))}
  temp=cart2bary(bg$X[as.logical(A),],bg$X)
  if(is.null(temp)){print("warning: degenerate simplex");return(H.obj(A,bg$context))}
  temp2=rowMin(temp)>=0
  return(temp2*1)}



convex.H.obj=function(A,bg){## Huellenoperator Phi \circ Psi speziell fuer einen Geomtrie-Kontext (also mit G= Punkte in R^2 und M Halbräume
  ## benötigt Package geometry
  A <<- A
  if(sum(A)<=1){return(A)}
  if(sum(A)==2){return(H.obj(A,bg$context))}
  P <- try(convhulln(bg$X[which(A==1),]),silent=TRUE)
  if(class(P)[1]=="try-error"){return(H.obj(A,bg$context))}
  ans <- inhulln(P,as.matrix(bg$X))
  return(ans*1)}

H.attr=function(A,context){PSI(PHI(A,context),context)} ## Berechnet zu Merkmalsmenge A (gegeben durch A[i]=1 iff Merkmal i ist in Menge A, 0 sonst) deren Hülle PSI(PHI(A))
H.obj=function(A,context){PHI(PSI(A,context),context)} ## Berechnet zu Gegenstandsmenge A (gegeben durch A[i]=1 iff Gegenstand i ist in Menge A, 0 sonst) deren Hülle PHI(PSI(A))


MILP.from.generic.base.from.convex.incidence=function(bg,binary=TRUE,max.card=dim(bg$context)[2],DIST,maxdist,HOP=convex.H.obj2){

  ## erzeugt MILP Model ueber die Implementation von Contsraints, die das Repsektieren der generischen (Gegenstands-) Implikationsbasis sicherstellen

  ##hieß früher LP.from.convex.incidence.simple

  # bg:Liste mit Objekt context, das Kontext und Objekt X, das ursprüngliche Datenmatrix X enthält

  n=dim(bg$context)[1]
  N=choose(n,3)
  T=1
  tt=1
  D=rep(0,N)
  ii=rep(1/2,floor(N*n/2.5))
  jj=  rep(1/2,floor(N*n/2.5))
  vvv=rep(1/2,floor(N*n/2.5))
  tt=1
  indexs=array(0,c(N,3))
  # A <-simple_triplet_matrix(1,1,0,nrow=N,ncol=n)# sparseMatrix((0),nr=N,nc=n)# Matrix(nrow=N,ncol=n,sparse=TRUE)#simple_triplet_zero_matrix(nrow=N,ncol=n)#simple_sparse_array(i=0,v=0,dim=c(N,n))#,sparse=TRUE)#array(as.logical(0),c(N,n))
  #sparseMatrix(i=2*n^4,j=n,dims=c(2*n^4,n))#sparseMatrix((0),nr=2*n^4,nc=n)
  rhs=rep(0,N)
  sense=rep(">=",N)
  for(k in (1:(n-2))){
    print(k)
    for( l in ((k+1):(n-1))){
      if(DIST[k,l]<=maxdist){
        for(m in ((l+1):n)){
          if(DIST[k,l]<maxdist & DIST[m,l]<=maxdist){
            temp=rep(0,n)
            temp[k]=1
            temp[l]=1
            temp[m]=1
            D[T]=max(DIST[k,l],DIST[k,m],DIST[l,m])
            H=HOP(temp,bg)#H.attr(temp,bg)HULL(k,l,m,bg)#H.attr(temp,bg)
            H[c(k,l,m)]=0
            i=which(H==1)
            j=length(i)
            if(j!=0){


              ii[(tt:(tt+j-1))] <- T
              jj[(tt:(tt+j-1))] <- i
              vvv[(tt:(tt+j-1))]<- 1/j
              tt=tt+j

              ii[(tt:(tt+2))]=T
              jj[(tt:(tt+2))]=c(k,l,m)
              vvv[(tt:(tt+2))]=-1
              tt=tt+3


              #A[T,i]=1/j
              sense[T]=">="
              #A[T,c(k,l,m)]=-1
              rhs[T]=-2

              indexs[T,]=c(k,l,m)
              T=T+1
            }

          }}} }}
  T=T-1
  tt=tt-3
  ii=ii[(1:tt)]
  jj=jj[(1:tt)]
  vvv=vvv[(1:tt)]
  gc()
  model=list(A=simple_triplet_matrix(i=ii[(1:tt)],j=jj[(1:tt)],v=vvv[(1:tt)])  ,obj=NULL,modelsense="max",rhs=rhs[(1:T)],sense= sense[(1:T)],vtypes=rep('B',n) ,D=D,indexs=indexs)

  return(model=model)}




est_cond_prob_k_antichain <- function(poset,k){
	if(k==1){return(1)}
	comparability <- pmax(poset,t(poset))
	n_elements <- nrow(poset)
	index_set <- seq_len(n_elements)
	set <- NULL
	size <- 1
	for(i in seq_len(k)){
		for(l in set){index_set <- setdiff(index_set,which(comparability[,l]==1))}
		print("index_set")
		print(index_set)
		if(length(index_set)==0){return(0)}

		if(length(index_set)==1){set <- c(set,index_set[1])}
		if(length(index_set)>1){set <- c(set,sample(index_set,size=1))}
		print(set)
		size <- size * length(index_set)

	}
	#if(k==3){SETS <<- unique(rbind(SETS,set))}
	return(size/factorial(k))}





# Data Example d-dimensional interordinal scaling



if(FALSE){



setwd("C:/Paper_fallibilistic_regularization/Allbus2018")
library(foreign)
a=read.spss("ZA5270_v2-0-0.sav")
dat=data.frame(a$sex,a$age,a$iscd11, a$J007_1,  a$incc, a$dw01, a$id02, a$J006,   a$dm02c,a$pa02,a$pk01,a$pk02,a$pk03,a$pk04,a$pk05,a$pk06,a$pk07,a$pk08,a$pk09, a$pv01,a$wghtpew)

#I=which( dat[,2]=="NICHT GENERIERBAR" | dat[,3]=="NICHT GENERIERBAR" | dat[,4] %in% c("KEIN ISSP", "KEIN ISSP RELIGION","KEINE ANGABE", "KANN NICHT SAGEN") | dat[,5]=="NICHT GENERIERBAR"|dat[,6] %in% c("DATENFEHLER","KEINE ANGABE")|dat[,7]%in%c("KEINER DER SCHICHTEN","KEINE ANGABE","WEISS NICHT","VERWEIGERT")|dat[,8]%in%c("KEIN ISSP","KEIN ISSP RELIGION","KEINE ANGABE","KANN NICHT SAGEN") |dat[,10]=="KEINE ANGABE"|dat[,20]=="NEUE BUNDESLAENDER"|dat[,9]=="KEINE ANGABE"|dat[,21]=="NEUE BUNDESLAENDER")

I=which( dat[,2]=="NICHT GENERIERBAR" | dat[,3]=="NICHT GENERIERBAR" |  dat[,5]=="NICHT GENERIERBAR"|dat[,6] %in% c("DATENFEHLER","KEINE ANGABE")|dat[,7]%in%c("KEINER DER SCHICHTEN","KEINE ANGABE","WEISS NICHT","VERWEIGERT")|dat[,10]=="KEINE ANGABE"|dat[,20]=="NEUE BUNDESLAENDER"|dat[,9]=="KEINE ANGABE"|dat[,21]=="NEUE BUNDESLAENDER")




 dat=dat[-I,]

 dat[,2]=factor(dat[,2],ordered=TRUE)
 dat[,3]=factor(dat[,3],ordered=TRUE)
 dat[,4]=factor(dat[,4],ordered=TRUE)
 dat[,5]=factor(dat[,5],ordered=TRUE)
 dat[,6]=factor(dat[,6],ordered=FALSE)
 dat[,7]=factor(dat[,7],ordered=TRUE)
 dat[,8]=factor(dat[,8],ordered=TRUE)
 dat[,9]=factor(dat[,9],ordered=FALSE)

dim(dat)
I=c(1,2,3,5,6,10)
dat <- dat[,I]
 context<- oofos:::get_auto_conceptual_scaling(dat[,-c(1,2,5,6)])

y <- dat[,6] %in% c("SEHR STARK", "STARK")
table(y)
objective <- oofos:::compute_objective(data.frame(y=y),"y","TRUE")
table(objective)


model <- oofos:::optimize_on_context_extents(context,seq_len(nrow(context)),objective)
result <- gurobi::gurobi(model)

#with weighted representation:

wdat <- get_weighted_representation(dat[,-c(1,2,5,6)])
wcontext <- oofos:::get_auto_conceptual_scaling(wdat$x_wieghted)
#context <- oofos:::get_auto_conceptual_scaling(dat[,-6])#Z)#[,-1]+rnorm(length(Z[,-1]),sd=0.000001))#dat[,-6])
vc_model <- oofos:::compute_extent_vc_dimension(context)
vc_dimension <- gurobi::gurobi(vc_model)
#VC dimension \in \{9,10\}

n_rep <- 2
sizes <- array(0,c(6,n_rep))
for(k in (1:6)){
  for(l in (1:n_rep)){
    sizes[k,l] <- estimate_size_mingen_k(context,k)
  }
  print(k)
  print(sizes[k,])
}

plot(rowMeans(sizes))


N<- 912
size <- sum(rowMeans(sizes))

eps <- result$objval

size*exp(-eps*N)



dim(context)
dim(context2)

dat <- dat[,-c(1,2,5,6)]

Z <- array(0,c(nrow(dat),2))
for(k in (1:2)){Z[,k] <- as.numeric(dat[,k])}
context2 <- oofos:::get_auto_conceptual_scaling(Z+rnorm(length(Z),sd=0.00000001))
model2 <- oofos:::optimize_on_context_extents(context2,seq_len(nrow(context2)),objective,binary_variables="all")
result2 <- gurobi::gurobi(model2)
}
