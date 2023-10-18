context_is_meet_distributive <- function(context){
  context <- unique(context)
  n_objects <- nrow(context)
  for(k in seq_len(n_objects-1)){
    for(l in ((k+1):n_objects)){

     indexs <- which(context[k,]!=context[l,])

     indexs2 <- which(context[k,]==0 & context[l,]==0)
     for( m in indexs2){
      indexs3 <- which(context[,m]==1)
      if(all(colSums(matrix(context[indexs3,indexs],nrow=length(indexs3)))<length(indexs3))){context[l,m]=1;return(list(result=FALSE,new_context=context))}



      #context <<- context;print(c(k,l));print(m);print(indexs3);indexs <<- indexs;indexs2 <<- indexs2;indexs3 <<-indexs3;return(FALSE)}
    }

}


}
return(TRUE)}

estimate_concept_lattice_size=function(context,nrep){    #### Schätzt Anzahl von formalen Begriffen über Monte-Carlo-Simulation
  m=dim(context)[2]
  a=rep(0,nrep)
  for(k in (1:nrep)){
   temp=runif(m)>=0.5
   H=oofos:::operator_closure_attr_input(temp,context)
   if(all(H==temp)){a[k]=1}
  }
return(mean(a)*2^m)}


is_freely_addable <- function(set,element,context){

if(set[element]==1){return(FALSE)}
if(all(set==0)){return(TRUE)}
if((ddandrda:::operator_closure_obj_input(set,context))[element]==1){return(FALSE)}
for(k in which(set==1)){

new_set <- set
new_set[k] <-0
new_set[element] <- 1
if((ddandrda:::operator_closure_obj_input(new_set,context))[k]==1){return(FALSE)}
}

return(TRUE)
}


est_cond_prob_k <- function(context,k){

set <- rep(0,nrow(context))
set[sample(seq_len(nrow(context)),size=1)] <- 1

size <- as.numeric(nrow(context))
while(TRUE){
if(sum(set)==k){return(size)}
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
 a=est_cond_prob_k(context,7)



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
	return(size)}
	return(length(index_set)/n_elements)




# Data Example d-dimensional interordinal scaling







setwd("C:/Paper_fallibilistic_regularization/Allbus2018")
library(foreign)
a=read.spss("ZA5270_v2-0-0.sav")
dat=data.frame(a$sex,a$age,a$iscd11, a$J007_1,  a$incc, a$dw01, a$id02, a$J006,   a$dm02c,a$pa02,a$pk01,a$pk02,a$pk03,a$pk04,a$pk05,a$pk06,a$pk07,a$pk08,a$pk09, a$pv01,a$wghtpew)

I=which( dat[,2]=="NICHT GENERIERBAR" | dat[,3]=="NICHT GENERIERBAR" | dat[,4] %in% c("KEIN ISSP", "KEIN ISSP RELIGION","KEINE ANGABE", "KANN NICHT SAGEN") | dat[,5]=="NICHT GENERIERBAR"|dat[,6] %in% c("DATENFEHLER","KEINE ANGABE")|dat[,7]%in%c("KEINER DER SCHICHTEN","KEINE ANGABE","WEISS NICHT","VERWEIGERT")|dat[,8]%in%c("KEIN ISSP","KEIN ISSP RELIGION","KEINE ANGABE","KANN NICHT SAGEN") |dat[,10]=="KEINE ANGABE"|dat[,20]=="NEUE BUNDESLAENDER"|dat[,9]=="KEINE ANGABE"|dat[,21]=="NEUE BUNDESLAENDER")

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

y <- dat[,6] %in% c("SEHR STARK", "STARK")
table(y)
objective <- oofos:::compute_objective(data.frame(y=y),"y","TRUE")
table(objective)
context <- oofos:::get_auto_conceptual_scaling(Z[,-1]+rnorm(length(Z[,-1]),sd=0.000001))#dat[,-6])
vc_model <- oofos:::compute_extent_vc_dimension(context)
vc_dimension <- gurobi::gurobi(vc_model)
#VC dimension \in \{9,10\}

model <- oofos:::optimize_on_context_extents(context,seq_len(nrow(context)),objective,binary_variables="all")
result <- gurobi::gurobi(model)


