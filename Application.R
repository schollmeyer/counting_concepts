








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
I=c(1,2,3,5,6)
dat <- dat[,I]
