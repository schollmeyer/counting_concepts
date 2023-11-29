library(foreign)
library(gurobi)
# read data

dat=read.spss("ZA5240_v2-1-0.sav",to.data.frame=TRUE)       ## Daten einlesen
#Vorher: Pfad entsprechend setzen

dat=dat[which(dat$V5=="SPLIT B: F75B"),]## Wähle Split F075 B

#   FRAGEBOGENSPLIT F075
#   Ergänzender Kurzkommentar zur Variablenbeschreibung:
#   Fragebogensplit Gesundheitszustand
#   Die Frage zum allgemeinen Gesundheitszustand (F075A/F075B) wurde im Erhebungsjahr 2014 in einem
#   Splitverfahren erhoben, um die Auswirkung verschiedener Antwortskalen auf das Antwortverhalten zu testen.
#   Die Befragten wurden sowohl in Split A als auch in Split B gefragt, wie sie ihren Gesundheitszustand im Allgemeinen
#   beschreiben würden. Die eine Hälfte der Befragten (Split A) erhielt die Antwortkategorien „Sehr gut“, „Gut“,
#   „Zufriedenstellend“, „Weniger gut" und „Schlecht". Bei der anderen Hälfte der Befragten (Split B) wurde? als zusätzliche
#   Antwortkategorie „Ausgezeichnet“ angeboten.



# Variablenreport zu V102 (Bildung)

# V102 BEFR.: ISCED 2011
# Ergänzender Kurzkommentar zur Variablenbeschreibung:
#   International Standard Classification of Education (ISCED) 2011
# 1 Level 1 - Primary education
# 2 Level 2 - Lower secondary education
# 3 Level 3 - Upper secondary education
# 4 Level 4 - Post secondary non-tertiary education
# 5 Level 5 - Short-cycle tertiary education
# 6 Level 6 - Bachelor's or equivalent level
# 7 Level 7 - Master's or equivalent level
# 8 Level 8 - Doctoral or equivalent level
# 94 Noch Schüler
# 99 Nicht klassifizierbar
# Ableitung der Daten:
#   Diese Variable wurde mit Hilfe der Angaben zum allgemeinbildenden Schulabschluss (V86) und dem
# berufsqualifizierenden Ausbildungsabschluss (V87-V100) gebildet.



# vgl Codebook
#  F075B
#  <Falls Teilnahme an Split B (Code 2 in V5).>
#  (Int.: Liste 75B vorlegen!)
#  Ich möchte Ihnen nun einige Fragen zu Ihrer Gesundheit stellen. Wie würden Sie Ihren Gesundheitszustand im
#  Allgemeinen beschreiben?
#  (Int.: Bitte achten Sie darauf, dass die richtige Liste, 75B, vorliegt!)
#  0 Keine Teilnahme an Split B (Code 1 in V5)
#  1 Ausgezeichnet
#  2 Sehr gut
#  3 Gut
#  4 Zufriedenstellend
#  5 Weniger gut
#  6 Schlecht
#  9 Keine Angabe

##############
#############
###########

Z=cbind(dat$V81,      dat$V7  ,  dat$V419 ,                            dat$V102 ,                                            7-as.numeric(dat$V226),    dat $V870)
###     Geschlecht    OST     ,  Einkommen (offen + Listen Abfrage)    Bildung gem. ISCED 2011           Beruf               Gesundheit                  Samplegewicht
#offen:  dat$V417 ,                                                      # dat$V107 ,


education.names=c("Primary education","Lower secondary education","Upper secondary education","Post secondary non-tertiary education","Short-cycle tertiary education","Bachelor's or equivalent level", "Master's or equivalent level","Doctoral or equivalent level")






Z=na.omit(Z)
X=Z[,c(3,4,5)]  #betrachte 3 Dimensionen Einkommen, Bildung und Gesundheit


gesundheit.names=c("Schlecht","Weniger gut", "Zufriedenstellend", "Gut", "Sehr gut", "Ausgezeichnet")
gesundheit.names.eng=c("bad","suboptimal", "satisfactory", "good", "very good", "excellent")      # Achtung: eigene Übersetzung, checken!!!

wx=(Z[,1]==1)*Z[,6]     ##Gewichtsvektor für Subpopulation der Männer
wy=(Z[,1]==2)*Z[,6]     ## Gewichtsvektor für Subpopulation der Frauen

v=wx/sum(wx)-wy/sum(wy)    ## Differenzvektor beschreibt Differenz der Anteile der Männer und der Frauen in Oberhalbmenge


XX=oofos:::get_weighted_representation(X,v)    ### gweichtete repräsentation vereinfacht berechnung (Personen mit gleicher Ausprägung werden zu einem Datenpunkt mit entsprechendem Gewicht


        #### Berechnung <= Relation I mit Interpretation x <= y iff x

width <- oofos:::compute_width(I)
# 33

n_rep <- 100
k_max <- 33
estimated_sizes <- array(0,c(n_rep,k_max))

start_time <- Sys.time()
set.seed(1234567)
for( k in (1:k_max)){
  for(l in (1:n_rep)){

    estimated_sizes[l,k] <-  est_cond_prob_k_antichain(I,k)
    print(estimated_sizes[l,k])

  }}

end_time <- Sys.time()
total_time <- Sys.time() - start_time
total_time
z <- as.vector(estimated_sizes)*k_max
n_est <- mean(z)


#### Permutationstest

model$obj=XX$y_weighted            ### ?bergabe der Zielfunktion (Vektor v in gewichteter Darstellung an Liste, die das Lineare     Programm enth?lt


model$modelsense="max"
n_rep <- 20000
ans <- rep(0,n_rep)
start_time <- Sys.time()
set.seed(1234567)
for(k in (1:n_rep)){
  model$obj <- sample(model$obj)
  ans[k] <- gurobi(model,list(outputflag=0))$objval
  print(k)


}

end_time <- Sys.time()
total_time <- Sys.time() - start_time
total_time


plot(ecdf(ans),xlab="",ylab="",xlim=c(0,0.38),main="",lwd=3)
abline(v=0.36,lwd=3)

pnorm(0.36,mean=mean(ans),sd=sd(ans),lower.tail=FALSE)

