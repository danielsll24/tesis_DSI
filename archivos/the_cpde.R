library(tidyverse)
library(readr)
library(foreign)
temporal <- read_csv("/Volumes/DANIEL/tesis_DSI/catalogo_secciones_ine/secciones/ver_sec.csv")

dd<-"/Users/danielsll/mxDistritos/mapasComparados/loc/"
d <- read.csv(paste(dd,"verLoc.csv",sep=""), stringsAsFactors = FALSE)
head(d)

 #mapa de averscalientes
#seleccionar las verumnas ver necesito
p <- data.frame(d$seccion,d$disloc2016,d$disloc2007,d$edon)
names(p) <- c("SECCION","DIS_NUEVA","DIS_ANT","EDON")
q <- right_join(p,temporal,by="SECCION")
q$urbano <- as.numeric(q$TIPO=="URBANO(A)")
q$rural <- as.numeric(q$TIPO=="RURAL")
q$mixto <- as.numeric(q$TIPO=="MIXTO(A)")

t <- data.frame(q$EDON,q$SECCION,q$DIS_ANT,q$DIS_NUEVA,q$urbano,q$rural,q$mixto)
names(t) <- c("EDON","SECCION","DIS_ANT","DIS_NUEVA","URBANO","RURAL","MIXTO")





N <- max(t$DIS_ANT,na.rm = T)
t$prop_urban <- 0
for (i in 1:N){
  sel <- which(t$DIS_ANT==i)
  t$prop_urban[sel] <- round(sum(t$URBANO[sel])/(sum(t$URBANO[sel])+sum(t$RURAL[sel])+sum(t$MIXTO[sel])),3)
}

N <- max(t$DIS_ANT)
t$prop_rural <- 0
for (i in 1:N){
  sel <- which(t$DIS_ANT==i)
  t$prop_rural[sel] <- round(sum(t$RURAL[sel])/(sum(t$URBANO[sel])+sum(t$RURAL[sel])+sum(t$MIXTO[sel])),3)
}

N <- max(t$DIS_ANT)
t$prop_mixto <- 0
for (i in 1:N){
  sel <- which(t$DIS_ANT==i)
  t$prop_mixto[sel] <- round(sum(t$MIXTO[sel])/(sum(t$URBANO[sel])+sum(t$RURAL[sel])+sum(t$MIXTO[sel])),3)
}


N <- max(t$DIS_NUEVA)
t$prop_urban_n <- 0
for (i in 1:N){
  sel <- which(t$DIS_NUEVA==i)
  t$prop_urban_n[sel] <- round(sum(t$URBANO[sel])/(sum(t$URBANO[sel])+sum(t$RURAL[sel])+sum(t$MIXTO[sel])),3)
}

N <- max(t$DIS_NUEVA)
t$prop_rural_n <- 0
for (i in 1:N){
  sel <- which(t$DIS_NUEVA==i)
  t$prop_rural_n[sel] <- round(sum(t$RURAL[sel])/(sum(t$URBANO[sel])+sum(t$RURAL[sel])+sum(t$MIXTO[sel])),3)
}

N <- max(t$DIS_NUEVA)
t$prop_mixto_n <- 0
for (i in 1:N){
  sel <- which(t$DIS_NUEVA==i)
  t$prop_mixto_n[sel] <- round(sum(t$MIXTO[sel])/(sum(t$URBANO[sel])+sum(t$RURAL[sel])+sum(t$MIXTO[sel])),3)
}

kk <- "/Volumes/DANIEL/comparaciones/"
k<-read.csv(paste(kk,"ver.csv",sep=""),stringsAsFactors = F) #van los escenarios

e <- read.dbf("/Volumes/DANIEL/Tesis/datos_geoelec/datos_geolectorales-30/f15d337c70078947cfe1b5d6f0ed3f13_geolectorales.dbf")
head(e) # va la población

ee<-data.frame(e$SECCION,e$POBTOT)
names(ee) <- c("SECCION","POB_TOT")
w <- right_join(ee,t,by="SECCION")

names(k)
names(k)[3]<-c("SECCION")
u<-data.frame(k$SECCION,k$escenario1,k$escenario2,k$escenario3)
names(u) <- c("SECCION","ESC1","ESC2","ESC3")

tt <- right_join(u,w,by="SECCION")


write.csv(tt,"/Volumes/DANIEL/tesis_DSI/archivos/ver.csv")


#calcular DSI secciones SQ-E1, SQ-E3, E1-E3
SQ<-tt$DIS_ANT#era father
E1<-tt$ESC1 #era ver 
E3<-tt$ESC3
#E3jge<-f$escenario3_jge
#E3trib<-f$escenario3_tribunal
#SQ-E1
N<-max(E1, na.rm=T)
tt$SQ<-NA
tt$dsiSQ_E1_sec<-0
for (i in 1:N){
  sel.n<-which(E1==2)
  tmp<-table(SQ[sel.n])
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1])
  tt$SQ[sel.n] <- target
  sel.f <- which(SQ==target) # secciones in father district
  sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
  tt$dsiSQ_E1_sec[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}
dsi <- tt[duplicated(E1)==FALSE,]
dsi<-tt[duplicated(SQ)==FALSE,]
#dsi <- dsi[,c("edon.x","disloc2007","escenario1","dsiSQ_E1")]
head(dsi)

N<-max(E1, na.rm=T)
tt$SQ_<-NA
tt$dsiSQ_E1_pob<-0

for (i in 1:N){
  sel.n <- which(E1==i)
  pob_n <- sum(tt$POB_TOT[sel.n])
  tmp <- table(SQ[sel.n]) #Identificando las secciones del distrito viejo con relación al nuevo
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1]) #Identifica al distrito padre
  tt$SQ_[sel.n] <- target #Escribe en el vector al distrito padre
  sel.f <- which(SQ==target)
  pob_f <- sum(tt$POB_TOT[sel.f])
  sel.c <- intersect(sel.n, sel.f)
  pob_c <- sum(tt$POB_TOT[sel.c])
  tt$dsiSQ_E1_pob[sel.n]<-round(((pob_c)/(pob_f+pob_n-pob_c)),3) 
  }

dsi <- tt[duplicated(E1)==FALSE,]
dsi<-tt[duplicated(SQ)==FALSE,]
#dsi <- dsi[,c("edon.x","disloc2007","escenario1","dsiSQ_E1")]
head(dsi)

write.csv(dsi,"/Volumes/DANIEL/tesis_DSI/archivos/dsi_ver_SQ_E1.csv")


