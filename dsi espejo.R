
#zac33tar los distritos donde c8 de las 3 zaclas 

library(foreign)
library(tidyzac33se)
#zac33
# leer distritación
dd <- "/Users/danielsll/mxDistritos/mapasComparados/loc/"
d <- read.csv(paste(dd, "zacLoc.csv", sep = ""), stringsAsFactors = FALSE)
head(d,1)

kk <- "/Volumes/DANIEL/comparaciones/"
k<-read.csv(paste(kk,"zac.csv",sep=""),stringsAsFactors = F)
head(k,1)    
esc_loc<-merge(d,k,by="seccion")

gg <- "/Volumes/DANIEL/tesis_DSI/catalogo_secciones_ine/secciones/"
g <- read.csv(paste(gg,"zac_sec.csv",sep=""),stringsAsFactors=F)
head(g,1)
g$urban <- as.numeric(g$TIPO=="URBANO(A)")
g$rural <- as.numeric(g$TIPO=="RURAL")
g$mixto <- as.numeric(g$TIPO=="MIXTO(A)")
g$rural<-g$rural+g$mixto
# leer archivo dbf con población
e <- read.dbf("/Volumes/DANIEL/Tesis/datos_geoelec/datos_geolectorales-32/f6e794a75c5d51de081dbefa224304f9_geolectorales.dbf")
head(e) # tiene 593 secciones    589 observaciones

colnames(esc_loc)[1] <- ("SECCION")
esc_loc_pob <- merge(esc_loc, e, by="SECCION")
esc_loc_pob <- merge(esc_loc_pob, g, by="SECCION")
names(bastion_zac)[6]<-"SECCION"
esc_loc_pob <- merge(esc_loc_pob,bastion_zac,by="SECCION")
#seleccionar variables a considerar y zac33biar nombres
head(esc_loc_pob,1)
db <- data.frame(esc_loc_pob$SECCION,esc_loc_pob$munn.x,esc_loc_pob$edon.x,
                 esc_loc_pob$disloc2016, esc_loc_pob$disloc2013, esc_loc_pob$POBTOT, esc_loc_pob$escenario1,
                 esc_loc_pob$escenario3, esc_loc_pob$urban, esc_loc_pob$rural, esc_loc_pob$mixto,
                 esc_loc_pob$dummy_pan,esc_loc_pob$dummy_pri,esc_loc_pob$dummy_prd )

colnames(db)<- c("seccion","munn","edon","disloc2016","disloc2013","pobtot","esc1","esc3","urbana","rural","mixta","pan","pri","prd")
# seleccionar variables a considerar y zac33biar nombres
#g <- data.frame(ff$SECCION, ff$munn, ff$edo, ff$edon, ff$disloc2013, ff$disloc2016,
#                ff$POBTOT)
#colnames(g) <- c("SECCION", "munn", "edo", "edon", "disloc2013", "disloc2016", "P_Total"

#merge(g,k,by="SECCION")
#head(g,1)
#colnames(g) <- c("SECCION","munn","edon","edo","disloc2016","disloc2013","esc1","esc2","esc3")
#p<-merge(g,e,by="SECCION")
#head(p,1)

#q <- data.frame(p$SECCION,p$munn,p$edon,p$edo,p$disloc2016,p$disloc2013,p$esc1,p$esc2,p$esc3,p$POBTOT)
#colnames(q) <- c("seccion","munn","edon","edo","disloc2016","disloc2013","esc1","esc2","esc3","P_Total")
E1 <- db$esc1 
#E3 <- db$esc3jge
#E3 <- db$esc3cg
E3 <-db$esc3
DN <- db$disloc2016
SQ <- db$disloc2013

db$pob_dist <- 0
db$pob_edo <- sum(db$pobtot)
db$RRI <- 0
N <- max(SQ,na.rm = T)
for (i in 1:N){
  sel <- which(SQ==i)
  pob_dist <- sum(db$pobtot[sel])
  db$pob_dist[sel]<-pob_dist
  RRI <- ((1/db$pob_dist)/(N/db$pob_edo))
  db$RRI_2013<-RRI
}

db$pob_dist <- 0
db$pob_edo <- sum(db$pobtot)
db$RRI <- 0
N <- max(DN,na.rm = T)
for (i in 1:N){
  sel <- which(DN==i)
  pob_dist <- sum(db$pobtot[sel])
  db$pob_dist[sel]<-pob_dist
  RRI <- ((1/db$pob_dist)/(N/db$pob_edo))
  db$RRI_2016<-RRI
}

#SQ vs E1
N<-max(E1, na.rm=T)
db$SQ<-NA
db$dsi_pob<-0

for (i in 1:N){
  sel.n <- which(E1==i)
  pob_n <- sum(db$pobtot[sel.n])
  tmp <- table(SQ[sel.n]) #Identificando las secciones del distrito viejo con relación al nuevo
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1]) #Identifica al distrito padre
  db$SQ[sel.n] <- target #Escribe en el vector al distrito padre
  sel.f <- which(SQ==target)
  pob_f <- sum(db$pobtot[sel.f])
  sel.c <- intersect(sel.n, sel.f)
  pob_c <- sum(db$pobtot[sel.c])
  db$dsi_pob[sel.n]<-round(((pob_c)/(pob_f+pob_n-pob_c)),3) 
}
#SQ vs E1 espejo
N<-max(SQ, na.rm=T)
db$E1<-NA
db$dsi_pob_e<-0

for (i in 1:N){
  sel.n <- which(SQ==i)
  pob_n <- sum(db$pobtot[sel.n])
  tmp <- table(E1[sel.n]) #Identificando las secciones del distrito viejo con relación al nuevo
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1]) #Identifica al distrito padre
  db$E1[sel.n] <- target #Escribe en el vector al distrito padre
  sel.f <- which(E1==target)
  pob_f <- sum(db$pobtot[sel.f])
  sel.c <- intersect(sel.n, sel.f)
  pob_c <- sum(db$pobtot[sel.c])
  db$dsi_pob_e[sel.n]<-round(((pob_c)/(pob_f+pob_n-pob_c)),3) 
}
#dsi_pob <- db[duplicated(E1)==FALSE,]
#dsi_pob<-db[duplicated(SQ)==FALSE,]
#dsi <- dsi_pob[,c("edon","disloc2016","disloc2013","esc1","esc3","dsi_pob","pobtot")]
#head(dsi)

N <- max(db$disloc2013,na.rm = T)
db$prop_urbana <- 0
db$prop_rural<-0
db$prop_mixta <- 0
for (i in 1:N){
  sel <- which(db$disloc2013==i)
  db$prop_urbana[sel] <- round(sum(db$urbana[sel])/(sum(db$urbana[sel])+sum(db$rural[sel])+sum(db$mixta[sel])),3)
}
for (i in 1:N){
  sel <- which(db$disloc2013==i)
  db$prop_rural[sel] <- round(sum(db$rural[sel])/(sum(db$urbana[sel])+sum(db$rural[sel])+sum(db$mixta[sel])),3)
}
for (i in 1:N){
  sel <- which(db$disloc2013==i)
  db$prop_mixta[sel] <- round(sum(db$mixta[sel])/(sum(db$urbana[sel])+sum(db$rural[sel])+sum(db$mixta[sel])),3)
}
db$prop_rural<-db$prop_rural+db$prop_mixta

db$prop_bastion_pan <- 0
db$prop_bastion_pri <- 0
db$prop_bastion_prd <- 0

for(i in 1:N){
  sel<-which(db$disloc2013==i)
  db$prop_bastion_pan[sel]<-round(sum(db$pan[sel])/(sum(db$pan[sel])+sum(db$pri[sel])+sum(db$prd[sel])),3)
}
for(i in 1:N){
  sel<-which(db$disloc2013==i)
  db$prop_bastion_pri[sel]<-round(sum(db$pri[sel])/(sum(db$pan[sel])+sum(db$pri[sel])+sum(db$prd[sel])),3)
}
for(i in 1:N){
  sel<-which(db$disloc2013==i)
  db$prop_bastion_prd[sel]<-round(sum(db$prd[sel])/(sum(db$pan[sel])+sum(db$pri[sel])+sum(db$prd[sel])),3)
}

#sq e1 
N<-max(E1, na.rm=T)
db$SQ_<-NA
db$dsi_sec<-0
for (i in 1:N){
  sel.n<-which(E1==i)
  tmp<-table(SQ[sel.n])
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1])
  db$SQ_[sel.n] <- target
  sel.f <- which(SQ==target) # secciones in father district
  sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
  db$dsi_sec[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}
dsi <- db[duplicated(E1)==FALSE,]
dsi<-db[duplicated(SQ)==FALSE,]

N<-max(SQ, na.rm=T)
db$E1_<-NA
db$dsi_sec_e<-0
for (i in 1:N){
  sel.n<-which(SQ==i)
  tmp<-table(E1[sel.n])
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1])
  db$E1_[sel.n] <- target
  sel.f <- which(E1==target) # secciones in father district
  sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
  db$dsi_sec_e[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}
dsi <- db[duplicated(E1)==FALSE,]
dsi<-db[duplicated(SQ)==FALSE,]


write.csv(dsi, file = "/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E1_zac.csv", row.names = FALSE)
write.csv(db, file="/Volumes/DANIEL/tesis_DSI/archivos/espejo/db_SQ_E1_zac.csv", row.names = F)

#SQ vs E3
N<-max(E3, na.rm=T)
db$SQ<-NA
db$dsi_pob<-0

for (i in 1:N){
  sel.n <- which(E3==i)
  pob_n <- sum(db$pobtot[sel.n])
  tmp <- table(SQ[sel.n]) #Identificando las secciones del distrito viejo con relación al nuevo
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1]) #Identifica al distrito padre
  db$SQ[sel.n] <- target #Escribe en el vector al distrito padre
  sel.f <- which(SQ==target)
  pob_f <- sum(db$pobtot[sel.f])
  sel.c <- intersect(sel.n, sel.f)
  pob_c <- sum(db$pobtot[sel.c])
  db$dsi_pob[sel.n]<-round(((pob_c)/(pob_f+pob_n-pob_c)),3) 
}

N<-max(E3, na.rm=T)
db$SQ_<-NA
db$dsi_sec<-0
for (i in 1:N){
  sel.n<-which(E3==i)
  tmp<-table(SQ[sel.n])
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1])
  db$SQ_[sel.n] <- target
  sel.f <- which(SQ==target) # secciones in father district
  sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
  db$dsi_sec[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}
#SQ vs E3
N<-max(SQ, na.rm=T)
db$E3<-NA
db$dsi_pob_e<-0

for (i in 1:N){
  sel.n <- which(SQ==i)
  pob_n <- sum(db$pobtot[sel.n])
  tmp <- table(E3[sel.n]) #Identificando las secciones del distrito viejo con relación al nuevo
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1]) #Identifica al distrito padre
  db$E3[sel.n] <- target #Escribe en el vector al distrito padre
  sel.f <- which(E3==target)
  pob_f <- sum(db$pobtot[sel.f])
  sel.c <- intersect(sel.n, sel.f)
  pob_c <- sum(db$pobtot[sel.c])
  db$dsi_pob_e[sel.n]<-round(((pob_c)/(pob_f+pob_n-pob_c)),3) 
}

N<-max(SQ, na.rm=T)
db$E3_<-NA
db$dsi_sec_e<-0
for (i in 1:N){
  sel.n<-which(SQ==i)
  tmp<-table(E3[sel.n])
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1])
  db$E3_[sel.n] <- target
  sel.f <- which(E3==target) # secciones in father district
  sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
  db$dsi_sec_e[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}
dsi <- db[duplicated(E3)==FALSE,]
dsi<-db[duplicated(SQ)==FALSE,]

write.csv(db, file="/Volumes/DANIEL/tesis_DSI/archivos/espejo/db_SQ_E3_zac.csv", row.names = F)
write.csv(dsi, file = "/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_zac.csv", row.names = FALSE)


#E1 vs E3
N<-max(E3, na.rm=T)
db$E1<-NA
db$dsi_pob<-0

for (i in 1:N){
  sel.n <- which(E3==i)
  pob_n <- sum(db$pobtot[sel.n])
  tmp <- table(E1[sel.n]) #Identificando las secciones del distrito viejo con relación al nuevo
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1]) #Identifica al distrito padre
  db$E1[sel.n] <- target #Escribe en el vector al distrito padre
  sel.f <- which(E1==target)
  pob_f <- sum(db$pobtot[sel.f])
  sel.c <- intersect(sel.n, sel.f)
  pob_c <- sum(db$pobtot[sel.c])
  db$dsi_pob[sel.n]<-round(((pob_c)/(pob_f+pob_n-pob_c)),3)
}

N<-max(E3, na.rm=T)
db$E1_<-NA 
db$dsi_sec<-0
for (i in 1:N){
  sel.n<-which(E3==i)
  tmp<-table(E1[sel.n])
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1])
  db$E1_[sel.n] <- target
  sel.f <- which(E1==target) # secciones in father district
  sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
  db$dsi_sec[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}

#E1 vs E3
N<-max(E1, na.rm=T)
db$E3<-NA
db$dsi_pob_e<-0

for (i in 1:N){
  sel.n <- which(E1==i)
  pob_n <- sum(db$pobtot[sel.n])
  tmp <- table(E3[sel.n]) #Identificando las secciones del distrito viejo con relación al nuevo
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1]) #Identifica al distrito padre
  db$E3[sel.n] <- target #Escribe en el vector al distrito padre
  sel.f <- which(E3==target)
  pob_f <- sum(db$pobtot[sel.f])
  sel.c <- intersect(sel.n, sel.f)
  pob_c <- sum(db$pobtot[sel.c])
  db$dsi_pob_e[sel.n]<-round(((pob_c)/(pob_f+pob_n-pob_c)),3)
}

N<-max(E1, na.rm=T)
db$E3_<-NA 
db$dsi_sec_e<-0
for (i in 1:N){
  sel.n<-which(E1==i)
  tmp<-table(E3[sel.n])
  target<-as.numeric(names(tmp)[tmp==max(tmp)][1])
  db$E3_[sel.n] <- target
  sel.f <- which(E3==target) # secciones in father district
  sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
  db$dsi_sec_e[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}
dsi <- db[duplicated(E3)==FALSE,]
dsi<-db[duplicated(E1)==FALSE,]

write.csv(db, file="/Volumes/DANIEL/tesis_DSI/archivos/espejo/db_E1_E3_zac.csv", row.names = F)
write.csv(dsi, file = "/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_E1_E3_zac.csv", row.names = FALSE)


#zac33tar los distritos donde c8 de las 3 zaclas 

