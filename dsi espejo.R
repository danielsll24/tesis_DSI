
#cps33tar los distritos donde c8 de las 3 cpslas 

library(foreign)
library(tidycps33se)
#cps33
# leer distritación
dd <- "/Users/danielsll/mxDistritos/mapasComparados/loc/"
d <- read.csv(paste(dd, "oaxLoc.csv", sep = ""), stringsAsFactors = FALSE)
head(d,1)

kk <- "/Volumes/DANIEL/comparaciones/"
k<-read.csv(paste(kk,"oax.csv",sep=""),stringsAsFactors = F)
head(k,1)    
esc_loc<-merge(d,k,by="seccion")

gg <- "/Volumes/DANIEL/tesis_DSI/catalogo_secciones_ine/secciones/"
g <- read.csv(paste(gg,"cps_sec.csv",sep=""),stringsAsFactors=F)
head(g,1)
g$urban <- as.numeric(g$TIPO=="URBANO(A)")
g$rural <- as.numeric(g$TIPO=="RURAL")
g$mixto <- as.numeric(g$TIPO=="MIXTO(A)")
g$rural<-g$rural+g$mixto
# leer archivo dbf con población
e <- read.dbf("/Volumes/DANIEL/Tesis/datos_geoelec/datos_geolectorales-7/63dc7ed1010d3c3b8269faf0ba7491d4_geolectorales.dbf")
head(e) # tiene 593 secciones    589 observaciones

colnames(esc_loc)[1] <- ("SECCION")
esc_loc_pob <- merge(esc_loc, e, by="SECCION")
esc_loc_pob <- merge(esc_loc_pob, g, by="SECCION")
names(bastion_cps)[6]<-"SECCION"
esc_loc_pob <- merge(esc_loc_pob,bastion_cps,by="SECCION")
#seleccionar variables a considerar y cps33biar nombres
head(esc_loc_pob,1)
db <- data.frame(esc_loc_pob$SECCION,esc_loc_pob$munn.x,esc_loc_pob$edon.x,
                 esc_loc_pob$disloc2016, esc_loc_pob$disloc2013, esc_loc_pob$POBTOT, esc_loc_pob$escenario1,
                 esc_loc_pob$escenario3, esc_loc_pob$urban, esc_loc_pob$rural, esc_loc_pob$mixto,
                 esc_loc_pob$dummy_pan,esc_loc_pob$dummy_pri,esc_loc_pob$dummy_prd,esc_loc_pob$NOMBRE.MUNICIPIO )

colnames(db)<- c("seccion","munn","edon","disloc2016","disloc2013","pobtot","esc1","esc3","urbana","rural","mixta","pan","pri","prd","nombre")
# seleccionar variables a considerar y cps33biar nombres
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


write.csv(dsi, file = "/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E1_cps.csv", row.names = FALSE)
write.csv(db, file="/Volumes/DANIEL/tesis_DSI/archivos/espejo/db_SQ_E1_cps.csv", row.names = F)

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

write.csv(db, file="/Volumes/DANIEL/tesis_DSI/archivos/espejo/db_SQ_E3_cps.csv", row.names = F)
write.csv(dsi, file = "/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_cps.csv", row.names = FALSE)


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

write.csv(db, file="/Volumes/DANIEL/tesis_DSI/archivos/espejo/db_E1_E3_cps.csv", row.names = F)
write.csv(dsi, file = "/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_E1_E3_cps.csv", row.names = FALSE)


#cps33tar los distritos donde c8 de las 3 cpslas 
#pegar todo

dsi_SQ_E3_ags <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_ags.csv")
dsi_SQ_E3_bc <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_bc.csv")
dsi_SQ_E3_bcs <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_bcs.csv")
dsi_SQ_E3_cps <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_cps.csv")
dsi_SQ_E3_coa <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_coa.csv")#
dsi_SQ_E3_col <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_col.csv")
dsi_SQ_E3_cps <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_cps.csv")
dsi_SQ_E3_cua <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_cua.csv")
dsi_SQ_E3_df33 <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_df33.csv")#
dsi_SQ_E3_df40 <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_df40.csv")
dsi_SQ_E3_dgo <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_dgo.csv")
dsi_SQ_E3_gua <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_gua.csv")
dsi_SQ_E3_gue <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_gue.csv")#
dsi_SQ_E3_hgo <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_hgo.csv")
dsi_SQ_E3_jal <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_jal.csv")
dsi_SQ_E3_mex <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_mex.csv")
dsi_SQ_E3_mic <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_mic.csv")
dsi_SQ_E3_mor12 <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_mor.csv")
dsi_SQ_E3_mor18 <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_mor18.csv")
dsi_SQ_E3_nay <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_nay.csv")
dsi_SQ_E3_nl <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_nl.csv")
dsi_SQ_E3_cps <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_cps.csv")
dsi_SQ_E3_pue <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_pue.csv")
dsi_SQ_E3_que <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_que.csv")
dsi_SQ_E3_qui <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_qui.csv")
dsi_SQ_E3_san <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_san.csv")#
dsi_SQ_E3_sin <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_sin.csv")
dsi_SQ_E3_son <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_son.csv")
dsi_SQ_E3_tab <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_tab.csv")
dsi_SQ_E3_tam <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_tam.csv")
dsi_SQ_E3_tla15 <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_tla.csv")#
dsi_SQ_E3_tla19 <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_tla19.csv")
dsi_SQ_E3_ver <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_ver.csv")
dsi_SQ_E3_yuc <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_yuc.csv")#
dsi_SQ_E3_cps <- read.csv("/Volumes/DANIEL/tesis_DSI/archivos/espejo/dsi_SQ_E3_cps.csv")#

nombres<-c("seccion","munn","edon","dis_nueva","dis_anterior","pob_total","esc1",
           "esc3","urbana","rural","mixta","pan","pri","prd","pob_dist","pob_edo","RRI","RRI_anterior","RRI_nueva","SQ","dsi_pob",
           "E1","dsi_pob_e","prop_urbana","prop_rural","prop_mixta","prop_bastion_pan","prop_bastion_pri",
           "prop_bastion_prd","SQ_","dsi_sec","E1_","dsi_sec_e","E3","E3_")
dsi_SQ_E3_df40$edon<-33
dsi_SQ_E3_mor18$edon<-34
dsi_SQ_E3_tla19$edon<-35
#repetir esto hasta cps
names(dsi_SQ_E3_ags) <- nombres
names(dsi_SQ_E3_bc) <- nombres
names(dsi_SQ_E3_bcs) <- nombres
names(dsi_SQ_E3_cps) <- nombres
names(dsi_SQ_E3_coa) <- nombres
names(dsi_SQ_E3_col) <- nombres
names(dsi_SQ_E3_cps) <- nombres
names(dsi_SQ_E3_cua) <- nombres
names(dsi_SQ_E3_df33) <- nombres
names(dsi_SQ_E3_df40) <- nombres
names(dsi_SQ_E3_dgo) <- nombres
names(dsi_SQ_E3_gua) <- nombres
names(dsi_SQ_E3_gue) <- nombres
names(dsi_SQ_E3_hgo) <- nombres
names(dsi_SQ_E3_jal) <- nombres
names(dsi_SQ_E3_mex) <- nombres
names(dsi_SQ_E3_mic) <- nombres
names(dsi_SQ_E3_mor12) <- nombres
names(dsi_SQ_E3_mor18) <- nombres
names(dsi_SQ_E3_nay) <- nombres
names(dsi_SQ_E3_nl) <- nombres
names(dsi_SQ_E3_cps) <- nombres
names(dsi_SQ_E3_pue) <- nombres
names(dsi_SQ_E3_que) <- nombres
names(dsi_SQ_E3_qui) <- nombres
names(dsi_SQ_E3_san) <- nombres
names(dsi_SQ_E3_sin) <- nombres
names(dsi_SQ_E3_son) <- nombres
names(dsi_SQ_E3_tab) <- nombres
names(dsi_SQ_E3_tam) <- nombres
names(dsi_SQ_E3_tla15) <- nombres
names(dsi_SQ_E3_tla19) <- nombres
names(dsi_SQ_E3_ver) <- nombres
names(dsi_SQ_E3_yuc) <- nombres
names(dsi_SQ_E3_cps) <- nombres



temp <- union(dsi_SQ_E3_ags, dsi_SQ_E3_bc)
temp <- union(temp, dsi_SQ_E3_bcs)
temp <- union(temp, dsi_SQ_E3_cps)
temp <- union(temp, dsi_SQ_E3_coa)
temp <- union(temp, dsi_SQ_E3_col)
temp <- union(temp, dsi_SQ_E3_cps)
temp <- union(temp, dsi_SQ_E3_cua)
temp <- union(temp, dsi_SQ_E3_df33)
temp <- union(temp, dsi_SQ_E3_df40)
temp <- union(temp, dsi_SQ_E3_dgo)
temp <- union(temp, dsi_SQ_E3_gua)
temp <- union(temp, dsi_SQ_E3_gue)
temp <- union(temp, dsi_SQ_E3_hgo)
temp <- union(temp, dsi_SQ_E3_jal)
temp <- union(temp, dsi_SQ_E3_mex)
temp <- union(temp, dsi_SQ_E3_mic)
temp <- union(temp, dsi_SQ_E3_mor12)
temp <- union(temp, dsi_SQ_E3_mor18)
temp <- union(temp, dsi_SQ_E3_nay)
temp <- union(temp, dsi_SQ_E3_nl)
temp <- union(temp, dsi_SQ_E3_cps)
temp <- union(temp, dsi_SQ_E3_pue)
temp <- union(temp, dsi_SQ_E3_que)
temp <- union(temp, dsi_SQ_E3_qui)
temp <- union(temp, dsi_SQ_E3_san)
temp <- union(temp, dsi_SQ_E3_sin)
temp <- union(temp, dsi_SQ_E3_son)
temp <- union(temp, dsi_SQ_E3_tab)
temp <- union(temp, dsi_SQ_E3_tam)
temp <- union(temp, dsi_SQ_E3_tla15)
temp <- union(temp, dsi_SQ_E3_tla19)
temp <- union(temp, dsi_SQ_E3_ver)
temp <- union(temp, dsi_SQ_E3_yuc)
temp <- union(temp, dsi_SQ_E3_cps)

SQ_E3 <- temp
names(SQ_E3) <- nombres
summary_SQ_E3 <- summary(SQ_E3)

SQ_E3$dcrit8 <- as.numeric(  SQ_E3$edon==5| SQ_E3$edon==9| SQ_E3$edon==12|
                               SQ_E3$edon==24|SQ_E3$edon==29|SQ_E3$edon==31|
                               SQ_E3$edon==32)

mod1 <- lm(dsi_sec~dcrit8+prop_urbana+prop_rural+dist2017+elec_cong_2018+
             PANgob_antes+PRIgob_antes+PRDgob_antes+
             PANgob_despues+PRIgob_despues+PRDgob_despues+
             PANcong_antes+PRIcong_antes+
             PANcong_despues+PRIcong_despues+Morenacong_despues,data=SQ_E3)


write.csv(SQ_E3,"/Volumes/DANIEL/tesis_DSI/SQ_E3.csv")
write.csv(summary_SQ_E3,"/Volumes/DANIEL/tesis_DSI/estad_desc_SQ_E3.csv")
#scatterplots 
ggplot(data=SQ_E3,aes(prop_rural, dsi_sec))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=0.08497,intercept=0.53879))+
  ggtitle(label="Proporción de secciones rurales  vs DSI por sección electoral")+
  xlab("Proporción de secciones rurales por distrito local")+ylab("DSI por sección electoral")
ggsave(filename = "proprural_dsisec.png",plot = last_plot(),device = "png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=SQ_E3,aes(prop_rural, dsi_pob))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=0.07958,intercept=0.54290))+
  ggtitle(label="Proporción de secciones rurales  vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones rurales por distrito local")+ylab("DSI por población")
ggsave(filename = "proprural_dsipob.png",plot = last_plot(),device = "png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")


ggplot(data=SQ_E3,aes(prop_urbana, dsi_sec))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=-0.07774,intercept=0.62115))+
  ggtitle(label="Proporción de secciones urbanas vs DSI por sección electoral")+
  xlab("Proporción de secciones urbanas por distrito local")+ylab("DSI por sección electoral")
ggsave(filename="propurb_dsisec.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=SQ_E3,aes(prop_urbana, dsi_pob))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=-0.07774,intercept=0.62115))+
  ggtitle(label="Proporción de secciones urbanas vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones urbanas por distrito local")+ylab("DSI por población")
ggsave(filename="propurb_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")
selecc <- which(SQ_E3$RRI_anterior>0.85 & SQ_E3$RRI_anterior<1.15)
SQ_E3_malap <- SQ_E3 %>% filter(RRI_anterior>.85 & RRI_anterior<1.15)

ggplot(data=SQ_E3_malap,aes(prop_bastion_pan, dsi_sec))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=-0.1773,intercept=0.6962))+
  ggtitle(label="Proporción de bastiones del PAN vs DSI por sección electoral")+
  xlab("Proporción de secciones bastión del PAN por distrito local")+ylab("DSI por sección electoral")
ggsave(filename = "bastionPAN_dsisec.png",plot = last_plot(),device = "png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=SQ_E3_malap,aes(prop_bastion_pri, dsi_sec))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=-0.01144,intercept=0.64973))+
  ggtitle(label="Proporción de bastiones del PRI vs DSI por sección electoral")+
  xlab("Proporción de secciones bastión del PRI por distrito local")+ylab("DSI por sección electoral")
ggsave(filename = "bastionPRI_dsisec.png",plot = last_plot(),device = "png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=SQ_E3_malap,aes(prop_bastion_prd, dsi_sec))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=0.1551,intercept=0.6059))+
  ggtitle(label="Proporción de bastiones del PRD vs DSI por sección electoral")+
  xlab("Proporción de secciones bastión del PRD por distrito local")+ylab("DSI por sección electoral")
ggsave(filename = "bastionPRD_dsisec.png",plot = last_plot(),device = "png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=SQ_E3_malap,aes(prop_bastion_pan, dsi_pob))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=-0.1840,intercept=0.7003))+
  ggtitle(label="Proporción de bastiones del PAN vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones bastión del PAN por distrito local")+ylab("DSI por población")
ggsave(filename = "bastionPAN_dsipob.png",plot = last_plot(),device = "png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=SQ_E3_malap,aes(prop_bastion_pri, dsi_pob))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=-0.0132,intercept=0.6527))+
  ggtitle(label="Proporción de bastiones del PRI vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones bastión del PRI por distrito local")+ylab("DSI por población")
ggsave(filename = "bastionPRI_dsipob.png",plot = last_plot(),device = "png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=SQ_E3_malap,aes(prop_bastion_prd, dsi_pob))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=0.1623,intercept=0.6063))+
  ggtitle(label="Proporción de bastiones del PRD vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones bastión del PRD por distrito local")+ylab("DSI por población")
ggsave(filename = "bastionPRD_dsipob.png",plot = last_plot(),device = "png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

#Año en que redistritaron los estados 
SQ_E1$dist2015 <- as.numeric(SQ_E1$edon==1 | SQ_E1$edon==2|SQ_E1$edon==5|
                               SQ_E1$edon==8|SQ_E1$edon==10|SQ_E1$edon==13|
                               SQ_E1$edon==20|SQ_E1$edon==21|SQ_E1$edon==23|
                               SQ_E1$edon==18|SQ_E1$edon==25|SQ_E1$edon==28|
                               SQ_E1$edon==29|SQ_E1$edon==30|SQ_E1$edon==32|SQ_E1$edon==35)

SQ_E3$dist2015 <- as.numeric(SQ_E3$edon==1 | SQ_E3$edon==2|SQ_E3$edon==5|
                               SQ_E3$edon==8|SQ_E3$edon==10|SQ_E3$edon==13|
                               SQ_E3$edon==20|SQ_E3$edon==21|SQ_E3$edon==23|
                               SQ_E3$edon==18|SQ_E3$edon==25|SQ_E3$edon==28|
                               SQ_E3$edon==29|SQ_E3$edon==30|SQ_E3$edon==32|SQ_E3$edon==35)

E1_E3$dist2015 <- as.numeric(E1_E3$edon==1 | E1_E3$edon==2|E1_E3$edon==5|
                               E1_E3$edon==8|E1_E3$edon==10|E1_E3$edon==13|
                               E1_E3$edon==20|E1_E3$edon==21|E1_E3$edon==23|
                               E1_E3$edon==18|E1_E3$edon==25|E1_E3$edon==28|
                               E1_E3$edon==29|E1_E3$edon==30|E1_E3$edon==32|E1_E3$edon==35)

SQ_E1$dist2017 <- as.numeric(SQ_E1$edon==3|SQ_E1$edon==4|SQ_E1$edon==6|SQ_E1$edon==7|
                               SQ_E1$edon==9|SQ_E1$edon==11|SQ_E1$edon==12|SQ_E1$edon==14|
                               SQ_E1$edon==15|SQ_E1$edon==16|SQ_E1$edon==17|SQ_E1$edon==19|
                               SQ_E1$edon==22|SQ_E1$edon==24|SQ_E1$edon==26|SQ_E1$edon==27|
                               SQ_E1$edon==31|SQ_E1$edon==33|SQ_E1$edon==34)
E1_E3$dist2017<- as.numeric(E1_E3$edon==3|E1_E3$edon==4| E1_E3$edon==6| E1_E3$edon==7|
                              E1_E3$edon==9| E1_E3$edon==11|E1_E3$edon==12|E1_E3$edon==14|
                              E1_E3$edon==15|E1_E3$edon==16|E1_E3$edon==17|E1_E3$edon==19|
                              E1_E3$edon==22|E1_E3$edon==24|E1_E3$edon==26|E1_E3$edon==27|
                              E1_E3$edon==31|E1_E3$edon==33|E1_E3$edon==34)
SQ_E3$dist2017 <- as.numeric(SQ_E3$edon==3|SQ_E3$edon==4|SQ_E3$edon==6|SQ_E3$edon==7|
                               SQ_E3$edon==9| SQ_E3$edon==11|SQ_E3$edon==12|SQ_E3$edon==14|
                               SQ_E3$edon==15|SQ_E3$edon==16|SQ_E3$edon==17|SQ_E3$edon==19|
                               SQ_E3$edon==22|SQ_E3$edon==24|SQ_E3$edon==26|SQ_E3$edon==27|
                               SQ_E3$edon==31|SQ_E3$edon==33|SQ_E3$edon==34)

#Primera elección con el nuevo mapa. Repetir para SQ_E3 y E1_E3
SQ_E1$elec_cong_2016 <- as.numeric(SQ_E1$edon==1|
                                     SQ_E1$edon==8|SQ_E1$edon==10|SQ_E1$edon==13|
                                     SQ_E1$edon==20|SQ_E1$edon==21|SQ_E1$edon==23|
                                     SQ_E1$edon==25|SQ_E1$edon==28|
                                     SQ_E1$edon==29|SQ_E1$edon==30|SQ_E1$edon==32|SQ_E1$edon==35)
SQ_E1$elec_cong_2017 <- as.numeric(SQ_E1$edon==5|SQ_E1$edon==18)
SQ_E1$elec_cong_2018 <- as.numeric(SQ_E1$edon==3|SQ_E1$edon==4|SQ_E1$edon==6|SQ_E1$edon==7|
                                     SQ_E1$edon==9|SQ_E1$edon==11|SQ_E1$edon==12|SQ_E1$edon==14|
                                     SQ_E1$edon==15|SQ_E1$edon==16|SQ_E1$edon==17|SQ_E1$edon==19|
                                     SQ_E1$edon==22|SQ_E1$edon==24|SQ_E1$edon==26|SQ_E1$edon==27|
                                     SQ_E1$edon==31|SQ_E1$edon==33|SQ_E1$edon==34)
SQ_E1$elec_cong_2019 <- as.numeric(SQ_E1$edon==2)


SQ_E3$elec_cong_2016 <- as.numeric(SQ_E3$edon==1|
                                     SQ_E3$edon==8| SQ_E3$edon==10|SQ_E3$edon==13|
                                     SQ_E3$edon==20|SQ_E3$edon==21|SQ_E3$edon==23|
                                     SQ_E3$edon==25|SQ_E3$edon==28|
                                     SQ_E3$edon==29|SQ_E3$edon==30|SQ_E3$edon==32|SQ_E3$edon==35)
SQ_E3$elec_cong_2017 <- as.numeric(SQ_E3$edon==5| SQ_E3$edon==18)
SQ_E3$elec_cong_2018 <- as.numeric(SQ_E3$edon==3| SQ_E3$edon==4|SQ_E3$edon==6|SQ_E3$edon==7|
                                     SQ_E3$edon==9| SQ_E3$edon==11|SQ_E3$edon==12|SQ_E3$edon==14|
                                     SQ_E3$edon==15|SQ_E3$edon==16|SQ_E3$edon==17|SQ_E3$edon==19|
                                     SQ_E3$edon==22|SQ_E3$edon==24|SQ_E3$edon==26|SQ_E3$edon==27|
                                     SQ_E3$edon==31|SQ_E3$edon==33|SQ_E3$edon==34)
SQ_E3$elec_cong_2019 <- as.numeric(SQ_E3$edon==2)


E1_E3$elec_cong_2016 <- as.numeric(E1_E3$edon==1|
                                     E1_E3$edon==8| E1_E3$edon==10|E1_E3$edon==13|
                                     E1_E3$edon==20|E1_E3$edon==21|E1_E3$edon==23|
                                     E1_E3$edon==25|E1_E3$edon==28|
                                     E1_E3$edon==29|E1_E3$edon==30|E1_E3$edon==32|E1_E3$edon==35)
E1_E3$elec_cong_2017 <- as.numeric(E1_E3$edon==5| E1_E3$edon==18)
E1_E3$elec_cong_2018 <- as.numeric(E1_E3$edon==3| E1_E3$edon==4| E1_E3$edon==6| E1_E3$edon==7|
                                     E1_E3$edon==9| E1_E3$edon==11|E1_E3$edon==12|E1_E3$edon==14|
                                     E1_E3$edon==15|E1_E3$edon==16|E1_E3$edon==17|E1_E3$edon==19|
                                     E1_E3$edon==22|E1_E3$edon==24|E1_E3$edon==26|E1_E3$edon==27|
                                     E1_E3$edon==31|E1_E3$edon==33|E1_E3$edon==34)
E1_E3$elec_cong_2019 <- as.numeric(E1_E3$edon==2)
#partido del gobernador antes de la primera elección local con nuevo mapa. Repetir para SQ_E3 y E1_E3
SQ_E1$PANgob_antes <- as.numeric(SQ_E1$edon==2|SQ_E1$edon==3|SQ_E1$edon==11|
                                   SQ_E1$edon==21|SQ_E1$edon==22|SQ_E1$edon==25)
SQ_E1$PRIgob_antes <- as.numeric(SQ_E1$edon==1|SQ_E1$edon==4|SQ_E1$edon==5|
                                   SQ_E1$edon==6|SQ_E1$edon==8|SQ_E1$edon==10|SQ_E1$edon==12|
                                   SQ_E1$edon==13|SQ_E1$edon==14|SQ_E1$edon==15|SQ_E1$edon==18|
                                   SQ_E1$edon==23|SQ_E1$edon==24|SQ_E1$edon==26|SQ_E1$edon==28|
                                   SQ_E1$edon==29|SQ_E1$edon==30|SQ_E1$edon==31|SQ_E1$edon==32|SQ_E1$edon==35)
SQ_E1$PRDgob_antes <- as.numeric(SQ_E1$edon==9|SQ_E1$edon==16|SQ_E1$edon==17|SQ_E1$edon==27|SQ_E1$edon==33|SQ_E1$edon==34)
SQ_E1$PVEMgob_antes <- as.numeric(SQ_E1$edon==7)
SQ_E1$MCgob_antes <- as.numeric(SQ_E1$edon==20)
SQ_E1$Indgob_antes <- as.numeric(SQ_E1$edon==19)


SQ_E3$PANgob_antes <- as.numeric(SQ_E3$edon==2| SQ_E3$edon==3| SQ_E3$edon==11|
                                   SQ_E3$edon==21|SQ_E3$edon==22|SQ_E3$edon==25)
SQ_E3$PRIgob_antes <- as.numeric(SQ_E3$edon==1| SQ_E3$edon==4| SQ_E3$edon==5|
                                   SQ_E3$edon==6| SQ_E3$edon==8| SQ_E3$edon==10|SQ_E3$edon==12|
                                   SQ_E3$edon==13|SQ_E3$edon==14|SQ_E3$edon==15|SQ_E3$edon==18|
                                   SQ_E3$edon==23|SQ_E3$edon==24|SQ_E3$edon==26|SQ_E3$edon==28|SQ_E3$edon==7|
                                   SQ_E3$edon==29|SQ_E3$edon==30|SQ_E3$edon==31|SQ_E3$edon==32|SQ_E3$edon==35)
SQ_E3$PRDgob_antes <- as.numeric(SQ_E3$edon==9| SQ_E3$edon==16|SQ_E3$edon==17|SQ_E3$edon==27|SQ_E3$edon==33|SQ_E3$edon==34|SQ_E3$edon==20)
#SQ_E3$PVEMgob_antes <- as.numeric(SQ_E3$edon==7)
#SQ_E3$MCgob_antes <- as.numeric(SQ_E3$edon==20)
SQ_E3$Indgob_antes <- as.numeric(SQ_E3$edon==19)


E1_E3$PANgob_antes <- as.numeric(E1_E3$edon==2| E1_E3$edon==3| E1_E3$edon==11|
                                   E1_E3$edon==21|E1_E3$edon==22|E1_E3$edon==25)
E1_E3$PRIgob_antes <- as.numeric(E1_E3$edon==1| E1_E3$edon==4| E1_E3$edon==5|
                                   E1_E3$edon==6| E1_E3$edon==8| E1_E3$edon==10|E1_E3$edon==12|
                                   E1_E3$edon==13|E1_E3$edon==14|E1_E3$edon==15|E1_E3$edon==18|
                                   E1_E3$edon==23|E1_E3$edon==24|E1_E3$edon==26|E1_E3$edon==28|
                                   E1_E3$edon==29|E1_E3$edon==30|E1_E3$edon==31|E1_E3$edon==32|E1_E3$edon==35)
E1_E3$PRDgob_antes <- as.numeric(E1_E3$edon==9| E1_E3$edon==16|E1_E3$edon==17|E1_E3$edon==27|E1_E3$edon==33|E1_E3$edon==34)
E1_E3$PVEMgob_antes <- as.numeric(E1_E3$edon==7)
E1_E3$MCgob_antes <- as.numeric(E1_E3$edon==20)
E1_E3$Indgob_antes <- as.numeric(E1_E3$edon==19)


#partido del gobernador después de la primera elección local con nuevo mapa.
SQ_E1$PANgob_despues <- as.numeric(SQ_E1$edon==2|SQ_E1$edon==3|SQ_E1$edon==11|
                                     SQ_E1$edon==21|SQ_E1$edon==22|SQ_E1$edon==1|
                                     SQ_E1$edon==8|SQ_E1$edon==10|SQ_E1$edon==18|
                                     SQ_E1$edon==28|
                                     SQ_E1$edon==30|SQ_E1$edon==31)
SQ_E1$PRIgob_despues <- as.numeric(SQ_E1$edon==4|SQ_E1$edon==5|
                                     SQ_E1$edon==6|SQ_E1$edon==12|
                                     SQ_E1$edon==13|SQ_E1$edon==15|SQ_E1$edon==20|
                                     SQ_E1$edon==24|SQ_E1$edon==26|SQ_E1$edon==25|
                                     SQ_E1$edon==29|SQ_E1$edon==32|SQ_E1$edon==35)
SQ_E1$PRDgob_despues <- as.numeric(SQ_E1$edon==16|SQ_E1$edon==23)
SQ_E1$Morenagob_despues <- as.numeric(SQ_E1$edon==7,SQ_E1$edon==9,SQ_E1$edon==27|SQ_E1$edon==33)
SQ_E1$MCgob_despues <- as.numeric(SQ_E1$edon==14)
SQ_E1$Indgob_despues <- as.numeric(SQ_E1$edon==19)
SQ_E1$PESgob_despues <- as.numeric(SQ_E1$edon==17|SQ_E1$edon==34)


SQ_E3$PANgob_despues <- as.numeric(SQ_E3$edon==2| SQ_E3$edon==3| SQ_E3$edon==11|
                                     SQ_E3$edon==21|SQ_E3$edon==22|SQ_E3$edon==1|
                                     SQ_E3$edon==8| SQ_E3$edon==10|SQ_E3$edon==18|
                                     SQ_E3$edon==28|
                                     SQ_E3$edon==30|SQ_E3$edon==31)
SQ_E3$PRIgob_despues <- as.numeric(SQ_E3$edon==4| SQ_E3$edon==5|
                                     SQ_E3$edon==6| SQ_E3$edon==12|
                                     SQ_E3$edon==13|SQ_E3$edon==15|SQ_E3$edon==20|
                                     SQ_E3$edon==24|SQ_E3$edon==26|SQ_E3$edon==25|
                                     SQ_E3$edon==29|SQ_E3$edon==32|SQ_E3$edon==35)
SQ_E3$PRDgob_despues <- as.numeric(SQ_E3$edon==16|SQ_E3$edon==23)
SQ_E3$Morenagob_despues <- as.numeric(SQ_E3$edon==7,SQ_E3$edon==9,SQ_E3$edon==27|SQ_E3$edon==33)
SQ_E3$MCgob_despues <- as.numeric( SQ_E3$edon==14)
SQ_E3$Indgob_despues <- as.numeric(SQ_E3$edon==19)
SQ_E3$PESgob_despues <- as.numeric(SQ_E3$edon==17|SQ_E3$edon==34)


E1_E3$PANgob_despues <- as.numeric(E1_E3$edon==2| E1_E3$edon==3| E1_E3$edon==11|
                                     E1_E3$edon==21|E1_E3$edon==22|E1_E3$edon==1|
                                     E1_E3$edon==8| E1_E3$edon==10|E1_E3$edon==18|
                                     E1_E3$edon==28|
                                     E1_E3$edon==30|E1_E3$edon==31)
E1_E3$PRIgob_despues <- as.numeric(E1_E3$edon==4| E1_E3$edon==5|
                                     E1_E3$edon==6| E1_E3$edon==12|
                                     E1_E3$edon==13|E1_E3$edon==15|E1_E3$edon==20|
                                     E1_E3$edon==24|E1_E3$edon==26|E1_E3$edon==25|
                                     E1_E3$edon==29|E1_E3$edon==32|E1_E3$edon==35)
E1_E3$PRDgob_despues <- as.numeric(E1_E3$edon==16|E1_E3$edon==23)
E1_E3$Morenagob_despues <- as.numeric(E1_E3$edon==7,E1_E3$edon==9,E1_E3$edon==27|E1_E3$edon==33)
E1_E3$MCgob_despues <- as.numeric( E1_E3$edon==14)
E1_E3$Indgob_despues <- as.numeric(E1_E3$edon==19)
E1_E3$PESgob_despues <- as.numeric(E1_E3$edon==17|E1_E3$edon==34)

#partido con mayoría relativa en los congresos locales antes del nuevo mapa
SQ_E1$PANcong_antes <- as.numeric(SQ_E1$edon==1|SQ_E1$edon==2|SQ_E1$edon==3|
                                    SQ_E1$edon==6|SQ_E1$edon==8|SQ_E1$edon==10|
                                    SQ_E1$edon==11|SQ_E1$edon==19|SQ_E1$edon==21|
                                    SQ_E1$edon==22|SQ_E1$edon==24|SQ_E1$edon==30)
SQ_E1$PRIcong_antes <- as.numeric(SQ_E1$edon==4|SQ_E1$edon==5|SQ_E1$edon==12|
                                    SQ_E1$edon==13|SQ_E1$edon==14|SQ_E1$edon==15|
                                    SQ_E1$edon==16|SQ_E1$edon==18|SQ_E1$edon==20|
                                    SQ_E1$edon==23|SQ_E1$edon==25|SQ_E1$edon==26|
                                    SQ_E1$edon==28|SQ_E1$edon==29|SQ_E1$edon==31|
                                    SQ_E1$edon==32|SQ_E1$edon==24|SQ_E1$edon==35)
SQ_E1$PRDcong_antes <- as.numeric(SQ_E1$edon==9|SQ_E1$edon==17|SQ_E1$edon==27|SQ_E1$edon==33|SQ_E1$edon==34)
SQ_E1$PVEMcong_antes <- as.numeric(SQ_E1$edon==7)
SQ_E1$MCcong_antes <- as.numeric(SQ_E1$edon==14)


SQ_E3$PANcong_antes <- as.numeric(SQ_E3$edon==1| SQ_E3$edon==2| SQ_E3$edon==3|
                                    SQ_E3$edon==6| SQ_E3$edon==8| SQ_E3$edon==10|
                                    SQ_E3$edon==11|SQ_E3$edon==19|SQ_E3$edon==21|
                                    SQ_E3$edon==22|SQ_E3$edon==24|SQ_E3$edon==30)
SQ_E3$PRIcong_antes <- as.numeric(SQ_E3$edon==4| SQ_E3$edon==5| SQ_E3$edon==12|
                                    SQ_E3$edon==13|SQ_E3$edon==14|SQ_E3$edon==15|
                                    SQ_E3$edon==16|SQ_E3$edon==18|SQ_E3$edon==20|
                                    SQ_E3$edon==23|SQ_E3$edon==25|SQ_E3$edon==26|
                                    SQ_E3$edon==28|SQ_E3$edon==29|SQ_E3$edon==31|
                                    SQ_E3$edon==32|SQ_E3$edon==24|SQ_E3$edon==35)
SQ_E3$PRDcong_antes <- as.numeric(SQ_E3$edon==9| SQ_E3$edon==17|SQ_E3$edon==27|SQ_E3$edon==33|SQ_E3$edon==34)
SQ_E3$PVEMcong_antes <- as.numeric(SQ_E3$edon==7)
SQ_E3$MCcong_antes <- as.numeric(SQ_E3$edon==14)


E1_E3$PANcong_antes <- as.numeric(E1_E3$edon==1| E1_E3$edon==2| E1_E3$edon==3|
                                    E1_E3$edon==6| E1_E3$edon==8| E1_E3$edon==10|
                                    E1_E3$edon==11|E1_E3$edon==19|E1_E3$edon==21|
                                    E1_E3$edon==22|E1_E3$edon==24|E1_E3$edon==30)
E1_E3$PRIcong_antes <- as.numeric(E1_E3$edon==4| E1_E3$edon==5| E1_E3$edon==12|
                                    E1_E3$edon==13|E1_E3$edon==14|E1_E3$edon==15|
                                    E1_E3$edon==16|E1_E3$edon==18|E1_E3$edon==20|
                                    E1_E3$edon==23|E1_E3$edon==25|E1_E3$edon==26|
                                    E1_E3$edon==28|E1_E3$edon==29|E1_E3$edon==31|
                                    E1_E3$edon==32|E1_E3$edon==24|E1_E3$edon==35)
E1_E3$PRDcong_antes <- as.numeric(E1_E3$edon==9| E1_E3$edon==17|E1_E3$edon==27|E1_E3$edon==33|E1_E3$edon==34)
E1_E3$PVEMcong_antes <- as.numeric(E1_E3$edon==7)
E1_E3$MCcong_antes <- as.numeric(E1_E3$edon==14)

#partido con mayoría relativa en los congresos locales después del nuevo mapa
SQ_E1$PANcong_despues <- as.numeric(SQ_E1$edon==1|SQ_E1$edon==8|SQ_E1$edon==11|
                                      SQ_E1$edon==18|SQ_E1$edon==19|SQ_E1$edon==22|
                                      SQ_E1$edon==24|SQ_E1$edon==28)
SQ_E1$PRIcong_despues <- as.numeric(SQ_E1$edon==5|SQ_E1$edon==23|SQ_E1$edon==31|SQ_E1$edon==32)
SQ_E1$MCcong_despues <- as.numeric(SQ_E1$edon==14)
SQ_E1$PENDIENTEcong_despues <- as.numeric(SQ_E1$edon==2)
SQ_E1$Morenacong_despues <- as.numeric(SQ_E1$edon==3|SQ_E1$edon==4|SQ_E1$edon==6|SQ_E1$edon==7|SQ_E1$edon==9|
                                         SQ_E1$edon==10|SQ_E1$edon==12|SQ_E1$edon==13|SQ_E1$edon==15|
                                         SQ_E1$edon==16|SQ_E1$edon==17|SQ_E1$edon==20|SQ_E1$edon==21|SQ_E1$edon==24|
                                         SQ_E1$edon==25|SQ_E1$edon==26|SQ_E1$edon==27|SQ_E1$edon==29|SQ_E1$edon==30|SQ_E1$edon==33|SQ_E1$edon==34|SQ_E1$edon==35)



SQ_E3$PANcong_despues <- as.numeric(SQ_E3$edon==1| SQ_E3$edon==8| SQ_E3$edon==11|
                                      SQ_E3$edon==18|SQ_E3$edon==19|SQ_E3$edon==22|
                                      SQ_E3$edon==24|SQ_E3$edon==28)
SQ_E3$PRIcong_despues <- as.numeric(SQ_E3$edon==5| SQ_E3$edon==23|SQ_E3$edon==31|SQ_E3$edon==32)
SQ_E3$MCcong_despues <- as.numeric(   SQ_E3$edon==14)
SQ_E3$PENDIENTEcong_despues <- as.numeric(SQ_E3$edon==2)
SQ_E3$Morenacong_despues <- as.numeric(SQ_E3$edon==3| SQ_E3$edon==4| SQ_E3$edon==6| SQ_E3$edon==7|SQ_E3$edon==9|
                                         SQ_E3$edon==10|SQ_E3$edon==12|SQ_E3$edon==13|SQ_E3$edon==15|
                                         SQ_E3$edon==16|SQ_E3$edon==17|SQ_E3$edon==20|SQ_E3$edon==21|SQ_E3$edon==24|
                                         SQ_E3$edon==25|SQ_E3$edon==26|SQ_E3$edon==27|SQ_E3$edon==29|SQ_E3$edon==30|SQ_E3$edon==33|SQ_E3$edon==34|SQ_E3$edon==35)



E1_E3$PANcong_despues <- as.numeric(E1_E3$edon==1| E1_E3$edon==8| E1_E3$edon==11|
                                      E1_E3$edon==18|E1_E3$edon==19|E1_E3$edon==22|
                                      E1_E3$edon==24|E1_E3$edon==28)
E1_E3$PRIcong_despues <- as.numeric(E1_E3$edon==5| E1_E3$edon==23|E1_E3$edon==31|E1_E3$edon==32)
E1_E3$MCcong_despues <- as.numeric(   E1_E3$edon==14)
E1_E3$PENDIENTEcong_despues <- as.numeric(E1_E3$edon==2)
E1_E3$Morenacong_despues <- as.numeric(E1_E3$edon==3| E1_E3$edon==4|  E1_E3$edon==6|E1_E3$edon==7|E1_E3$edon==9|
                                         E1_E3$edon==10|E1_E3$edon==12| E1_E3$edon==13|E1_E3$edon==15|
                                         E1_E3$edon==16|E1_E3$edon==17| E1_E3$edon==20|E1_E3$edon==21|E1_E3$edon==24|
                                         E1_E3$edon==25|E1_E3$edon==26|E1_E3$edon==27|E1_E3$edon==29|E1_E3$edon==30|E1_E3$edon==33|E1_E3$edon==34|E1_E3$edon==35)
#Gobernador 2 antes
SQ_E1$PANgober_2antes <- as.numeric(SQ_E1$edon==1|SQ_E1$edon==2|SQ_E1$edon==11|
                                      SQ_E1$edon==14|SQ_E1$edon==17|SQ_E1$edon==22|
                                      SQ_E1$edon==24|SQ_E1$edon==29|SQ_E1$edon==34|SQ_E1$edon==35)
SQ_E1$PRIgober_2antes <- as.numeric(SQ_E1$edon==4|SQ_E1$edon==5|SQ_E1$edon==6|SQ_E1$edon==8|
                                      SQ_E1$edon==10|SQ_E1$edon==13|SQ_E1$edon==15|SQ_E1$edon==18|
                                      SQ_E1$edon==19|SQ_E1$edon==20|SQ_E1$edon==21|SQ_E1$edon==23|
                                      SQ_E1$edon==25|SQ_E1$edon==26|SQ_E1$edon==27|SQ_E1$edon==28|
                                      SQ_E1$edon==30|SQ_E1$edon==31)
SQ_E1$PRDgober_2antes <- as.numeric(SQ_E1$edon==3|SQ_E1$edon==7|SQ_E1$edon==12|
                                      SQ_E1$edon==32|SQ_E1$edon==9|SQ_E1$edon==16|SQ_E1$edon==33)

SQ_E3$PANgober_2antes <- as.numeric(SQ_E3$edon==1|SQ_E3$edon==2|SQ_E3$edon==11|
                                      SQ_E3$edon==14|SQ_E3$edon==17|SQ_E3$edon==22|
                                      SQ_E3$edon==24|SQ_E3$edon==29|SQ_E3$edon==34|SQ_E3$edon==35)
SQ_E3$PRIgober_2antes <- as.numeric(SQ_E3$edon==4|SQ_E3$edon==5|SQ_E3$edon==6|SQ_E3$edon==8|
                                      SQ_E3$edon==10|SQ_E3$edon==13|SQ_E3$edon==15|SQ_E3$edon==18|
                                      SQ_E3$edon==19|SQ_E3$edon==20|SQ_E3$edon==21|SQ_E3$edon==23|
                                      SQ_E3$edon==25|SQ_E3$edon==26|SQ_E3$edon==27|SQ_E3$edon==28|
                                      SQ_E3$edon==30|SQ_E3$edon==31)
SQ_E3$PRDgober_2antes <- as.numeric(SQ_E3$edon==3|SQ_E3$edon==7|SQ_E3$edon==12|
                                      SQ_E3$edon==32|SQ_E3$edon==9|SQ_E3$edon==16|SQ_E3$edon==33)

E1_E3$PANgober_2antes <- as.numeric(E1_E3$edon==1| E1_E3$edon==2| E1_E3$edon==11|
                                      E1_E3$edon==14|E1_E3$edon==17|E1_E3$edon==22|
                                      E1_E3$edon==24|E1_E3$edon==29|E1_E3$edon==34|E1_E3$edon==35)
E1_E3$PRIgober_2antes <- as.numeric(E1_E3$edon==4| E1_E3$edon==5| E1_E3$edon==6| E1_E3$edon==8|
                                      E1_E3$edon==10|E1_E3$edon==13|E1_E3$edon==15|E1_E3$edon==18|
                                      E1_E3$edon==19|E1_E3$edon==20|E1_E3$edon==21|E1_E3$edon==23|
                                      E1_E3$edon==25|E1_E3$edon==26|E1_E3$edon==27|E1_E3$edon==28|
                                      E1_E3$edon==30|E1_E3$edon==31)
E1_E3$PRDgober_2antes <- as.numeric(E1_E3$edon==3|E1_E3$edon==7|E1_E3$edon==12|
                                      E1_E3$edon==32|E1_E3$edon==9|E1_E3$edon==16|E1_E3$edon==33)



#Histogramas
h<-hist(SQ_E1$dsi_sec, breaks=50,density=100, ylim=c(0,80),xlab="DSI",ylab="Frecuencia",main="Histograma del DSI por sección electoral SQ-E1",col="grey",border="black")
xfit<-seq(min(SQ_E1$dsi_sec,na.rm = T),max(SQ_E1$dsi_sec,na.rm = T),length=200)
yfit<-dnorm(xfit,mean=mean(SQ_E1$dsi_sec,na.rm = T),sd=sd(SQ_E1$dsi_sec,na.rm = T))
yfit<-yfit*diff(h$mids[1:2])*length(SQ_E1$dsi_sec)
lines(xfit,yfit, col="black",lwd=2)

mode()

h<-hist(SQ_E1$dsi_pob, breaks=50, density=100, ylim=c(0,80),xlab="DSI",ylab="Frecuencia",main="Histograma del DSI población SQ-E1",col = "grey",border="black")
xfit<-seq(min(SQ_E1$dsi_pob,na.rm = T),max(SQ_E1$dsi_pob,na.rm = T),length=200)
yfit<-dnorm(xfit,mean=mean(SQ_E1$dsi_pob,na.rm = T),sd=sd(SQ_E1$dsi_pob,na.rm = T))
yfit<-yfit*diff(h$mids[1:2])*length(SQ_E1$dsi_pob)
lines(xfit,yfit, col="black",lwd=2)

h<-hist(SQ_E3$dsi_sec, breaks=50, density=100, ylim=c(0,80), xlab="DSI",ylab="Frecuencia",main="Histograma del DSI secciones SQ-E3",col="grey",border="black")
xfit<-seq(min(SQ_E3$dsi_sec,na.rm = T),max(SQ_E3$dsi_sec,na.rm = T),length=200)
yfit<-dnorm(xfit,mean=mean(SQ_E3$dsi_sec,na.rm = T),sd=sd(SQ_E3$dsi_sec,na.rm = T))
yfit<-yfit*diff(h$mids[1:2])*length(SQ_E3$dsi_sec)
lines(xfit,yfit, col="black",lwd=2)

h<-hist(SQ_E3$dsi_pob, breaks=50, density=100, ylim=c(0,80), xlab="DSI",ylab="Frecuencia",main="Histograma del DSI población SQ-E3",col = "grey",border="black")
xfit<-seq(min(SQ_E3$dsi_pob,na.rm = T),max(SQ_E3$dsi_pob,na.rm = T),length=200)
yfit<-dnorm(xfit,mean=mean(SQ_E3$dsi_pob,na.rm = T),sd=sd(SQ_E3$dsi_pob,na.rm = T))
yfit<-yfit*diff(h$mids[1:2])*length(SQ_E3$dsi_pob)
lines(xfit,yfit, col="black",lwd=2)

h<-hist(E1_E3$dsi_sec, breaks=50, density=100, ylim=c(0,80),xlab="DSI",ylab="Frecuencia",main="Histograma del DSI secciones E1-E3",col="grey",border="black")
xfit<-seq(min(E1_E3$dsi_sec,na.rm = T),max(E1_E3$dsi_sec,na.rm = T),length=200)
yfit<-dnorm(xfit,mean=mean(E1_E3$dsi_sec,na.rm = T),sd=sd(E1_E3$dsi_sec,na.rm = T))
yfit<-yfit*diff(h$mids[1:2])*length(E1_E3$dsi_sec)
lines(xfit,yfit, col="black",lwd=2)

h<-hist(E1_E3$dsi_pob, breaks=50, density=100, ylim=c(0,80), xlab="DSI",ylab="Frecuencia",main="Histograma del DSI población E1-E3",col = "grey",border="black")
xfit<-seq(min(E1_E3$dsi_pob,na.rm = T),max(E1_E3$dsi_pob,na.rm = T),length=200)
yfit<-dnorm(xfit,mean=mean(E1_E3$dsi_pob,na.rm = T),sd=sd(E1_E3$dsi_pob,na.rm = T))
yfit<-yfit*diff(h$mids[1:2])*length(E1_E3$dsi_pob)
lines(xfit,yfit, col="black",lwd=2)


PANgobantes<- SQ_E3 %>% filter(PANgob_antes==1)

PRIgobantes <- SQ_E3 %>% filter(PRIgob_antes==1)

PRDgobantes <- SQ_E3 %>% filter(PRDgob_antes==1)

ggplot(data=PRDgobantes, aes(prop_urbana, dsi_pob))+
  geom_point(color="goldenrod")+theme_minimal()+geom_abline(aes(slope=0.06442,intercept=0.60287))+
  ggtitle(label="Proporción de secciones urbanas en estados gobernados por el PRD vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones urbanas por distrito local")+ylab("DSI por población")
ggsave(filename="PRDpropurb_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=PRDgobantes,aes(prop_urbana, dsi_sec))+
  geom_point(color="goldenrod")+theme_minimal()+geom_abline(aes(slope=0.05724,intercept=0.60327))+
  ggtitle(label="Proporción de secciones urbanas en estados gobernados por el PRD vs DSI por sección electoral")+
  xlab("Proporción de secciones urbanas por distrito local")+ylab("DSI por sección electoral")
ggsave(filename="PRDpropurb_dsisec.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=PRDgobantes, aes(prop_rural, dsi_pob))+
  geom_point(color="goldenrod")+theme_minimal()+geom_abline(aes(slope=-0.06445,intercept=0.66730))+
  ggtitle(label="Proporción de secciones rurales en estados gobernados por el PRD vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones rurales por distrito local")+ylab("DSI por población")
ggsave(filename="PRDproprur_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=PRDgobantes, aes(prop_rural, dsi_sec))+
  geom_point(color="goldenrod")+theme_minimal()+geom_abline(aes(slope=-0.05727,intercept=0.66051))+
  ggtitle(label="Proporción de secciones rurales en estados gobernados por el PRD vs DSI por sección electoral")+
  xlab("Proporción de secciones urbanas por distrito local")+ylab("DSI por sección electoral")
ggsave(filename="PRDproprur_dsisec.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")


ggplot(data=PRIgobantes, aes(prop_urbana, dsi_sec))+
  geom_point(color="darkred")+theme_minimal()+geom_abline(aes(slope=-0.1317,intercept=0.6512))+
  ggtitle(label="Proporción de secciones urbanas en estados gobernados por el PRD vs DSI por sección electoral")+
  xlab("Proporción de secciones urbanas por distrito local")+ylab("DSI por sección electoral")
ggsave(filename="PRIpropurb_dsisec.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=PRIgobantes, aes(prop_rural, dsi_pob))+
  geom_point(color="darkred")+theme_minimal()+geom_abline(aes(slope=0.1335,intercept=0.5206))+
  ggtitle(label="Proporción de secciones rurales en estados gobernados por el PRI vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones rurales por distrito local")+ylab("DSI por población")
ggsave(filename="PRIproprur_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=PRIgobantes, aes(prop_urbana, dsi_pob))+
  geom_point(color="darkred")+theme_minimal()+geom_abline(aes(slope=-0.1297,intercept=0.6512))+
  ggtitle(label="Proporción de secciones urbanas en estados gobernados por el PRI vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones urbanas por distrito local")+ylab("DSI por población")
ggsave(filename="PRIpropurb_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=PRIgobantes, aes(prop_rural, dsi_sec))+
  geom_point(color="darkred")+theme_minimal()+geom_abline(aes(slope=0.1356,intercept=0.5186))+
  ggtitle(label="Proporción de secciones rurales en estados gobernados por el PRI vs DSI por sección electoral")+
  xlab("Proporción de secciones rurales por distrito local")+ylab("DSI por sección electoral")
ggsave(filename="PRIproprur_dsisec.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")




ggplot(data=PANgobantes, aes(prop_rural, dsi_pob))+
  geom_point(color="navyblue")+theme_minimal()+geom_abline(aes(slope=0.2909,intercept=0.3777))+
  ggtitle(label="Proporción de secciones rurales en estados gobernados por el PAN vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones rurales por distrito local")+ylab("DSI por población")
ggsave(filename="PANproprur_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")


ggplot(data=PANgobantes,aes(prop_urbana, dsi_pob))+
  geom_point(color="darkblue")+theme_minimal()+geom_abline(aes(slope=-0.2908,intercept=0.6686))+
  ggtitle(label="Proporción de secciones urbanas en estados gobernados por el PAN vs DSI por población",subtitle="Censo 2010, INEGI")+
  xlab("Proporción de secciones urbanas por distrito local")+ylab("DSI por población")
ggsave(filename="PANpropurb_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=PANgobantes,aes(prop_urbana, dsi_sec))+
  geom_point(color="darkblue")+theme_minimal()+geom_abline(aes(slope=-0.2786,intercept=0.6618))+
  ggtitle(label="Proporción de secciones urbanas en estados gobernados por el PAN vs DSI por sección electoral")+
  xlab("Proporción de secciones urbanas por distrito local")+ylab("DSI por sección electoral")
ggsave(filename="PANpropurb_dsisec.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")

ggplot(data=PANgobantes,aes(prop_rural, dsi_sec))+
  geom_point(color="darkblue")+theme_minimal()+geom_abline(aes(slope=0.2786,intercept=0.3832))+
  ggtitle(label="Proporción de secciones rurales en estados gobernados por el PAN vs DSI por sección electoral")+
  xlab("Proporción de secciones rurales por distrito local")+ylab("DSI por sección electoral")
ggsave(filename="PANproprur_dsisec.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/archivos/scatterplots/")


#en E1vsE3 los valores de dsi cercanos a 1 significa que son 
#distritos que poco cpsbiaron del primer al tercer escenario
#por lo tanto, habría que aislar los dsi casi 1 que son los reconstruidos con el c8
#y los distritos con dsi no 1 son los que aceptaron cpsbios ¿cuántos y quién?

sincrit8E1_E3 <- E1_E3 %>% 
  filter(dcrit8==0)
concrit8E1_E3<-E1_E3 %>% 
  filter(dcrit8==1)

sincrit8E1_E3sindsi1 <- sincrit8E1_E3 %>% 
  filter(dsi_sec<0.9)

concrit8E1_E3sindsi1 <- concrit8E1_E3 %>% 
  filter(dsi_sec<0.9)

sincrit8SQ_E3<-SQ_E3 %>% 
  filter(dcrit8==0)
concrit8SQ_E3 <- SQ_E3 %>% 
  filter(dcrit8==1)

sincrit8SQ_E1 <- SQ_E1 %>% 
  filter(dcrit8==0)
concrit8SQ_E1 <- SQ_E1 %>% 
  filter(dcrit8==1)

#aislar los dsi de E3 que son distintos de los de E1
dsisec <- data.frame(SQ_E1$dsi_sec,SQ_E3$dsi_sec,SQ_E1$dcrit8,SQ_E1$edon,SQ_E1$esc1,SQ_E1$esc3)
dsisec$dif <- as.numeric(dsisec$SQ_E1.dsi_sec != dsisec$SQ_E3.dsi_sec)
base<-dsisec %>% filter(dif==1) %>% filter(SQ_E1.dcrit8==0) %>% filter( SQ_E3.dsi_sec>0.9)
write.csv(base,"/Volumes/DANIEL/tesis_DSI/pasana1noc8.csv")

kk <- "/Volumes/DANIEL/comparaciones/"
k<-read.csv(paste(kk,"ver.csv",sep=""),stringsAsFactors = F)
head(k,1)    

dsisecc <- data.frame(sincrit8E1_E3sindsi1$dsi_sec,sincrit8E1_E3sindsi1$esc1,sincrit8E1_E3sindsi1$esc3,sincrit8E1_E3sindsi1$edon)

#hacer matriz de dsi por estado 35 estados 
#SQ_E3<-SQ_E3
SQ_E3 <- SQ_E3 %>% mutate(estado = case_when(
  edon==1 ~"Aguascalientes",
  edon==2~"Baja California",
  edon==3~"Baja California Sur",
  edon==4~"cpspeche",
  edon==5~"Coahuila",
  edon==6~"Colima",
  edon==7~"Chiapas",
  edon==8~"Chihuahua",
  edon==9~"Ciudad de México",
  edon==10~"Durango",
  edon==11~"Guanajuato",
  edon==12~"Guerrero",
  edon==13~"Hidalgo",
  edon==14~"Jalisco",
  edon==15~"Estado de México",
  edon==16~"Michoacán",
  edon==17~"Morelos",
  edon==18~"Nayarit",
  edon==19~"Nuevo León",
  edon==20~"cpsaca",
  edon==21~"Puebla",
  edon==22~"Querétaro",
  edon==23~"Quintana Roo",
  edon==24~"San Luis Potosí",
  edon==25~"Sinaloa",
  edon==26~"Sonora",
  edon==27~"Tabasco",
  edon==28~"Tamaulipas",
  edon==29~"Tlaxcala",
  edon==30~"Veracruz",
  edon==31~"Yucatán",
  edon==32~"cpsatecas",
  edon==33~"Ciudad de México 40",
  edon==34~"Morelos 18",
  edon==35~"Tlaxcala 19"
))

ggplot(E1_E3temp,aes(dsi_pob))+
  geom_histogram(color="black",bins=25,fill="white")+facet_wrap(vars(estado),nrow=5,ncol=7)+
  theme_minimal()+
  ggtitle("Distribución del DSI_pob E1 vs E3 por entidad")

ggplot(data=SQ_E3temp, aes(prop_bastion_prd, dsi_pob))+
  geom_point(color="goldenrod")+theme_minimal()+geom_abline(aes(slope=0.15354,intercept=0.55849))+
  ggtitle(label="Proporción de bastiones del PRD vs DSI por población", subtitle="Censo 2010, INEGI")+
  xlab("Proporción de bastiones del PRD por distrito local")+ylab("DSI por población")
ggsave(filename="PRDbast_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")
summary(lm(dsi_pob~prop_bastion_prd,SQ_E3temp))

ggplot(data=SQ_E3temp, aes(prop_bastion_prd, dsi_sec))+
  geom_point(color="goldenrod")+theme_minimal()+geom_abline(aes(slope=0.14540,intercept=0.55789))+
  ggtitle(label="Proporción de bastiones del PRD vs DSI por distrito electoral")+
  xlab("Proporción de bastiones del PRD por distrito local")+ylab("DSI por sección electoral")
ggsave(filename="PRDbast_dsisec.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")
summary(lm(dsi_sec~prop_bastion_prd,SQ_E3temp))

ggplot(data=SQ_E3temp, aes(prop_urbana, dsi_sec))+
  geom_point()+theme_minimal()+geom_abline(aes(slope=-0.008364,intercept=0.63986))+
  ggtitle("Proporción de secciones urbanas vs DSI por distrito electoral")+
  xlab("Proporción de secciones urbanas por distrito local")+ylab("DSI por sección electoral")
ggsave(filename="urbana_dsisec.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")

summary(lm(dsi_sec~prop_urbana,SQ_E3temp))

ggplot(data=SQ_E3temp, aes(prop_urbana, dsi_sec))+
  geom_point()+theme_minimal()+geom_abline(slope=-0.08364,intercept=0.63986)+
  summary(lm(dsi_sec~prop_urbana,SQ_E3temp))

ggplot(SQ_E3temp,aes(RRI_anterior))+
  geom_dotplot(color="black",fill="white")+facet_wrap(vars(estado),nrow=5,ncol=7)+
  theme_minimal()+geom_vline(xintercept =0.85,colour="red")+
  geom_vline(xintercept =1.15,colour="red")+
  ggtitle("Malapportionment por entidad, SQ")
ggsave(filename="RRIant.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")

ggplot(SQ_E3temp,aes(RRI_nueva))+
  geom_dotplot(color="black",fill="white")+facet_wrap(vars(estado),nrow=5,ncol=7)+
  theme_minimal()+geom_vline(xintercept =0.85,colour="red")+
  geom_vline(xintercept =1.15,colour="red")+
  ggtitle("Malapportionment por entidad, E3")
ggsave(filename="RRInuevo.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")

ggplot(SQ_E3temp, aes(RRI_anterior,dsi_pob))+
  geom_point()+
  theme_minimal()+
  geom_abline(slope=-0.02435,intercept=0.61655,colour="navyblue")+
  geom_vline(xintercept =0.85,colour="red")+
  geom_vline(xintercept =1.15,colour="red")+
  ggtitle("Malapportionment vs DSI_sec, SQ")
ggsave(filename="RRIant_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")

ggplot(SQ_E3temp, aes(RRI_nueva,dsi_pob))+
  geom_point()+theme_minimal()+
  geom_abline(slope=0.05200,intercept=0.53649,colour="navyblue")+
  geom_vline(xintercept =0.85,colour="red")+
  geom_vline(xintercept =1.15,colour="red")+
  ggtitle("Malapportionment vs DSI_sec, E3")
ggsave(filename="RRInueva_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")

ggplot(SQ_E3temp, aes(RRI_anterior, prop_bastion_prd))+
  geom_point()+theme_minimal()+
  geom_abline(slope=-0.0533,intercept=0.24901,colour="navyblue")+
  geom_vline(xintercept =0.85,colour="red")+
  geom_vline(xintercept =1.15,colour="red")+
  ggtitle("Malapportionment vs bastiones PRD, SQ")
ggsave(filename="RRIanterior_bastionesprd.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")  

ggplot(SQ_E3temp, aes(RRI_nueva, prop_bastion_prd))+
  geom_point()+theme_minimal()+
  geom_abline(slope=-0.1153,intercept=0.325,colour="navyblue")+
  geom_vline(xintercept =0.85,colour="red")+
  geom_vline(xintercept =1.15,colour="red")+
  ggtitle("Malapportionment vs bastiones PRD, E3")
ggsave(filename="RRInueva_bastionesprd.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")  

ggplot(sinQR, aes(RRI_nueva, prop_bastion_prd))+
  geom_point()+theme_minimal()+
  geom_abline(slope=0.2877,intercept=0.5003,colour="navyblue")+
  geom_vline(xintercept =0.85,colour="red")+
  geom_vline(xintercept =1.15,colour="red")+
  ggtitle("Malapportionment vs bastiones PRD (sin Quintana Roo), E3")
ggsave(filename="RRInueva_bastionesprdsinQR.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")  


ggplot(SQ_E3temp, aes(RRI_nueva, prop_rural))+
  geom_point()+theme_minimal()

ggplot(SQ_E3temp, aes(RRI_nueva, prop_urbana))+
  geom_point()+theme_minimal()

summary(lm(dsi_sec~RRI_nueva,SQ_E3temp))

plot(SQ_E3temp$RRI_anterior,SQ_E3temp$prop_bastion_pri)

ggplot(SQ_E3, aes(RRI_nueva,dsi_pob))+
  geom_point()+theme_minimal()+
  geom_abline(slope=0.05200,intercept=0.53649,colour="navyblue")+
  geom_vline(xintercept =0.85,colour="red")+
  geom_vline(xintercept =1.15,colour="red")+
  ggtitle("Malapportionment vs DSI_sec, E3")
ggsave(filename="RRInueva_dsipob.png", plot=last_plot(),device="png",path="/Volumes/DANIEL/tesis_DSI/graficas_scatter/nuevas/")

ggplot(SQ_E3)+
  geom_point(aes(RRI_anterior, dsi_sec))+
  geom_point(aes(RRI_anterior,dsi_pob),colour="grey50")+
  theme_minimal()+ylab("DSI_sec (negro), DSI_pob (gris)")+
  xlab("RRI SQ")+
  geom_abline(slope=-0.02435,intercept=0.61655,colour="black")+
  geom_abline(slope=-0.03095,intercept=0.62652,colour="grey50")+
  geom_vline(xintercept =0.85,colour="red")+
  geom_vline(xintercept =1.15,colour="red")+
  ggtitle("Malapportionment SQ vs DSI")

summary(lm(dsi_sec~RRI_anterior,SQ_E3temp))  

SQ_E3$e2_pan<-as.numeric(SQ_E3$edon==4|SQ_E3$edon==11|SQ_E3$edon==14|SQ_E3$edon==17|SQ_E3$edon==19|
                           SQ_E3$edon==21|SQ_E3$edon==26|SQ_E3$edon==31|SQ_E3$edon==32|SQ_E3$edon==12)
SQ_E3$e2_pri <- as.numeric(SQ_E3$edon==8|SQ_E3$edon==15|SQ_E3$edon==18)
SQ_E3$e2_prd <- as.numeric(SQ_E3$edon==17)
SQ_E3$bastion_pan_e2 <- SQ_E3$prop_bastion_pan*SQ_E3$e2_pan
SQ_E3$pan_e2_i<-(1-SQ_E3$e2_pan)
SQ_E3$bastion_pan_e2_ <- SQ_E3$prop_bastion_pan*SQ_E3$pan_e2_i

SQ_E3$bastion_pri_e2 <- SQ_E3$prop_bastion_pri*SQ_E3$e2_pri
SQ_E3$pri_e2_i<-(1-SQ_E3$e2_pri)
SQ_E3$bastion_pri_e2_ <- SQ_E3$prop_bastion_pri*SQ_E3$pri_e2_i

SQ_E3$bastion_prd_e2 <- SQ_E3$prop_bastion_prd*SQ_E3$e2_prd
SQ_E3$prd_e2_i<-(1-SQ_E3$e2_prd)
SQ_E3$bastion_prd_e2_ <- SQ_E3$prop_bastion_prd*SQ_E3$prd_e2_i


head(SQ_E3$prd_e2_i)

library(tidyverse)
SQ_E3 <- SQ_E3 %>% mutate(RRI_abs=abs(RRI_anterior))
SQ_E3 <- SQ_E3 %>% mutate(RRI_sq=(RRI_anterior*RRI_anterior))

summary(lm(dsi_pob~RRI_abs,SQ_E3))

model1<-lm(dsi_pob~RRI_anterior,SQ_E3)
model2<-lm(dsi_pob~RRI_abs,SQ_E3)
model3<-lm(dsi_pob~RRI_anterior+RRI_sq,SQ_E3)

library(stargazer)

stargazer(model1, model2, model3, type="text", out="/Volumes/DANIEL/tesis_DSI/RRI.txt")

mod1 <- (lm(dsi_pob~dcrit8+RRI_anterior+prop_rural+prop_bastion_pri+
             prop_bastion_pan+prop_bastion_prd+e2_pan+e2_pri+e2_prd+
             bastion_pan_e2+
             bastion_pri_e2+
             bastion_prd_e2+
             PANgob_antes+PANgob_despues+
             PRIgob_antes+PRIgob_despues+
             PRDgob_antes+PRDgob_despues+indigena,SQ_E3))
mod2 <- (lm(dsi_sec~dcrit8+RRI_anterior+prop_rural+prop_bastion_pri+
              prop_bastion_pan+prop_bastion_prd+e2_pan+e2_pri+e2_prd+
              bastion_pan_e2+
              bastion_pri_e2+
              bastion_prd_e2+
              PANgob_antes+PANgob_despues+
              PRIgob_antes+PRIgob_despues+
              PRDgob_antes+PRDgob_despues,SQ_E3))
mod3 <- (lm(dsi_pob_e~dcrit8+RRI_anterior+prop_rural+prop_bastion_pri+
              prop_bastion_pan+prop_bastion_prd+e2_pan+e2_pri+e2_prd+
              bastion_pan_e2+
              bastion_pri_e2+
              bastion_prd_e2+
              PANgob_antes+PANgob_despues+
              PRIgob_antes+PRIgob_despues+
              PRDgob_antes+PRDgob_despues,SQ_E3))
mod4 <- (lm(dsi_sec_e~dcrit8+RRI_anterior+prop_rural+prop_bastion_pri+
              prop_bastion_pan+prop_bastion_prd+e2_pan+e2_pri+e2_prd+
              bastion_pan_e2+
              bastion_pri_e2+
              bastion_prd_e2+
              PANgob_antes+PANgob_despues+
              PRIgob_antes+PRIgob_despues+
              PRDgob_antes+PRDgob_despues,SQ_E3))

mod5 <- (lm(dsi_pob~dcrit8+RRI_anterior*PANgob_antes+
              RRI_anterior*PRIgob_antes+
              prop_rural+prop_bastion_pan+prop_bastion_pri+
              e2_pan+e2_pri+indigena+RRI_sq+RRI_pos
              ,SQ_E3))
mod6 <- (lm(dsi_pob~dcrit8+RRI_anterior+PANgob_antes+
              PRIgob_antes+prop_rural+e2_pan*prop_bastion_pan+
              e2_pri*prop_bastion_pri+indigena+RRI_sq+RRI_pos,SQ_E3))
mod7 <- (lm(dsi_sec~dcrit8+RRI_anterior*PANgob_antes+
              RRI_anterior*PRIgob_antes+
              prop_rural+prop_bastion_pan+prop_bastion_pri+
              e2_pan+e2_pri+indigena+RRI_sq+RRI_pos
            ,SQ_E3))
mod8 <- (lm(dsi_sec~dcrit8+RRI_anterior+PANgob_antes+
              PRIgob_antes+prop_rural+e2_pan*prop_bastion_pan+
              e2_pri*prop_bastion_pri+indigena+RRI_sq+RRI_pos
            ,SQ_E3))

mod9 <- lm(dsi_pob~RRI_anterior+RRI_sq+
             prop_bastion_pan+prop_bastion_pri+
             RRI_anterior*prop_bastion_pan+RRI_anterior*prop_bastion_pri+
             RRI_sq*prop_bastion_pan+RRI_sq*prop_bastion_pri+
             dcrit8+prop_rural+e2_pan+e2_pri+indigena+
             PANgob_antes+PRIgob_antes,SQ_E3)
mod10 <- lm(dsi_sec~RRI_anterior+RRI_sq+
              prop_bastion_pan+prop_bastion_pri+
              RRI_anterior*prop_bastion_pan+RRI_anterior*prop_bastion_pri+
              RRI_sq*prop_bastion_pan+RRI_sq*prop_bastion_pri+
              dcrit8+prop_rural+e2_pan+e2_pri+indigena+
              PANgob_antes+PRIgob_antes,SQ_E3)
mod11 <- lm(dsi_pob~RRI_anterior+RRI_sq+
              PANgob_antes+PRIgob_antes+
              RRI_anterior*PANgob_antes+RRI_anterior*PRIgob_antes+
              RRI_sq*PANgob_antes+RRI_sq*PRIgob_antes+
              prop_bastion_pan+prop_bastion_pri+
              dcrit8+prop_rural+e2_pan+e2_pri+indigena,SQ_E3)
mod12 <- lm(dsi_sec~RRI_anterior+RRI_sq+
              PANgob_antes+PRIgob_antes+
              RRI_anterior*PANgob_antes+RRI_anterior*PRIgob_antes+
              RRI_sq*PANgob_antes+RRI_sq*PRIgob_antes+
              prop_bastion_pan+prop_bastion_pri+
              dcrit8+prop_rural+e2_pan+e2_pri+indigena,SQ_E3)
mod13 <- lm(dsi_pob~RRI_pos*PANgob_antes+RRI_pos*PRIgob_antes+
              RRI_neg*PANgob_antes+RRI_neg*PRIgob_antes+
              prop_bastion_pan+prop_bastion_pri+
              dcrit8+prop_rural+e2_pan+e2_pri+indigena,SQ_E3)
mod14<- lm(dsi_sec~RRI_pos*PANgob_antes+RRI_pos*PRIgob_antes+
             RRI_neg*PANgob_antes+RRI_neg*PRIgob_antes+
             prop_bastion_pan+prop_bastion_pri+
             dcrit8+prop_rural+e2_pan+e2_pri+indigena,SQ_E3)
mod15<- lm(dsi_pob~RRI_pos*prop_bastion_pan+RRI_pos*prop_bastion_pri+
        RRI_neg*prop_bastion_pan+RRI_neg*prop_bastion_pri+
        PANgob_antes+PRIgob_antes+
        dcrit8+prop_rural+e2_pan+e2_pri+indigena,SQ_E3)
mod16<- lm(dsi_sec~RRI_pos*prop_bastion_pan+RRI_pos*prop_bastion_pri+
             RRI_neg*prop_bastion_pan+RRI_neg*prop_bastion_pri+
             PANgob_antes+PRIgob_antes+
             dcrit8+prop_rural+e2_pan+e2_pri+indigena,SQ_E3)


mod1 <- lm(dsi_pob~RRI_anterior+
             dcrit8+prop_rural+indigena
             ,SQ_E3)

mod2 <- lm(dsi_pob~RRI_anterior+
              prop_bastion_pan+prop_bastion_pri+
              e2_pan+e2_pri+
              PANgob_antes+PRIgob_antes+dcrit8+prop_rural+indigena,SQ_E3)

mod3 <- lm(dsi_pob~RRI_anterior+
              prop_bastion_pan+prop_bastion_pri+
              e2_pan+e2_pri+
              PANgob_antes+PRIgob_antes+
              RRI_anterior*PANgob_antes+RRI_anterior*PRIgob_antes+
              dcrit8+prop_rural+indigena,SQ_E3)

mod4 <- lm(dsi_pob~RRI_anterior+
              prop_bastion_pan+prop_bastion_pri+
              e2_pan+e2_pri+
              PANgob_antes+PRIgob_antes+
              RRI_anterior*prop_bastion_pan+RRI_anterior*prop_bastion_pri+
              dcrit8+prop_rural+indigena,SQ_E3)
mod5 <- lm(dsi_pob~RRI_anterior+
             prop_bastion_pan+prop_bastion_pri+
             e2_pan+e2_pri+
             PANgob_antes+PRIgob_antes+
             RRI_anterior*prop_bastion_pan+RRI_anterior*prop_bastion_pri+
             RRI_anterior*PANgob_antes+RRI_anterior*PRIgob_antes+
             dcrit8+prop_rural+indigena,SQ_E3)


names(SQ_E3)<-

mod1 <- lm(dsi_pob~RRI_anterior+
             dcrit8+prop_rural+indigena
           ,SQ_E3)

mod2 <- lm(dsi_pob~RRI_anterior+
             prop_bastion_pan+prop_bastion_pri+
             e2_pan+e2_pri+
             PANgob_antes+PRIgob_antes+dcrit8+prop_rural+indigena,SQ_E3)

#mod3 <- lm(dsi_sec~RRI_anterior+
             prop_bastion_pan+prop_bastion_pri+
             e2_pan+e2_pri+
             PANgob_antes+PRIgob_antes+
             RRI_anterior*PANgob_antes+RRI_anterior*PRIgob_antes+
             dcrit8+prop_rural+indigena SQ_E3)

mod4 <- lm(dsi_pob~RRI_anterior+
             prop_bastion_pan+prop_bastion_pri+
             e2_pan+e2_pri+
             PANgob_antes+PRIgob_antes+
             RRI_anterior*prop_bastion_pan+RRI_anterior*prop_bastion_pri+
             dcrit8+prop_rural+indigena,SQ_E3)
mod5 <- lm(dsi_pob~RRI_anterior+
             prop_bastion_pan+prop_bastion_pri+
             e2_pan+e2_pri+
             PANgob_antes+PRIgob_antes+
             RRI_anterior*prop_bastion_pan+RRI_anterior*prop_bastion_pri+
             RRI_anterior*PANgob_antes+RRI_anterior*PRIgob_antes+
             dcrit8+prop_rural+indigena,SQ_E3)

percentiles<-quantile(SQ_E3$RRI_anterior, seq(from=0.05, to=0.95, by=0.05))
#simulaciones
tmp_j<-0
for (j in 1:19){
  data.frame(
    Intercept = 1,
    dsi_sec = median(SQ_E3$dsi_sec),
    RRI_anterior = percentiles[j], 
    prop_bastion_pan      = median(SQ_E3$prop_bastion_pan),
    prop_bastion_pri      = median(SQ_E3$prop_bastion_pri),
    e2_pan      = 0,
    e2_pri      =  1,
    PANgob_antes  = 0,
    PRIgob_antes   = 1,
    dcrit8 = 0,
    prop_rural = median(SQ_E3$prop_rural),
    indigena = 0)
  
}


pred1 <- predict(object = mod4, newdata = tmp)
pred2 <- predict(object = mod4, newdata= tmp, interval="confidence")
pred3 <- as.data.frame(pred3 <- predict.lm(object = mod4, newdata= tmp, interval="prediction",type="response"))
mydata<-cbind(SQ_E3,pred3)
p <- ggplot(SQ_E3, aes( RRI_anterior, dsi_sec)) +
  geom_point()+
  stat_smooth(method = "lm")
  
p + geom_line(mapping = aes(lwr),data=SQ_E3)
# 3. Add prediction intervals
p + geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line( aes(y=upr), color = "red", linetype = "dashed")


data("cars", package = "datasets")
model <- lm(dist ~ speed, data = cars)
# 1. Add predictions 
pred.int <- predict(model, interval = "prediction")
mydata <- cbind(cars, pred.int)
# 2. Regression line + confidence intervals
library("ggplot2")
p <- ggplot(mydata, aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")







SQ_E3 <- SQ_E3 %>% mutate(RRI_pos= case_when(RRI_anterior>1~RRI_anterior))
SQ_E3 <- SQ_E3 %>% mutate(RRI_neg= case_when(RRI_anterior<1~RRI_anterior))
SQ_E3[is.na(SQ_E3)] <- 0

summary(lm(dsi_pob~dcrit8+RRI_anterior+prop_rural+prop_bastion_pan+
             prop_bastion_prd+
             bastion_pan_e2+
             bastion_pri_e2+
             bastion_prd_e2+
             PANgob_antes+PANgob_despues+
             PRIgob_antes+PRIgob_despues+
             PRDgob_antes+PRDgob_despues,SQ_E3))




summary(lm(dsi_sec~dcrit8+RRI_anterior+prop_rural+
             e2_pan+bastion_pan_e2+
             e2_pri+bastion_pri_e2+
             e2_prd+bastion_prd_e2+bastion_prd_e2_+
             PANgober_2antes+PANgob_antes+PANgob_despues+
             PRIgober_2antes+PRIgob_antes+PRIgob_despues+
             PRDgob_antes+PRDgob_despues,SQ_E3))

summary(lm(dsi_pob~dcrit8+RRI_anterior+prop_rural+prop_urbana+
             prop_bastion_pan+e2_pan+bastion_pan_e2+
             prop_bastion_pri+e2_pri+bastion_pri_e2+
             prop_bastion_prd+e2_prd+bastion_prd_e2+bastion_prd_e2_+
             PANgober_2antes+PANgob_antes+PANgob_despues+
             PRIgober_2antes+PRIgob_antes+PRIgob_despues+
             PRDgob_antes+PRDgob_despues,SQ_E3))

modelo1 <- lm(dsi_pob~dcrit8+RRI_anterior+prop_rural+
                e2_pan+e2_pri+prop_bastion_pan+prop_bastion_pri+PANgob_antes+PRIgob_antes
              ,SQ_E3)
modelo2 <- lm(dsi_sec~dcrit8+RRI_anterior+prop_rural+
                e2_pan+e2_pri+prop_bastion_pan+prop_bastion_pri+PANgob_antes+PRIgob_antes
              ,SQ_E3)
modelo3 <- lm(dsi_sec~dcrit8+RRI_anterior+prop_rural+
                e2_pan+e2_pri+e2_prd+prop_bastion_prd+prop_bastion_pan+prop_bastion_pri+PANgob_antes+PRIgob_antes+PRDgob_antes
              ,SQ_E3)
summary(modelo1)
#cpspeche
esc_loc_pob <- esc_loc_pob %>% mutate(indigena = case_when(NOMBRE.MUNICIPIO=="CALKINI"~1,
                                                           NOMBRE.MUNICIPIO=="HOPELCHEN"~1,
                                                           NOMBRE.MUNICIPIO=="HECELCHAKAN"~1
                                                          ))
esc_loc_pob[is.na(esc_loc_pob)] <- 0
#Chiapas
esc_loc_pob <- esc_loc_pob %>% mutate(indigena = case_when(NOMBRE.MUNICIPIO=="YAJALON"~1,
                                          NOMBRE.MUNICIPIO=="CHILON"~1,
                                          NOMBRE.MUNICIPIO=="SAN JUAN CANCUC"~1,
                                          NOMBRE.MUNICIPIO=="SITALA"~1,
                                          NOMBRE.MUNICIPIO=="SAN CRISTOBAL DE LAS CASAS"~1,
                                          NOMBRE.MUNICIPIO=="OCOSINGO"~1,
                                          NOMBRE.MUNICIPIO=="HUITUIPAN"~1,
                                          NOMBRE.MUNICIPIO=="SABANILLA"~1,
                                          NOMBRE.MUNICIPIO=="SAN ANDRES DURAZNAL"~1,
                                          NOMBRE.MUNICIPIO=="SIMOJOVEL"~1,
                                          NOMBRE.MUNICIPIO=="TILA"~1,
                                          NOMBRE.MUNICIPIO=="TUMBALA"~1,
                                          NOMBRE.MUNICIPIO=="CATAZAJA"~1,
                                          NOMBRE.MUNICIPIO=="LA LIBERTAD"~1,
                                          NOMBRE.MUNICIPIO=="PALENQUE"~1,
                                          NOMBRE.MUNICIPIO=="SALTO DE AGUA"~1,
                                          NOMBRE.MUNICIPIO=="AMATAN"~1,
                                          NOMBRE.MUNICIPIO=="BOCHIL"~1,
                                          NOMBRE.MUNICIPIO=="CHAPULTENANGO"~1,
                                          NOMBRE.MUNICIPIO=="EL BOSQUE"~1,
                                          NOMBRE.MUNICIPIO=="FRANCISCO LEON"~1,
                                          NOMBRE.MUNICIPIO=="IXHUATAN"~1,
                                          NOMBRE.MUNICIPIO=="JITOTOL"~1,
                                          NOMBRE.MUNICIPIO=="OCOTEPEC"~1,
                                          NOMBRE.MUNICIPIO=="PANTEPEC"~1,
                                          NOMBRE.MUNICIPIO=="PUEBLO NUEVO SOLISTAHUACAN"~1,
                                          NOMBRE.MUNICIPIO=="RAYON"~1,
                                          NOMBRE.MUNICIPIO=="SOYALO"~1,
                                          NOMBRE.MUNICIPIO=="TAPALAPA"~1,
                                          NOMBRE.MUNICIPIO=="TAPILULA"~1,
                                          NOMBRE.MUNICIPIO=="ALTAMIRANO"~1,
                                          NOMBRE.MUNICIPIO=="AMATENANGO DEL VALLE"~1,
                                          NOMBRE.MUNICIPIO=="BENEMERITO DE LAS AMERICAS"~1,
                                          NOMBRE.MUNICIPIO=="CHANAL"~1,
                                          NOMBRE.MUNICIPIO=="LAS MARGARITAS"~1,
                                          NOMBRE.MUNICIPIO=="MARAVILLA TENEJAPA"~1,
                                          NOMBRE.MUNICIPIO=="MARQUES DE COMILLAS"~1,
                                          NOMBRE.MUNICIPIO=="OCOSINGO"~1,
                                          NOMBRE.MUNICIPIO=="ALDAMA"~1,
                                          NOMBRE.MUNICIPIO=="CHALCHIHUITLAN"~1,
                                          NOMBRE.MUNICIPIO=="CHENALHO"~1,
                                          NOMBRE.MUNICIPIO=="LARRAINZAR"~1,
                                          NOMBRE.MUNICIPIO=="MITONTIC"~1,
                                          NOMBRE.MUNICIPIO=="OXCHUC"~1,
                                          NOMBRE.MUNICIPIO=="PANTELHO"~1,
                                          NOMBRE.MUNICIPIO=="SANTIAGO EL PINAR"~1,
                                          NOMBRE.MUNICIPIO=="TENEJAPA"~1,
                                          NOMBRE.MUNICIPIO=="CHAMULA"~1,
                                          NOMBRE.MUNICIPIO=="HUIXTAN"~1,
                                          NOMBRE.MUNICIPIO=="IXTAPA"~1,
                                          NOMBRE.MUNICIPIO=="SAN CRISTOBAL DE LAS CASAS"~1,
                                          NOMBRE.MUNICIPIO=="TEOPISCA"~1,
                                          NOMBRE.MUNICIPIO=="ZINACANTAN"~1))
esc_loc_pob[is.na(esc_loc_pob)] <- 0                                          
                                          
#Chihuahua
esc_loc_pob <- esc_loc_pob %>% mutate(indigena = case_when(NOMBRE.MUNICIPIO=="BALLEZA"~1,
                                                           NOMBRE.MUNICIPIO=="BATOPILAS"~1,
                                                           NOMBRE.MUNICIPIO=="CARICHI"~1,
                                                           NOMBRE.MUNICIPIO=="GUACHOCHI"~1,
                                                           NOMBRE.MUNICIPIO=="GUADALUPE Y CALVO"~1,
                                                           NOMBRE.MUNICIPIO=="MORELOS"~1,
                                                           NOMBRE.MUNICIPIO=="URIQUE"~1))
esc_loc_pob[is.na(esc_loc_pob)] <- 0

#Guerrero
esc_loc_pob <- esc_loc_pob %>% mutate(indigena = case_when(NOMBRE.MUNICIPIO=="MARTIR DE CUILAPAN"~1,
                                                           NOMBRE.MUNICIPIO=="MOCHITLAN"~1,
                                                           NOMBRE.MUNICIPIO=="QUECHULTENANGO"~1,
                                                           NOMBRE.MUNICIPIO=="TIXTLA DE GUERRERO"~1,
                                                           NOMBRE.MUNICIPIO=="ZITLALA"~1,
                                                           NOMBRE.MUNICIPIO=="CHILAPA DE ALVAREZ"~1,
                                                           NOMBRE.MUNICIPIO=="JOSE JOAQUIN DE HERRERA"~1,
                                                           NOMBRE.MUNICIPIO=="ACATEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="AHUACUOTZINGO"~1,
                                                           NOMBRE.MUNICIPIO=="ATLIXTAC"~1,
                                                           NOMBRE.MUNICIPIO=="COPANATOYAC"~1,
                                                           NOMBRE.MUNICIPIO=="TLACOAPA"~1,
                                                           NOMBRE.MUNICIPIO=="ZAPOTITLAN TABLAS"~1,
                                                           NOMBRE.MUNICIPIO=="ALPOYECA"~1,
                                                           NOMBRE.MUNICIPIO=="CUALAC"~1,
                                                           NOMBRE.MUNICIPIO=="HUAMUXTITLAN"~1,
                                                           NOMBRE.MUNICIPIO=="OLINALA"~1,
                                                           NOMBRE.MUNICIPIO=="TLALIXTAQUILLA DE MALDONADO"~1,
                                                           NOMBRE.MUNICIPIO=="TLAPA DE COMONFORT"~1,
                                                           NOMBRE.MUNICIPIO=="XOCHIHUEHUETLAN"~1,
                                                           NOMBRE.MUNICIPIO=="ALCOZAUCA DE GUERRERO"~1,
                                                           NOMBRE.MUNICIPIO=="ATLAMAJALCINGO DEL MONTE"~1,
                                                           NOMBRE.MUNICIPIO=="COCHOAPA EL GRANDE"~1,
                                                           NOMBRE.MUNICIPIO=="ILIATENCO"~1,
                                                           NOMBRE.MUNICIPIO=="MALINALTEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="METLATONOC"~1,
                                                           NOMBRE.MUNICIPIO=="SAN LUIS ACATLAN"~1,
                                                           NOMBRE.MUNICIPIO=="TLAPA DE COMONFORT"~1,
                                                           NOMBRE.MUNICIPIO=="XALPATLAHUAC"~1))
esc_loc_pob[is.na(esc_loc_pob)] <- 0

#HIDALGO
esc_loc_pob <- esc_loc_pob %>% mutate(indigena = case_when(NOMBRE.MUNICIPIO=="HUAUTLA"~1,
                                                           NOMBRE.MUNICIPIO=="HUEJUTLA DE REYES"~1,
                                                           NOMBRE.MUNICIPIO=="JALTOCAN"~1,
                                                           NOMBRE.MUNICIPIO=="CARDONAL"~1,
                                                           NOMBRE.MUNICIPIO=="CHILCUAUTLA"~1,
                                                           NOMBRE.MUNICIPIO=="IXMIQUILPAN"~1,
                                                           NOMBRE.MUNICIPIO=="NICOLAS FLORES"~1,
                                                           NOMBRE.MUNICIPIO=="SANTIAGO DE ANAYA"~1))
esc_loc_pob[is.na(esc_loc_pob)] <- 0

#Mexico
esc_loc_pob <- esc_loc_pob %>% mutate(indigena = case_when(NOMBRE.MUNICIPIO=="IXTLAHUACA"~1,
                                                           NOMBRE.MUNICIPIO=="JIQUIPILCO"~1,
                                                           NOMBRE.MUNICIPIO=="SAN FELIPE DEL PROGRESO"~1))
esc_loc_pob[is.na(esc_loc_pob)] <- 0

#Michoacan
esc_loc_pob <- esc_loc_pob %>% mutate(indigena = case_when(NOMBRE.MUNICIPIO=="CHARAPAN"~1,
                                                           NOMBRE.MUNICIPIO=="CHERAN"~1,
                                                           NOMBRE.MUNICIPIO=="CHILCHOTA"~1,
                                                           NOMBRE.MUNICIPIO=="COENEO"~1,
                                                           NOMBRE.MUNICIPIO=="ERONGARICUARO"~1,
                                                           NOMBRE.MUNICIPIO=="NAHUATZEN"~1,
                                                           NOMBRE.MUNICIPIO=="PARACHO"~1,
                                                           NOMBRE.MUNICIPIO=="QUIROGA"~1))
esc_loc_pob[is.na(esc_loc_pob)] <- 0

#Nayarit
esc_loc_pob <- esc_loc_pob %>% mutate(indigena = case_when(NOMBRE.MUNICIPIO=="DEL NAYAR"~1,
                                                           NOMBRE.MUNICIPIO=="RUIZ"~1))
esc_loc_pob[is.na(esc_loc_pob)] <- 0

#cpsaca
esc_loc_pob <- esc_loc_pob %>% mutate(indigena = case_when(NOMBRE.MUNICIPIO=="ACATLAN DE PEREZ FIGUEROA"~1,
                                                           NOMBRE.MUNICIPIO=="COSOLAPA"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JOSE INDEPENDENCIA"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JOSE TENANGO"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JUAN BAUTISTA TUXTEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="SAN MIGUEL SOYALTEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="SAN PEDRO IXCATLAN"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA CHILCHOTLA"~1,
                                                           NOMBRE.MUNICIPIO=="AYOTZINTEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="LOMA BONITA"~1,
                                                           NOMBRE.MUNICIPIO=="SAN FELIPE JALAPA DE DIAZ"~1,
                                                           NOMBRE.MUNICIPIO=="SAN FELIPE USILA"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JOSE CHILTEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JUAN BAUTISTA TLACOATZINTEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JUAN BAUTISTA TUXTEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="SAN LUCAS OJITLAN"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA JACATEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="SANTIAGO JOCOTEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="CONCEPCION PAPALO"~1,
                                                           NOMBRE.MUNICIPIO=="CUYAMECALCO VILLA DE ZARAGOZA"~1,
                                                           NOMBRE.MUNICIPIO=="ELOXOCHITLAN DE FLORES MAGON"~1,
                                                           NOMBRE.MUNICIPIO=="HUAUTEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="HUAUTLA DE JIMENEZ"~1,
                                                           NOMBRE.MUNICIPIO=="MAZATLAN VILLA DE FLORES"~1,
                                                           NOMBRE.MUNICIPIO=="SAN ANDRES TEOTILALPAM"~1,
                                                           NOMBRE.MUNICIPIO=="SAN ANTONIO NANAHUATIPAM"~1,
                                                           NOMBRE.MUNICIPIO=="SAN BARTOLOME AYAUTLA"~1,
                                                           NOMBRE.MUNICIPIO=="SAN FRANCISCO HUEHUETLAN"~1,
                                                           NOMBRE.MUNICIPIO=="SAN FRANCISCO CHAPULAPA"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JERONIMO TECOATL"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JUAN BAUTISTA CUICATLAN"~1,
                                                           NOMBRE.MUNICIPIO=="CHIQUIHUITLAN DE BENITO JUAREZ"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JUAN COATZOSPAM"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JUAN DE LOS CUES"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JUAN TEPEUXILA"~1,
                                                           NOMBRE.MUNICIPIO=="SAN LORENZO CUAUNECUILTITLA"~1,
                                                           NOMBRE.MUNICIPIO=="SAN LUCAS ZOQUIAPAM"~1,
                                                           NOMBRE.MUNICIPIO=="SAN MARTIN TOXPALAN"~1,
                                                           NOMBRE.MUNICIPIO=="SAN MATEO YOLOXOCHITLAN"~1,
                                                           NOMBRE.MUNICIPIO=="SAN MIGUEL SANTA FLOR"~1,
                                                           NOMBRE.MUNICIPIO=="SAN PEDRO JALTEPETONGO"~1,
                                                           NOMBRE.MUNICIPIO=="SAN PEDRO JOCOTIPAC"~1,
                                                           NOMBRE.MUNICIPIO=="SAN PEDRO OCOPETATILLO"~1,
                                                           NOMBRE.MUNICIPIO=="SAN PEDRO SOCHIAPAM"~1,
                                                           NOMBRE.MUNICIPIO=="SAN PEDRO TEUTILA"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA ANA ATEIXTLAHUACA"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA ANA CUAHTEMOC"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA CRUZ ACATEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA LA ASUNCION"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA IXCATLAN"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA PAPALO"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA TECOMAVACA"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA TEOPOXCO"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA TEXCATITLAN"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA TLALIXTAC"~1,
                                                           NOMBRE.MUNICIPIO=="SANTIAGO TEXCALCINGO"~1,
                                                           NOMBRE.MUNICIPIO=="SANTOS REYES PAPALO"~1,
                                                           NOMBRE.MUNICIPIO=="TEOTITLAN DE FLORES MAGON"~1,
                                                           NOMBRE.MUNICIPIO=="VALERIO TRUJANO"~1,
                                                           NOMBRE.MUNICIPIO=="COICOYAN DE LAS FLORES"~1,
                                                           NOMBRE.MUNICIPIO=="CONSTANCUA DEL ROSARIO"~1,
                                                           NOMBRE.MUNICIPIO=="MESONES HIDALGO"~1,
                                                           NOMBRE.MUNICIPIO=="PUTLA VILLA DE GUERRERO"~1,
                                                           NOMBRE.MUNICIPIO=="LA REFORMA"~1,
                                                           NOMBRE.MUNICIPIO=="SAN ANDRES CABECERA NUEVA"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JUAN CACAHUATEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="SAN JUAN MIXTEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="SAN MARTIN ITUNYOSO"~1,
                                                           NOMBRE.MUNICIPIO=="SAN MARTIN PERAS"~1,
                                                           NOMBRE.MUNICIPIO=="SAN PEDRO AMUZGOS"~1,
                                                           NOMBRE.MUNICIPIO=="SAN SEBASTIAN TECOMAXTLAHUACA"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA CRUZ ITUNDUJIA"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA IPALAPA"~1,
                                                           NOMBRE.MUNICIPIO=="SANTA MARIA ZACATEPEC"~1,
                                                           NOMBRE.MUNICIPIO=="SANTIAGO JUXTLAHUACA"~1,))
esc_loc_pob[is.na(esc_loc_pob)] <- 0

#Puebla

#Quintana Roo

#Yucatán

#San Luis Potosí

#Veracruz

SQ_E3$indigena <- as.numeric(SQ_E3$edon==4 & SQ_E3$esc3==1|
                               SQ_E3$edon==4 & SQ_E3$esc3==2|
                               SQ_E3$edon==4 & SQ_E3$esc3==3|
                               SQ_E3$edon==7 & SQ_E3$esc3==2|
                               SQ_E3$edon==7 & SQ_E3$esc3==1|
                               SQ_E3$edon==7 & SQ_E3$esc3==3|
                               SQ_E3$edon==7 & SQ_E3$esc3==4|
                               SQ_E3$edon==7 & SQ_E3$esc3==20|
                               SQ_E3$edon==7 & SQ_E3$esc3==11|
                               SQ_E3$edon==7 & SQ_E3$esc3==23|
                               SQ_E3$edon==7 & SQ_E3$esc3==10|
                               SQ_E3$edon==7 & SQ_E3$esc3==5|
                               SQ_E3$edon==8 & SQ_E3$esc3==6|
                               SQ_E3$edon==12 & SQ_E3$esc3==19|
                               SQ_E3$edon==12 & SQ_E3$esc3==12|
                               SQ_E3$edon==12 & SQ_E3$esc3==1|
                               SQ_E3$edon==12 & SQ_E3$esc3==15|
                               SQ_E3$edon==12 & SQ_E3$esc3==14|
                               SQ_E3$edon==13 & SQ_E3$esc3==14|
                               SQ_E3$edon==13 & SQ_E3$esc3==4|
                               SQ_E3$edon==13 & SQ_E3$esc3==16|
                               SQ_E3$edon==15 & SQ_E3$esc3==8|
                               SQ_E3$edon==16 & SQ_E3$esc3==4|
                               SQ_E3$edon==18 & SQ_E3$esc3==12|
                               SQ_E3$edon==20 & SQ_E3$esc3==2|
                               SQ_E3$edon==20 & SQ_E3$esc3==9|
                               SQ_E3$edon==20 & SQ_E3$esc3==15|
                               SQ_E3$edon==20 & SQ_E3$esc3==10|
                               SQ_E3$edon==20 & SQ_E3$esc3==1|
                               SQ_E3$edon==20 & SQ_E3$esc3==4|
                               SQ_E3$edon==20 & SQ_E3$esc3==20|
                               SQ_E3$edon==20 & SQ_E3$esc3==8|
                               SQ_E3$edon==20 & SQ_E3$esc3==6|
                               SQ_E3$edon==20 & SQ_E3$esc3==17|
                               SQ_E3$edon==20 & SQ_E3$esc3==13|
                               SQ_E3$edon==20 & SQ_E3$esc3==14|
                               SQ_E3$edon==21 & SQ_E3$esc3==14|
                               SQ_E3$edon==21 & SQ_E3$esc3==6|
                               SQ_E3$edon==21 & SQ_E3$esc3==18|
                               SQ_E3$edon==21 & SQ_E3$esc3==2|
                               SQ_E3$edon==24 & SQ_E3$esc3==14|
                               SQ_E3$edon==24 & SQ_E3$esc3==4|
                               SQ_E3$edon==24 & SQ_E3$esc3==7|
                               SQ_E3$edon==23 & SQ_E3$esc3==12|
                               SQ_E3$edon==23 & SQ_E3$esc3==15|
                               SQ_E3$edon==31 & SQ_E3$esc3==1|
                               SQ_E3$edon==31 & SQ_E3$esc3==4|
                               SQ_E3$edon==31 & SQ_E3$esc3==6|
                               SQ_E3$edon==31 & SQ_E3$esc3==8|
                               SQ_E3$edon==31 & SQ_E3$esc3==3|
                               SQ_E3$edon==31 & SQ_E3$esc3==9|
                               SQ_E3$edon==31 & SQ_E3$esc3==7|
                               SQ_E3$edon==31 & SQ_E3$esc3==5|
                               SQ_E3$edon==30 & SQ_E3$esc3==18|
                               SQ_E3$edon==30 & SQ_E3$esc3==19|
                               SQ_E3$edon==30 & SQ_E3$esc3==9)
