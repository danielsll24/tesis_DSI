
aa <- "/Volumes/DANIEL/tesis_DSI/archivos/dsi_SQ_E3_"
ags <- read.csv(paste(aa,"ags.csv",sep=""))[-3]
bc <- read.csv(paste(aa,"bc.csv",sep=""))[-3] 
bcs <- read.csv(paste(aa,"bcs.csv",sep=""))[-3] 
cam <- read.csv(paste(aa,"cam.csv",sep=""))[-3] 
coa <- read.csv(paste(aa,"coa.csv",sep=""))[-3] 
col <- read.csv(paste(aa,"col.csv",sep=""))[-3] 
cps <- read.csv(paste(aa,"cps.csv",sep=""))[-3] 
cua <- read.csv(paste(aa,"cua.csv",sep=""))[-3] 
df <- read.csv(paste(aa,"df.csv",sep=""))[-3] 
dgo <- read.csv(paste(aa,"dgo.csv",sep=""))[-3] 
gua <- read.csv(paste(aa,"gua.csv",sep=""))[-3] 
gue <- read.csv(paste(aa,"gue.csv",sep=""))[-3]
hgo <- read.csv(paste(aa,"hgo.csv",sep=""))[-3] #
jal <- read.csv(paste(aa,"jal.csv",sep=""))[-3] 
mex <- read.csv(paste(aa,"mex.csv",sep=""))[-3] 
mic <- read.csv(paste(aa,"mic.csv",sep=""))[-3] 
mor <- read.csv(paste(aa,"mor.csv",sep=""))[-3] #
nay <- read.csv(paste(aa,"nay.csv",sep=""))[-3] 
nl <- read.csv(paste(aa,"nl.csv",sep=""))[-3] 
oax <- read.csv(paste(aa,"oax.csv",sep="")) #
pue <- read.csv(paste(aa,"pue.csv",sep=""))[-3] 
que <- read.csv(paste(aa,"que.csv",sep=""))[-3] 
qui <- read.csv(paste(aa,"qui.csv",sep="")) 
san <- read.csv(paste(aa,"san.csv",sep=""))[-3] 
sin <- read.csv(paste(aa,"sin.csv",sep="")) 
son <- read.csv(paste(aa,"son.csv",sep="")) 
tab <- read.csv(paste(aa,"tab.csv",sep=""))[-3]
tam <- read.csv(paste(aa,"tam.csv",sep="")) 
tla <- read.csv(paste(aa,"tla.csv",sep=""))[-3] 
ver <- read.csv(paste(aa,"ver.csv",sep=""))[-3] 
yuc <- read.csv(paste(aa,"yuc.csv",sep="")) 
zac <- read.csv(paste(aa,"zac.csv",sep=""))[-3] 

nombres <- c("seccion","munn","edon","dis_nueva","dis_anterior","pob","esc1","esc3","urbana","rural","mixta",
             "SQ","dsi_pob","prop_urbana","prop_rural","prop_mixta","SQ_","dsi_sec")


names(ags) <- nombres
names(bc) <- nombres
names(bcs) <- nombres
names(cam) <- nombres
names(coa) <- nombres
names(col) <- nombres
names(cps) <- nombres
names(cua) <- nombres
names(df) <- nombres
names(dgo) <- nombres
names(gua) <- nombres
names(gue) <- nombres
names(hgo) <- nombres
names(jal) <- nombres
names(mex) <- nombres
names(mic) <- nombres
names(mor) <- nombres
names(nay) <- nombres
names(nl) <- nombres
names(oax) <- nombres
names(pue) <- nombres
names(que) <- nombres
names(qui) <- nombres
names(san) <- nombres
names(sin) <- nombres
names(son) <- nombres
names(tab) <- nombres
names(tam) <- nombres
names(tla) <- nombres
names(ver) <- nombres
names(yuc) <- nombres
names(zac) <- nombres


temp <- union(ags, bc)
temp <- union(temp, bcs)
temp <- union(temp, cam)
temp <- union(temp, coa)
temp <- union(temp, col)
temp <- union(temp, cps)
temp <- union(temp, cua)
temp <- union(temp, df)
temp <- union(temp, dgo)
temp <- union(temp, gua)
temp <- union(temp, gue)
temp <- union(temp, hgo)
temp <- union(temp, jal)
temp <- union(temp, mex)
temp <- union(temp, mic)
temp <- union(temp, mor)
temp <- union(temp, nay)
temp <- union(temp, nl)
temp <- union(temp, oax)
temp <- union(temp, pue)
temp <- union(temp, que)
temp <- union(temp, qui)
temp <- union(temp, san)
temp <- union(temp, sin)
temp <- union(temp, son)
temp <- union(temp, tab)
temp <- union(temp, tam)
temp <- union(temp, tla)
temp <- union(temp, ver)
temp <- union(temp, yuc)
temp <- union(temp, zac)

SQ_E1<-temp
write.csv(SQ_E1, "/Volumes/DANIEL/tesis_DSI/archivos/base_SQ_E1.csv")

SQ_E3<-temp
write.csv(SQ_E3, "/Volumes/DANIEL/tesis_DSI/archivos/base_SQ_E3.csv")

E1_E3 <- temp
write.csv(E1_E3, "/Volumes/DANIEL/tesis_DSI/archivos/base_E1_E3.csv")

aa <- "/Volumes/DANIEL/tesis_DSI/archivos/dsi_E1_E3_"
ags <- read.csv(paste(aa,"ags.csv",sep=""))[-3]
bc <- read.csv(paste(aa,"bc.csv",sep=""))[-3] 
bcs <- read.csv(paste(aa,"bcs.csv",sep=""))[-3] 
cam <- read.csv(paste(aa,"cam.csv",sep=""))[-3] 
coa <- read.csv(paste(aa,"coa.csv",sep=""))[-3] 
col <- read.csv(paste(aa,"col.csv",sep=""))[-3] 
cps <- read.csv(paste(aa,"cps.csv",sep=""))[-3] 
cua <- read.csv(paste(aa,"cua.csv",sep=""))[-3] 
df <- read.csv(paste(aa,"df.csv",sep=""))[-3] 
dgo <- read.csv(paste(aa,"dgo.csv",sep=""))[-3] 
gua <- read.csv(paste(aa,"gua.csv",sep=""))[-3] 
gue <- read.csv(paste(aa,"gue.csv",sep=""))[-3]
hgo <- read.csv(paste(aa,"hgo2.csv",sep=""))[-3] #
jal <- read.csv(paste(aa,"jal.csv",sep=""))[-3] 
mex <- read.csv(paste(aa,"mex.csv",sep=""))[-3] 
mic <- read.csv(paste(aa,"mic.csv",sep=""))[-3] 
mor <- read.csv(paste(aa,"mor2.csv",sep=""))[-3] #
nay <- read.csv(paste(aa,"nay.csv",sep=""))[-3] 
nl <- read.csv(paste(aa,"nl.csv",sep=""))[-3] 
oax <- read.csv(paste(aa,"oax3.csv",sep="")) #
pue <- read.csv(paste(aa,"pue.csv",sep=""))[-3] 
que <- read.csv(paste(aa,"que.csv",sep=""))[-3] 
qui <- read.csv(paste(aa,"qui.csv",sep="")) 
san <- read.csv(paste(aa,"san.csv",sep=""))[-3] 
sin <- read.csv(paste(aa,"sin.csv",sep="")) 
son <- read.csv(paste(aa,"son2.csv",sep="")) 
tab <- read.csv(paste(aa,"tab.csv",sep=""))[-3]
tam <- read.csv(paste(aa,"tam.csv",sep="")) 
tla <- read.csv(paste(aa,"tla.csv",sep=""))[-3] 
ver <- read.csv(paste(aa,"ver.csv",sep=""))[-3] 
yuc <- read.csv(paste(aa,"yuc.csv",sep="")) 
zac <- read.csv(paste(aa,"zac.csv",sep=""))[-3] 


#pegar a cada uno el criterio 8, el año de redistritación, el partido en gobierno y en legislatura 
#Año en que redistritaron los estados 
SQ_E1$dcrit8 <- as.numeric(SQ_E1$edon==5|SQ_E1$edon==9|SQ_E1$edon==12|SQ_E1$edon==24|SQ_E1$edon==29|
                             SQ_E1$edon==31|SQ_E1$edon==32)
SQ_E3$dcrit8<- as.numeric(SQ_E3$edon==5|SQ_E3$edon==9|SQ_E3$edon==12|SQ_E3$edon==24|SQ_E3$edon==29|
                            SQ_E3$edon==31|SQ_E3$edon==32)
E1_E3$dcrit8<- as.numeric(E1_E3$edon==5|E1_E3$edon==9|E1_E3$edon==12|E1_E3$edon==24|E1_E3$edon==29|
                                         E1_E3$edon==31|E1_E3$edon==32)

SQ_E1$dist2015 <- as.numeric(SQ_E1$edon==1 | SQ_E1$edon==2|SQ_E1$edon==5|
                               SQ_E1$edon==8|SQ_E1$edon==10|SQ_E1$edon==13|
                               SQ_E1$edon==20|SQ_E1$edon==21|SQ_E1$edon==23|
                               SQ_E1$edon==18|SQ_E1$edon==25|SQ_E1$edon==28|
                               SQ_E1$edon==29|SQ_E1$edon==30|SQ_E1$edon==32)

SQ_E3$dist2015 <- as.numeric(SQ_E3$edon==1 | SQ_E3$edon==2|SQ_E3$edon==5|
                               SQ_E3$edon==8|SQ_E3$edon==10|SQ_E3$edon==13|
                               SQ_E3$edon==20|SQ_E3$edon==21|SQ_E3$edon==23|
                               SQ_E3$edon==18|SQ_E3$edon==25|SQ_E3$edon==28|
                               SQ_E3$edon==29|SQ_E3$edon==30|SQ_E3$edon==32)

E1_E3$dist2015 <- as.numeric(E1_E3$edon==1 | E1_E3$edon==2|E1_E3$edon==5|
                               E1_E3$edon==8|E1_E3$edon==10|E1_E3$edon==13|
                               E1_E3$edon==20|E1_E3$edon==21|E1_E3$edon==23|
                               E1_E3$edon==18|E1_E3$edon==25|E1_E3$edon==28|
                               E1_E3$edon==29|E1_E3$edon==30|E1_E3$edon==32)

SQ_E1$dist2017 <- as.numeric(SQ_E1$edon==3|SQ_E1$edon==4|SQ_E1$edon==6|SQ_E1$edon==7|
                               SQ_E1$edon==9|SQ_E1$edon==11|SQ_E1$edon==12|SQ_E1$edon==14|
                               SQ_E1$edon==15|SQ_E1$edon==16|SQ_E1$edon==17|SQ_E1$edon==19|
                               SQ_E1$edon==22|SQ_E1$edon==24|SQ_E1$edon==26|SQ_E1$edon==27|
                               SQ_E1$edon==31)
E1_E3$dist2017<- as.numeric(E1_E3$edon==3|E1_E3$edon==4| E1_E3$edon==6| E1_E3$edon==7|
                              E1_E3$edon==9| E1_E3$edon==11|E1_E3$edon==12|E1_E3$edon==14|
                              E1_E3$edon==15|E1_E3$edon==16|E1_E3$edon==17|E1_E3$edon==19|
                              E1_E3$edon==22|E1_E3$edon==24|E1_E3$edon==26|E1_E3$edon==27|
                              E1_E3$edon==31)
SQ_E3$dist2017 <- as.numeric(SQ_E3$edon==3|SQ_E3$edon==4|SQ_E3$edon==6|SQ_E3$edon==7|
                               SQ_E3$edon==9| SQ_E3$edon==11|SQ_E3$edon==12|SQ_E3$edon==14|
                               SQ_E3$edon==15|SQ_E3$edon==16|SQ_E3$edon==17|SQ_E3$edon==19|
                               SQ_E3$edon==22|SQ_E3$edon==24|SQ_E3$edon==26|SQ_E3$edon==27|
                               SQ_E3$edon==31)

#Primera elección con el nuevo mapa. Repetir para SQ_E3 y E1_E3
SQ_E1$elec_cong_2016 <- as.numeric(SQ_E1$edon==1|
                                     SQ_E1$edon==8|SQ_E1$edon==10|SQ_E1$edon==13|
                                     SQ_E1$edon==20|SQ_E1$edon==21|SQ_E1$edon==23|
                                     SQ_E1$edon==25|SQ_E1$edon==28|
                                     SQ_E1$edon==29|SQ_E1$edon==30|SQ_E1$edon==32)
SQ_E1$elec_cong_2017 <- as.numeric(SQ_E1$edon==5|SQ_E1$edon==18)
SQ_E1$elec_cong_2018 <- as.numeric(SQ_E1$edon==3|SQ_E1$edon==4|SQ_E1$edon==6|SQ_E1$edon==7|
                                     SQ_E1$edon==9|SQ_E1$edon==11|SQ_E1$edon==12|SQ_E1$edon==14|
                                     SQ_E1$edon==15|SQ_E1$edon==16|SQ_E1$edon==17|SQ_E1$edon==19|
                                     SQ_E1$edon==22|SQ_E1$edon==24|SQ_E1$edon==26|SQ_E1$edon==27|
                                     SQ_E1$edon==31)
SQ_E1$elec_cong_2019 <- as.numeric(SQ_E1$edon==2)


SQ_E3$elec_cong_2016 <- as.numeric(SQ_E3$edon==1|
                                     SQ_E3$edon==8| SQ_E3$edon==10|SQ_E3$edon==13|
                                     SQ_E3$edon==20|SQ_E3$edon==21|SQ_E3$edon==23|
                                     SQ_E3$edon==25|SQ_E3$edon==28|
                                     SQ_E3$edon==29|SQ_E3$edon==30|SQ_E3$edon==32)
SQ_E3$elec_cong_2017 <- as.numeric(SQ_E3$edon==5| SQ_E3$edon==18)
SQ_E3$elec_cong_2018 <- as.numeric(SQ_E3$edon==3| SQ_E3$edon==4|SQ_E3$edon==6|SQ_E3$edon==7|
                                     SQ_E3$edon==9| SQ_E3$edon==11|SQ_E3$edon==12|SQ_E3$edon==14|
                                     SQ_E3$edon==15|SQ_E3$edon==16|SQ_E3$edon==17|SQ_E3$edon==19|
                                     SQ_E3$edon==22|SQ_E3$edon==24|SQ_E3$edon==26|SQ_E3$edon==27|
                                     SQ_E3$edon==31)
SQ_E3$elec_cong_2019 <- as.numeric(SQ_E3$edon==2)


E1_E3$elec_cong_2016 <- as.numeric(E1_E3$edon==1|
                                     E1_E3$edon==8| E1_E3$edon==10|E1_E3$edon==13|
                                     E1_E3$edon==20|E1_E3$edon==21|E1_E3$edon==23|
                                     E1_E3$edon==25|E1_E3$edon==28|
                                     E1_E3$edon==29|E1_E3$edon==30|E1_E3$edon==32)
E1_E3$elec_cong_2017 <- as.numeric(E1_E3$edon==5| E1_E3$edon==18)
E1_E3$elec_cong_2018 <- as.numeric(E1_E3$edon==3| E1_E3$edon==4| E1_E3$edon==6| E1_E3$edon==7|
                                     E1_E3$edon==9| E1_E3$edon==11|E1_E3$edon==12|E1_E3$edon==14|
                                     E1_E3$edon==15|E1_E3$edon==16|E1_E3$edon==17|E1_E3$edon==19|
                                     E1_E3$edon==22|E1_E3$edon==24|E1_E3$edon==26|E1_E3$edon==27|
                                     E1_E3$edon==31)
E1_E3$elec_cong_2019 <- as.numeric(E1_E3$edon==2)
#partido del gobernador antes de la primera elección local con nuevo mapa. Repetir para SQ_E3 y E1_E3
SQ_E1$PANgob_antes <- as.numeric(SQ_E1$edon==2|SQ_E1$edon==3|SQ_E1$edon==11|
                                   SQ_E1$edon==21|SQ_E1$edon==22|SQ_E1$edon==25)
SQ_E1$PRIgob_antes <- as.numeric(SQ_E1$edon==1|SQ_E1$edon==4|SQ_E1$edon==5|
                                   SQ_E1$edon==6|SQ_E1$edon==8|SQ_E1$edon==10|SQ_E1$edon==12|
                                   SQ_E1$edon==13|SQ_E1$edon==14|SQ_E1$edon==15|SQ_E1$edon==18|
                                   SQ_E1$edon==23|SQ_E1$edon==24|SQ_E1$edon==26|SQ_E1$edon==28|
                                   SQ_E1$edon==29|SQ_E1$edon==30|SQ_E1$edon==31|SQ_E1$edon==32)
SQ_E1$PRDgob_antes <- as.numeric(SQ_E1$edon==9|SQ_E1$edon==16|SQ_E1$edon==17|SQ_E1$edon==27)
SQ_E1$PVEMgob_antes <- as.numeric(SQ_E1$edon==7)
SQ_E1$MCgob_antes <- as.numeric(SQ_E1$edon==20)
SQ_E1$Indgob_antes <- as.numeric(SQ_E1$edon==19)


SQ_E3$PANgob_antes <- as.numeric(SQ_E3$edon==2| SQ_E3$edon==3| SQ_E3$edon==11|
                                   SQ_E3$edon==21|SQ_E3$edon==22|SQ_E3$edon==25)
SQ_E3$PRIgob_antes <- as.numeric(SQ_E3$edon==1| SQ_E3$edon==4| SQ_E3$edon==5|
                                   SQ_E3$edon==6| SQ_E3$edon==8| SQ_E3$edon==10|SQ_E3$edon==12|
                                   SQ_E3$edon==13|SQ_E3$edon==14|SQ_E3$edon==15|SQ_E3$edon==18|
                                   SQ_E3$edon==23|SQ_E3$edon==24|SQ_E3$edon==26|SQ_E3$edon==28|
                                   SQ_E3$edon==29|SQ_E3$edon==30|SQ_E3$edon==31|SQ_E3$edon==32)
SQ_E3$PRDgob_antes <- as.numeric(SQ_E3$edon==9| SQ_E3$edon==16|SQ_E3$edon==17|SQ_E3$edon==27)
SQ_E3$PVEMgob_antes <- as.numeric(SQ_E3$edon==7)
SQ_E3$MCgob_antes <- as.numeric(SQ_E3$edon==20)
SQ_E3$Indgob_antes <- as.numeric(SQ_E3$edon==19)


E1_E3$PANgob_antes <- as.numeric(E1_E3$edon==2| E1_E3$edon==3| E1_E3$edon==11|
                                   E1_E3$edon==21|E1_E3$edon==22|E1_E3$edon==25)
E1_E3$PRIgob_antes <- as.numeric(E1_E3$edon==1| E1_E3$edon==4| E1_E3$edon==5|
                                   E1_E3$edon==6| E1_E3$edon==8| E1_E3$edon==10|E1_E3$edon==12|
                                   E1_E3$edon==13|E1_E3$edon==14|E1_E3$edon==15|E1_E3$edon==18|
                                   E1_E3$edon==23|E1_E3$edon==24|E1_E3$edon==26|E1_E3$edon==28|
                                   E1_E3$edon==29|E1_E3$edon==30|E1_E3$edon==31|E1_E3$edon==32)
E1_E3$PRDgob_antes <- as.numeric(E1_E3$edon==9| E1_E3$edon==16|E1_E3$edon==17|E1_E3$edon==27)
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
                                     SQ_E1$edon==29|SQ_E1$edon==32)
SQ_E1$PRDgob_despues <- as.numeric(SQ_E1$edon==16|SQ_E1$edon==23)
SQ_E1$Morenagob_despues <- as.numeric(SQ_E1$edon==7,SQ_E1$edon==9,SQ_E1$edon==27)
SQ_E1$MCgob_despues <- as.numeric(SQ_E1$edon==14)
SQ_E1$Indgob_despues <- as.numeric(SQ_E1$edon==19)
SQ_E1$PESgob_despues <- as.numeric(SQ_E1$edon==17)


SQ_E3$PANgob_despues <- as.numeric(SQ_E3$edon==2| SQ_E3$edon==3| SQ_E3$edon==11|
                                     SQ_E3$edon==21|SQ_E3$edon==22|SQ_E3$edon==1|
                                     SQ_E3$edon==8| SQ_E3$edon==10|SQ_E3$edon==18|
                                     SQ_E3$edon==28|
                                     SQ_E3$edon==30|SQ_E3$edon==31)
SQ_E3$PRIgob_despues <- as.numeric(SQ_E3$edon==4| SQ_E3$edon==5|
                                     SQ_E3$edon==6| SQ_E3$edon==12|
                                     SQ_E3$edon==13|SQ_E3$edon==15|SQ_E3$edon==20|
                                     SQ_E3$edon==24|SQ_E3$edon==26|SQ_E3$edon==25|
                                     SQ_E3$edon==29|SQ_E3$edon==32)
SQ_E3$PRDgob_despues <- as.numeric(SQ_E3$edon==16|SQ_E3$edon==23)
SQ_E3$Morenagob_despues <- as.numeric(SQ_E3$edon==7,SQ_E3$edon==9,SQ_E3$edon==27)
SQ_E3$MCgob_despues <- as.numeric( SQ_E3$edon==14)
SQ_E3$Indgob_despues <- as.numeric(SQ_E3$edon==19)
SQ_E3$PESgob_despues <- as.numeric(SQ_E3$edon==17)


E1_E3$PANgob_despues <- as.numeric(E1_E3$edon==2| E1_E3$edon==3| E1_E3$edon==11|
                                     E1_E3$edon==21|E1_E3$edon==22|E1_E3$edon==1|
                                     E1_E3$edon==8| E1_E3$edon==10|E1_E3$edon==18|
                                     E1_E3$edon==28|
                                     E1_E3$edon==30|E1_E3$edon==31)
E1_E3$PRIgob_despues <- as.numeric(E1_E3$edon==4| E1_E3$edon==5|
                                     E1_E3$edon==6| E1_E3$edon==12|
                                     E1_E3$edon==13|E1_E3$edon==15|E1_E3$edon==20|
                                     E1_E3$edon==24|E1_E3$edon==26|E1_E3$edon==25|
                                     E1_E3$edon==29|E1_E3$edon==32)
E1_E3$PRDgob_despues <- as.numeric(E1_E3$edon==16|E1_E3$edon==23)
E1_E3$Morenagob_despues <- as.numeric(E1_E3$edon==7,E1_E3$edon==9,E1_E3$edon==27)
E1_E3$MCgob_despues <- as.numeric( E1_E3$edon==14)
E1_E3$Indgob_despues <- as.numeric(E1_E3$edon==19)
E1_E3$PESgob_despues <- as.numeric(E1_E3$edon==17)

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
                                    SQ_E1$edon==32|SQ_E1$edon==24)
SQ_E1$PRDcong_antes <- as.numeric(SQ_E1$edon==9|SQ_E1$edon==17|SQ_E1$edon==27)
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
                                    SQ_E3$edon==32|SQ_E3$edon==24)
SQ_E3$PRDcong_antes <- as.numeric(SQ_E3$edon==9| SQ_E3$edon==17|SQ_E3$edon==27)
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
                                    E1_E3$edon==32|E1_E3$edon==24)
E1_E3$PRDcong_antes <- as.numeric(E1_E3$edon==9| E1_E3$edon==17|E1_E3$edon==27)
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
                                         SQ_E1$edon==25|SQ_E1$edon==26|SQ_E1$edon==27|SQ_E1$edon==29|SQ_E1$edon==30)



SQ_E3$PANcong_despues <- as.numeric(SQ_E3$edon==1| SQ_E3$edon==8| SQ_E3$edon==11|
                                      SQ_E3$edon==18|SQ_E3$edon==19|SQ_E3$edon==22|
                                      SQ_E3$edon==24|SQ_E3$edon==28)
SQ_E3$PRIcong_despues <- as.numeric(SQ_E3$edon==5| SQ_E3$edon==23|SQ_E3$edon==31|SQ_E3$edon==32)
SQ_E3$MCcong_despues <- as.numeric(   SQ_E3$edon==14)
SQ_E3$PENDIENTEcong_despues <- as.numeric(SQ_E3$edon==2)
SQ_E3$Morenacong_despues <- as.numeric(SQ_E3$edon==3| SQ_E3$edon==4| SQ_E3$edon==6| SQ_E3$edon==7|SQ_E3$edon==9|
                                         SQ_E3$edon==10|SQ_E3$edon==12|SQ_E3$edon==13|SQ_E3$edon==15|
                                         SQ_E3$edon==16|SQ_E3$edon==17|SQ_E3$edon==20|SQ_E3$edon==21|SQ_E3$edon==24|
                                         SQ_E3$edon==25|SQ_E3$edon==26|SQ_E3$edon==27|SQ_E3$edon==29|SQ_E3$edon==30)



E1_E3$PANcong_despues <- as.numeric(E1_E3$edon==1| E1_E3$edon==8| E1_E3$edon==11|
                                      E1_E3$edon==18|E1_E3$edon==19|E1_E3$edon==22|
                                      E1_E3$edon==24|E1_E3$edon==28)
E1_E3$PRIcong_despues <- as.numeric(E1_E3$edon==5| E1_E3$edon==23|E1_E3$edon==31|E1_E3$edon==32)
E1_E3$MCcong_despues <- as.numeric(   E1_E3$edon==14)
E1_E3$PENDIENTEcong_despues <- as.numeric(E1_E3$edon==2)
E1_E3$Morenacong_despues <- as.numeric(E1_E3$edon==3| E1_E3$edon==4|  E1_E3$edon==6|E1_E3$edon==7|E1_E3$edon==9|
                                         E1_E3$edon==10|E1_E3$edon==12| E1_E3$edon==13|E1_E3$edon==15|
                                         E1_E3$edon==16|E1_E3$edon==17| E1_E3$edon==20|E1_E3$edon==21|E1_E3$edon==24|
                                         E1_E3$edon==25|E1_E3$edon==26|E1_E3$edon==27|E1_E3$edon==29|E1_E3$edon==30)

mod1 <- lm(dcrit8~dsi_pob+dist2015+prop_mixta+prop_urbana+prop_rural+PANgob_antes+PANgob_despues+PANcong_antes+PANcong_despues+PRIcong_antes+Morenacong_despues,data=SQ_E1)
mod2 <- lm(dcrit8~dsi_pob+dist2015+prop_mixta+prop_urbana+prop_rural+PANgob_antes+PANgob_despues+PANcong_antes+PANcong_despues+PRIcong_antes+Morenacong_despues,data=SQ_E3)
mod3 <- lm(dcrit8~dsi_pob+dist2015+prop_mixta+prop_urbana+prop_rural+PANgob_antes+PANgob_despues+PANcong_antes+PANcong_despues+PRIcong_antes+Morenacong_despues,data=E1_E3)

stargazer(mod1,mod2,mod3,type="html",out="/Volumes/DANIEL/tesis_DSI/archivos/dcrit8_dsipob.htm")

mod4 <- lm(dcrit8~dsi_sec+dist2015+prop_mixta+prop_urbana+prop_rural+PANgob_antes+PANgob_despues+PANcong_antes+PANcong_despues+PRIcong_antes+Morenacong_despues,data=SQ_E1)
mod5 <- lm(dcrit8~dsi_sec+dist2015+prop_mixta+prop_urbana+prop_rural+PANgob_antes+PANgob_despues+PANcong_antes+PANcong_despues+PRIcong_antes+Morenacong_despues,data=SQ_E3)
mod6 <- lm(dcrit8~dsi_sec+dist2015+prop_mixta+prop_urbana+prop_rural+PANgob_antes+PANgob_despues+PANcong_antes+PANcong_despues+PRIcong_antes+Morenacong_despues,data=E1_E3)

stargazer(mod4,mod5,mod6,type="html",out="/Volumes/DANIEL/tesis_DSI/archivos/dcrit8_dsisec.htm")

SQ_E1$PANgober_2antes <- as.numeric(SQ_E1$edon==1|SQ_E1$edon==2|SQ_E1$edon==11|
                                      SQ_E1$edon==14|SQ_E1$edon==17|SQ_E1$edon==22|
                                      SQ_E1$edon==24|SQ_E1$edon==29)
SQ_E1$PRIgober_2antes <- as.numeric(SQ_E1$edon==4|SQ_E1$edon==5|SQ_E1$edon==6|SQ_E1$edon==8|
                                      SQ_E1$edon==10|SQ_E1$edon==13|SQ_E1$edon==15|SQ_E1$edon==18|
                                      SQ_E1$edon==19|SQ_E1$edon==20|SQ_E1$edon==21|SQ_E1$edon==23|
                                      SQ_E1$edon==25|SQ_E1$edon==26|SQ_E1$edon==27|SQ_E1$edon==28|
                                      SQ_E1$edon==30|SQ_E1$edon==31)
SQ_E1$PRDgober_2antes <- as.numeric(SQ_E1$edon==3|SQ_E1$edon==7|SQ_E1$edon==12|
                                      SQ_E1$edon==32|SQ_E1$edon==9|SQ_E1$edon==16)
SQ_E3$PANgober_2antes <- as.numeric(SQ_E3$edon==1|SQ_E3$edon==2|SQ_E3$edon==11|
                                      SQ_E3$edon==14|SQ_E3$edon==17|SQ_E3$edon==22|
                                      SQ_E3$edon==24|SQ_E3$edon==29)
SQ_E3$PRIgober_2antes <- as.numeric(SQ_E3$edon==4|SQ_E3$edon==5|SQ_E3$edon==6|SQ_E3$edon==8|
                                      SQ_E3$edon==10|SQ_E3$edon==13|SQ_E3$edon==15|SQ_E3$edon==18|
                                      SQ_E3$edon==19|SQ_E3$edon==20|SQ_E3$edon==21|SQ_E3$edon==23|
                                      SQ_E3$edon==25|SQ_E3$edon==26|SQ_E3$edon==27|SQ_E3$edon==28|
                                      SQ_E3$edon==30|SQ_E3$edon==31)
SQ_E3$PRDgober_2antes <- as.numeric(SQ_E3$edon==3|SQ_E3$edon==7|SQ_E3$edon==12|
                                       SQ_E3$edon==32|SQ_E3$edon==9|SQ_E3$edon==16)
E1_E3$PANgober_2antes <- as.numeric(E1_E3$edon==1| E1_E3$edon==2| E1_E3$edon==11|
                                    E1_E3$edon==14|E1_E3$edon==17|E1_E3$edon==22|
                                    E1_E3$edon==24|E1_E3$edon==29)
E1_E3$PRIgober_2antes <- as.numeric(E1_E3$edon==4| E1_E3$edon==5| E1_E3$edon==6| E1_E3$edon==8|
                                    E1_E3$edon==10|E1_E3$edon==13|E1_E3$edon==15|E1_E3$edon==18|
                                    E1_E3$edon==19|E1_E3$edon==20|E1_E3$edon==21|E1_E3$edon==23|
                                    E1_E3$edon==25|E1_E3$edon==26|E1_E3$edon==27|E1_E3$edon==28|
                                    E1_E3$edon==30|E1_E3$edon==31)
E1_E3$PRDgober_2antes <- as.numeric(E1_E3$edon==3|E1_E3$edon==7|E1_E3$edon==12|
                                      E1_E3$edon==32|E1_E3$edon==9|E1_E3$edon==16)

summary(lm())


#hacer cruces de dcrit8 con partido gober actual -> ya estaba
#hacer cruces de dcrit8 con partido 2 gobers anteriores -> ya está el anterior, falta correr con el de hace 2
#hacer cruces de dcrit8 con año redist, ya estaba

#DV dcrit8
#DV dsi 


names(SQ_E1)
plot(SQ_E1$prop_urbana, SQ_E1$dsi_pob)
plot(SQ_E1$prop_rural, SQ_E1$dsi_pob)
plot(SQ_E1$prop_mixta, SQ_E1$dsi_pob)

scatter_SQ_E3_dsi_pob_urbana <- plot(SQ_E3$prop_urbana, SQ_E3$dsi_pob, main="prop_urbana vs dsi_pob, SQ_E3")
scatter_SQ_E3_dsi_pob_rural <-plot(SQ_E3$prop_rural, SQ_E3$dsi_pob,main="prop_rural vs dsi_pob, SQ_E3")
scatter_SQ_E3_dsi_pob_mixta <-plot(SQ_E3$prop_mixta, SQ_E3$dsi_pob,main="prop_mixta vs dsi_pob, SQ_E3")
scatter_SQ_E3_dsi_sec_urbana <-plot(SQ_E3$prop_urbana, SQ_E3$dsi_sec, main="prop_urbana vs dsi_sec, SQ_E3")
scatter_SQ_E3_dsi_sec_rural <-plot(SQ_E3$prop_rural, SQ_E3$dsi_sec, main="prop_rural vs dsi_sec, SQ_E3")
scatter_SQ_E3_dsi_sec_mixta <-plot(SQ_E3$prop_mixta, SQ_E3$dsi_sec, main="prop_mixta vs dsi_sec, SQ_E3")

library(tidyverse)
bastiones <- nwin
#generar logicals
bastiones <- bastiones %>% 
  mutate(
    bastion_pan=pan>=4,
    bastion_pri=pri>=4,
    bastion_prd=prd>=4)
#generar dummies
bastiones<-bastiones %>% 
  mutate(
    dummy_pan=as.numeric(bastion_pan),
    dummy_pri=as.numeric(bastion_pri),
    dummy_prd=as.numeric(bastion_prd)
  )

bastion_ags <- bastiones %>% 
  filter(edon==1)
bastion_bc <- bastiones %>% 
  filter(edon==2)
bastion_bcs <- bastiones %>% 
  filter(edon==3)
bastion_cam <- bastiones %>% 
  filter(edon==4)
bastion_coa <- bastiones %>% 
  filter(edon==5)
bastion_col <- bastiones %>% 
  filter(edon==6)
bastion_cps <- bastiones %>% 
  filter(edon==7)
bastion_cua <- bastiones %>% 
  filter(edon==8)
bastion_df <- bastiones %>% 
  filter(edon==9)
bastion_dgo <- bastiones %>% 
  filter(edon==10)
bastion_gua <- bastiones %>% 
  filter(edon==11)
bastion_gue <- bastiones %>% 
  filter(edon==12)
bastion_hgo <- bastiones %>% 
  filter(edon==13)
bastion_jal <- bastiones %>% 
  filter(edon==14)
bastion_mex <- bastiones %>% 
  filter(edon==15)
bastion_mic <- bastiones %>% 
  filter(edon==16)
bastion_mor <- bastiones %>% 
  filter(edon==17)
bastion_nay <- bastiones %>% 
  filter(edon==18)
bastion_nl <- bastiones %>% 
  filter(edon==19)
bastion_oax <- bastiones %>% 
  filter(edon==20)
bastion_pue <- bastiones %>% 
  filter(edon==21)
bastion_que <- bastiones %>% 
  filter(edon==22)
bastion_qui <- bastiones %>% 
  filter(edon==23)
bastion_san <- bastiones %>% 
  filter(edon==24)
bastion_sin <- bastiones %>% 
  filter(edon==25)
bastion_son <- bastiones %>% 
  filter(edon==26)
bastion_tab <- bastiones %>% 
  filter(edon==27)
bastion_tam <- bastiones %>% 
  filter(edon==28)
bastion_tla <- bastiones %>% 
  filter(edon==29)
bastion_ver <- bastiones %>% 
  filter(edon==30)
bastion_yuc <- bastiones %>% 
  filter(edon==31)
bastion_zac <- bastiones %>% 
  filter(edon==32)

SQ_E1<-read.csv("/Volumes/DANIEL/tesis_DSI/archivos/base_SQ_E1.csv")
ags <- SQ_E1 %>% 
  filter(edon==1)
ags_bastion <- merge(ags, bastion_ags,by="seccion")
