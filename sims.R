sims <- with(SQ_E3,
              data.frame(dsi_sec = median(dsi_sec),
                         prop_bastion_pan=median(prop_bastion_pan),
                         prop_bastion_pri=median(prop_bastion_pri),
                         e2_pan      = 0,
                         e2_pri      =  1,
                         PANgob_antes  = 0,
                         PRIgob_antes   = 1,
                         
                         dcrit8 = 0,
                         prop_rural = median(SQ_E3$prop_rural),
                         indigena = 0,
                         RRI_anterior=seq(from=(min(RRI_anterior)-0.05),to=(max(RRI_anterior)+.05),length.out = 19))
)
sims2 <- with(SQ_E3,
             data.frame(dsi_sec = median(dsi_sec),
                        prop_bastion_pan=median(prop_bastion_pan),
                        prop_bastion_pri=median(prop_bastion_pri),
                        e2_pan      = 1,
                        e2_pri      =  0,
                        PANgob_antes  = 0,
                        PRIgob_antes   = 1,
                        
                        dcrit8 = 0,
                        prop_rural = median(prop_rural),
                        indigena = 0,
                        RRI_anterior=seq(from=(min(RRI_anterior)-0.05),to=(max(RRI_anterior)+.05),length.out = 19))
)
sims3 <- with(SQ_E3,
              data.frame(dsi_sec = median(dsi_sec),
                         prop_bastion_pan=median(prop_bastion_pan),
                         prop_bastion_pri=median(prop_bastion_pri),
                         e2_pan      = 0,
                         e2_pri      =  0,
                         PANgob_antes  = 0,
                         PRIgob_antes   = 1,
                         
                         dcrit8 = 1,
                         prop_rural = median(prop_rural),
                         indigena = 0,
                         RRI_anterior=seq(from=(min(RRI_anterior)-0.05),to=(max(RRI_anterior)+.05),length.out = 19))
)
sims4 <- with(SQ_E3,
              data.frame(dsi_sec = median(dsi_sec),
                         prop_bastion_pan=seq(from=(min(prop_bastion_pan)-0.05),to=(max(prop_bastion_pan)+0.05),length.out=19),
                         prop_bastion_pri=median(prop_bastion_pri),
                         e2_pan      = 1,
                         e2_pri      =  0,
                         PANgob_antes  = 0,
                         PRIgob_antes   = 1,
                         
                         dcrit8 = 0,
                         prop_rural = median(prop_rural),
                         indigena = 0,
                         RRI_anterior=median(RRI_anterior)))

sims5 <- with(SQ_E3,
              data.frame(dsi_sec = median(dsi_sec),
                         prop_bastion_pan=seq(from=(min(prop_bastion_pan)-0.05),to=(max(prop_bastion_pan)+0.05),length.out=19),
              prop_bastion_pri=median(prop_bastion_pri),
              e2_pan      = 1,
              e2_pri      =  0,
              PANgob_antes  = 1,
              PRIgob_antes   = 0,
              
              dcrit8 = 0,
              prop_rural = median(prop_rural),
              indigena = 0,
              RRI_anterior=median(RRI_anterior)
))
sims6 <- with(SQ_E3,
              data.frame(dsi_sec = median(dsi_sec),
                         prop_bastion_pri=seq(from=(min(prop_bastion_pri)-0.05),to=(max(prop_bastion_pri)+0.05),length.out=19),
              prop_bastion_pan=median(prop_bastion_pan),
              e2_pan      = 1,
              e2_pri      =  0,
              PANgob_antes  = 0,
              PRIgob_antes   = 1,
              
              dcrit8 = 0,
              prop_rural = median(prop_rural),
              indigena = 0,
              RRI_anterior=median(RRI_anterior)
))
sims7 <- with(SQ_E3,
              data.frame(dsi_sec = median(dsi_sec),
                         prop_bastion_pri=seq(from=(min(prop_bastion_pri)-0.05),to=(max(prop_bastion_pri)+0.05),length.out=19),
              prop_bastion_pan=median(prop_bastion_pan),
              e2_pan      = 1,
              e2_pri      =  0,
              PANgob_antes  = 1,
              PRIgob_antes   = 0,
              
              dcrit8 = 0,
              prop_rural = median(prop_rural),
              indigena = 0,
              RRI_anterior=median(RRI_anterior)
))
sims8 <- with(SQ_E3,
              data.frame(dsi_sec = median(dsi_sec),
                         prop_rural=seq(from=(min(prop_rural)-0.05),to=(max(prop_rural)+0.05),length.out=19),
              prop_bastion_pan=median(prop_bastion_pan),
              e2_pan      = 1,
              e2_pri      =  0,
              PANgob_antes  = 0,
              PRIgob_antes   = 1,
              
              dcrit8 = 0,
              prop_bastion_pri = median(prop_bastion_pri),
              indigena = 0,
              RRI_anterior=median(RRI_anterior)
))
sims9 <- with(SQ_E3,
              data.frame(dsi_sec = median(dsi_sec),
                         prop_rural=seq(from=(min(prop_rural)-0.05),to=(max(prop_rural)+0.05),length.out=19),
              prop_bastion_pan=median(prop_bastion_pan),
              e2_pan      = 1,
              e2_pri      =  0,
              PANgob_antes  = 1,
              PRIgob_antes   = 0,
              
              dcrit8 = 0,
              prop_bastion_pri = median(prop_bastion_pri),
              indigena = 0,
              RRI_anterior=median(RRI_anterior)
))

sims$pr<-predict(mod4,newdata=sims,type="response",interval="prediction")
sims<-cbind(sims, predict(mod4, newdata=sims,type="response",se=T))
sims<-within(sims, {
  PredictedProb <-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims2$pr<-predict(mod4,newdata = sims2,type = "response",interval="prediction")
sims2<-cbind(sims2, predict(mod4, newdata=sims2,type="response",se=T))
sims2<-within(sims2, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims3$pr<-predict(mod4,newdata = sims3,type = "response",interval="prediction")
sims3<-cbind(sims3, predict(mod4, newdata=sims3,type="response",se=T))
sims3<-within(sims3, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims4$pr<-predict(mod4,newdata = sims4,type = "response",interval="prediction")
sims4<-cbind(sims4, predict(mod4, newdata=sims4,type="response",se=T))
sims4<-within(sims4, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims5$pr<-predict(mod4,newdata = sims5,type = "response",interval="prediction")
sims5<-cbind(sims5, predict(mod4, newdata=sims5,type="response",se=T))
sims5<-within(sims5, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims6$pr<-predict(mod4,newdata = sims6,type = "response",interval="prediction")
sims6<-cbind(sims6, predict(mod4, newdata=sims6,type="response",se=T))
sims6<-within(sims6, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims7$pr<-predict(mod4,newdata = sims7,type = "response",interval="prediction")
sims7<-cbind(sims7, predict(mod4, newdata=sims7,type="response",se=T))
sims7<-within(sims7, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims8$pr<-predict(mod4,newdata = sims8,type = "response",interval="prediction")
sims8<-cbind(sims8, predict(mod4, newdata=sims8,type="response",se=T))
sims8<-within(sims8, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims9$pr<-predict(mod4,newdata = sims9,type = "response",interval="prediction")
sims9<-cbind(sims9, predict(mod4, newdata=sims9,type="response",se=T))
sims9<-within(sims9, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
#sims$legyr<-seq(from=1, to=0, length.out = 19)
#head(sims)
#ggplot(sims, aes(x=legyr, y=PredictedProb))+
  #geom_ribbon(ymin=LL,ymax=UL)+
  #geom_line(aes(colour=(RRI_anterior),size=1))

#Tomar las columnas RRI_anterior, PredictedProb1, PredictedProb2. PredictedProb3,
#UL1,LL1,UL2,LL2,UL3,LL3 para graficar 
ggplot(sims,aes(x=sims$RRI_anterior,y=sims$PredictedProb))+
  geom_ribbon(aes(sims$RRI_anterior,ymax=sims$UL, ymin=sims$LL),
                         color="grey80",alpha=0.2)+
  stat_smooth(method="lm",color="blue")+theme_bw()
ggplot(sims2, aes(x=RRI_anterior,y=PredictedProb))+
  geom_ribbon(aes(RRI_anterior,ymax=UL,ymin=LL),
              color="grey80",alpha=0.2)+
  stat_smooth(method="lm",color="red")+theme_bw()
ggplot(sims3, aes(x=RRI_anterior,y=PredictedProb))+
  geom_ribbon(aes(RRI_anterior,ymax=UL,ymin=LL),
              color="grey80",alpha=0.2)+
  stat_smooth(method="lm",color="green")+theme_bw()


simula_g1 <- data.frame(sims$RRI_anterior,sims$UL,sims$LL,sims$PredictedProb,sims2$UL,sims2$LL,sims2$PredictedProb,sims3$UL,sims3$LL,sims3$PredictedProb)
names(simula_g1)<-c("RRI","UL1","LL1","PredictedProb1","UL2","LL2","PredictedProb2","UL3","LL3","PredictedProb3")
ggplot(simula_g1, aes(x=RRI))+theme_minimal()+
  geom_ribbon(aes(RRI,ymax=UL1,ymin=LL1),
              alpha=0.2)+
  stat_smooth(aes(y=PredictedProb1),method="lm",color="red")+
  geom_ribbon(aes(RRI,ymax=UL2,ymin=LL2),
              alpha=0.15)+
  stat_smooth(aes(y=PredictedProb2),method="lm",color="blue")+
  geom_ribbon(aes(RRI,ymax=UL3,ymin=LL3),
              alpha=0.1)+
  stat_smooth(aes(y=PredictedProb3),method="lm",color="darkgreen")+
  labs(title = "Probabilidad predicha del dsi_sec a cambios en el RRI utilizando el modelo 4 de la tabla 5.2",
       x="Malapportionment (RRI)", 
       y="Índice de similitud distrital por secciones electorales predicho")+
  annotate("text",x=3,y=0.63,label="escenario 2 PAN",colour="blue",size=6)+
  annotate("text",x=1,y=0.62,label="escenario 2 PRI",colour="red",size=6)+
  annotate("text",x=6,y=0.64,label="escenario 2 otro",colour="darkgreen",size=6)


simula_g2 <- data.frame(sims4$prop_bastion_pan,sims4$UL,sims4$LL,sims4$PredictedProb,sims5$UL,sims5$LL,sims5$PredictedProb)
names(simula_g2)<-c("Prop_bastion_pan","UL1","LL1","PredictedProb1","UL2","LL2","PredictedProb2")
ggplot(simula_g2, aes(Prop_bastion_pan))+theme_minimal()+
  geom_ribbon(aes(Prop_bastion_pan,ymax=UL1,ymin=LL1),
              alpha=0.2)+
  stat_smooth(aes(y=PredictedProb1),method="lm",color="red")+
  geom_ribbon(aes(Prop_bastion_pan,ymax=UL2,ymin=LL2),
              alpha=0.15)+
  stat_smooth(aes(y=PredictedProb2),method="lm",color="blue")+
  labs(title="Probabilidad predicha del dsi_sec a cambios en la proporción de secciones bastión del PAN utilizando el modelo 4 de la tabla 5.2",x="Proporción de secciones bastión del PAN por distrito electoral",y="Índice de similitud distrital por secciones electorales predicho")+
  annotate("text",x=.1,y=.615,label="gobernador PAN",colour="blue",size=6)+
  annotate("text",x=.75,y=.65,label="gobernador PRI",colour="red",size=6)

simula_g3 <- data.frame(sims6$prop_bastion_pri,sims6$UL,sims6$LL,sims6$PredictedProb,sims7$UL,sims7$LL,sims7$PredictedProb)
names(simula_g3)<-c("Prop_bastion_pri","UL1","LL1","PredictedProb1","UL2","LL2","PredictedProb2")
ggplot(simula_g3, aes(x=Prop_bastion_pri))+theme_minimal()+
  geom_ribbon(aes(Prop_bastion_pri,ymax=UL1,ymin=LL1),
              alpha=0.2)+
  stat_smooth(aes(y=PredictedProb1),method="lm",color="red")+
  geom_ribbon(aes(Prop_bastion_pri,ymax=UL2,ymin=LL2),
              alpha=0.15)+
  stat_smooth(aes(y=PredictedProb2),method="lm",color="blue")+
  labs(title="Probabilidad predicha del dsi_sec a cambios en la proporción de secciones bastión del PRI utilizando el modelo 4 de la tabla 5.2",x="Proporción de secciones bastión del PRI por distrito electoral",y="Índice de similitud distrital por secciones electorales predicho")+
  annotate("text",x=.1,y=.64,label="gobernador PAN",colour="blue",size=6)+
  annotate("text",x=.75,y=.66,label="gobernador PRI",colour="red",size=6)

simula_g4 <- data.frame(sims8$prop_rural,sims8$UL,sims8$LL,sims8$PredictedProb,sims9$UL,sims9$LL,sims9$PredictedProb)
names(simula_g4)<-c("Prop_rural","UL1","LL1","PredictedProb1","UL2","LL2","PredictedProb2")
ggplot(simula_g4, aes(x=Prop_rural))+theme_minimal()+
  geom_ribbon(aes(Prop_rural,ymax=UL1,ymin=LL1),
              alpha=0.2)+
  stat_smooth(aes(y=PredictedProb1),method="lm",color="blue")+
  geom_ribbon(aes(Prop_rural,ymax=UL2,ymin=LL2),
              alpha=0.15)+
  stat_smooth(aes(y=PredictedProb2),method="lm",color="red")+
 labs(title = "Probabilidad predicha del dsi_sec a cambios en la proporción de secciones rurales utilizando el modelo 4 de la tabla 5.2",
       x="Proporción de secciones rurales por distrito electoral",y="Índice de similitud distrital por secciones electorales predicho")+
  annotate("text",x=0.25,y=0.67,label="gobernador PAN",colour="blue",size=6)+
  annotate("text",x=0.75,y=0.635,label="gobernador PRI",colour="red",size=6)
  
library(ggplot2)
ggplot(data=SQ_E3, aes(dsi_sec, dsi_pob))+
  geom_point()+theme_minimal()+
  labs(title = "Índice de similitud distrital por secciones vs Índice de similitud distrital por población")+
  geom_abline(slope=1,intercept=0,color="red")

sims10 <- with(SQ_E3,
             data.frame(dsi_sec = median(dsi_sec),
                        prop_bastion_pan=median(prop_bastion_pan),
                        prop_bastion_pri=median(prop_bastion_pri),
                        e2_pan      = 0,
                        e2_pri      =  0,
                        PANgob_antes  = 1,
                        PRIgob_antes   = 0,
                        
                        dcrit8 = 0,
                        prop_rural = median(SQ_E3$prop_rural),
                        indigena = 0,
                        RRI_anterior=seq(from=(min(RRI_anterior)-0.05),to=(max(RRI_anterior)+.05),length.out = 19))
)
sims11 <- with(SQ_E3,
               data.frame(dsi_sec = median(dsi_sec),
                          prop_bastion_pan=median(prop_bastion_pan),
                          prop_bastion_pri=median(prop_bastion_pri),
                          e2_pan      = 0,
                          e2_pri      =  0,
                          PANgob_antes  = 0,
                          PRIgob_antes   = 1,
                          
                          dcrit8 = 0,
                          prop_rural = median(SQ_E3$prop_rural),
                          indigena = 0,
                          RRI_anterior=seq(from=(min(RRI_anterior)-0.05),to=(max(RRI_anterior)+.05),length.out = 19))
)
sims12 <- with(SQ_E3,
               data.frame(dsi_sec = median(dsi_sec),
                          prop_bastion_pan=median(prop_bastion_pan),
                          prop_bastion_pri=median(prop_bastion_pri),
                          e2_pan      = 0,
                          e2_pri      =  0,
                          PANgob_antes  = 0,
                          PRIgob_antes   = 0,
                          
                          dcrit8 = 0,
                          prop_rural = median(SQ_E3$prop_rural),
                          indigena = 0,
                          RRI_anterior=seq(from=(min(RRI_anterior)-0.05),to=(max(RRI_anterior)+.05),length.out = 19))
)
sims10$pr<-predict(mod4,newdata = sims10,type = "response",interval="prediction")
sims10<-cbind(sims10, predict(mod4, newdata=sims10,type="response",se=T))
sims10<-within(sims10, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims11$pr<-predict(mod4,newdata = sims11,type = "response",interval="prediction")
sims11<-cbind(sims11, predict(mod4, newdata=sims11,type="response",se=T))
sims11<-within(sims11, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims12$pr<-predict(mod4,newdata = sims12,type = "response",interval="prediction")
sims12<-cbind(sims12, predict(mod4, newdata=sims12,type="response",se=T))
sims12<-within(sims12, {
  PredictedProb<-plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

simula_g5 <- data.frame(sims10$RRI_anterior,sims10$UL,sims10$LL,sims10$PredictedProb,sims11$UL,sims11$LL,sims11$PredictedProb,sims12$UL,sims12$LL,sims12$PredictedProb)
names(simula_g5)<-c("RRI","UL1","LL1","PredictedProb1","UL2","LL2","PredictedProb2","UL3","LL3","PredictedProb3")
ggplot(simula_g5, aes(x=RRI))+theme_minimal()+
  geom_ribbon(aes(RRI,ymax=UL1,ymin=LL1),
              alpha=0.2)+
  stat_smooth(aes(y=PredictedProb1),method="lm",color="red")+
  geom_ribbon(aes(RRI,ymax=UL2,ymin=LL2),
              alpha=0.15)+
  stat_smooth(aes(y=PredictedProb2),method="lm",color="blue")+
  geom_ribbon(aes(RRI,ymax=UL3,ymin=LL3),
              alpha=0.1)+
  stat_smooth(aes(y=PredictedProb3),method="lm",color="darkgreen")+
  labs(title = "Probabilidad predicha del dsi_sec a cambios en el RRI utilizando el modelo 4 de la tabla 5.2",
       x="Malapportionment (RRI)", 
       y="Índice de similitud distrital por secciones electorales predicho")+
  annotate("text",x=3,y=0.65,label="escenario 2 PAN",colour="blue",size=6)+
  annotate("text",x=1,y=0.60,label="escenario 2 PRI",colour="red",size=6)+
  annotate("text",x=6,y=0.64,label="escenario 2 otro",colour="darkgreen",size=6)
