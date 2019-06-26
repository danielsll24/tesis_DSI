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




summary(lm(dsi_pob~dcrit8+RRI_anterior+prop_rural+prop_bastion_pri+
             prop_bastion_pan+prop_bastion_prd+
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
