library(ggplot2)

dsi_sec_SQE1 <- c(0.028,0.343,0.4775,0.6532,1)
nombres_boxplot <- c("min","Q1","med","Q3","max")
df <- data.frame(nombres_boxplot,dsi_sec_SQE1)

y <- c(0.028,0.343,0.4775,0.6532,1)
df <- data.frame(
  x = 1,
  y0 = min(y),
  y25 = quantile(y, 0.25),
  y50 = median(y),
  y75 = quantile(y, 0.75),
  y100 = max(y)
)
ggplot(df, aes(x)) +
  geom_boxplot(
    aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
    stat = "identity") +
  ggtitle("Distribución del DSI_sec SQ-E1")+
  xlab(" ")+ylab(" ")+theme_minimal()
  
Distribución <- c(0.028,0.343,0.4775,0.6532,1,0.025, 0.335, 0.4865, 0.6773, 1)
SQ_E1_pob <- c()
DSI_SQ_E1 <- c("DSI_sec","DSI_sec","DSI_sec","DSI_sec","DSI_sec","DSI_pob","DSI_pob","DSI_pob","DSI_pob","DSI_pob")

df <- data.frame(Distribución, DSI_SQ_E1)
ggplot(df, aes(DSI_SQ_E1,Distribución))+geom_boxplot()+theme_minimal()+
  ggtitle("Distribución del índice de similitud distrital poblacional y por secciones electorales",subtitle="Comparación de statu quo (SQ) con escenario 1 (E1)")

Distribución <- c(0.027,0.356,0.563,0.767,1,0.025, 0.3575, 0.5405, 0.7642, 1)
SQ_E1_pob <- c()
DSI_SQ_E3 <- c("DSI_sec","DSI_sec","DSI_sec","DSI_sec","DSI_sec","DSI_pob","DSI_pob","DSI_pob","DSI_pob","DSI_pob")

df <- data.frame(Distribución, DSI_SQ_E3)
ggplot(df, aes(DSI_SQ_E3,Distribución))+geom_boxplot()+theme_minimal()+
  ggtitle("Distribución del índice de similitud distrital poblacional y por secciones electorales",subtitle="Comparación de statu quo (SQ) con escenario 3 (E3)")

Distribución <- c(0,0.505,0.925,1,1,0,0.512, 0.942, 1, 1)
SQ_E1_pob <- c()
DSI_E1_E3 <- c("DSI_sec","DSI_sec","DSI_sec","DSI_sec","DSI_sec","DSI_pob","DSI_pob","DSI_pob","DSI_pob","DSI_pob")

df <- data.frame(Distribución, DSI_E1_E3)
ggplot(df, aes(DSI_E1_E3,Distribución))+geom_boxplot()+theme_minimal()+
  ggtitle("Distribución del índice de similitud distrital poblacional y por secciones electorales",subtitle="Comparación de escenario 1 (E1) con escenario 3 (E3)")

modelo1 <- lm(dsi_sec~dcrit8+prop_urbana+prop_bastion_pan+prop_bastion_pri+prop_bastion_prd+
                PANgob_antes+PANgober_2antes+PANcong_antes+PANcong_despues+
                PRIgob_antes+PRIgober_2antes+PRIcong_antes+PRIcong_despues+
                PRDgob_antes+dist2017,data=SQ_E3)
modelo2 <- lm(dsi_sec~dcrit8+prop_rural+prop_bastion_pan+prop_bastion_pri+prop_bastion_prd+
                PANgob_antes+PANgober_2antes+PANcong_antes+PANcong_despues+
                PRIgob_antes+PRIgober_2antes+PRIcong_antes+PRIcong_despues+
                PRDgob_antes+dist2017,data=SQ_E3)
modelo3 <- lm(dsi_pob~dcrit8+prop_urbana+prop_bastion_pan+prop_bastion_pri+prop_bastion_prd+
                PANgob_antes+PANgober_2antes+PANcong_antes+PANcong_despues+
                PRIgob_antes+PRIgober_2antes+PRIcong_antes+PRIcong_despues+
                PRDgob_antes+dist2017,data=SQ_E3)
modelo4 <- lm(dsi_pob~dcrit8+prop_rural+prop_bastion_pan+prop_bastion_pri+prop_bastion_prd+
                PANgob_antes+PANgober_2antes+PANcong_antes+PANcong_despues+
                PRIgob_antes+PRIgober_2antes+PRIcong_antes+PRIcong_despues+
                PRDgob_antes+dist2017,data=SQ_E3)

#coa df gue san tla yuc zac
ee<-"/Volumes/DANIEL/comparaciones/"
e<-read.csv(paste(ee,"tla.csv",sep=""),stringsAsFactors = F)
head(e)
tail(e)
