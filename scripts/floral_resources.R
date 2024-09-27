rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(tidyverse, dplyr, purr, iNEXT, wesandersom, ggplot2)


d <- read.csv("./data/floral_resources_donana_20.csv")
d.gorb <- read.csv("./data/floral_resources_gorbea_20.csv")
d.21 <- read.csv("./data/floral_resources_donana_21.csv")
d.21.gorb <- read.csv("./data/floral_resources_gorbea_21.csv")
d.22.gorb <- read.csv(
  "./data/rec_floralesG22.csv",
  header = TRUE,
  sep = ";",
  fileEncoding = "Latin1"
)

##sampling completness
d <- d %>%
  select(Tanda, Dia, Bosque, Especie.planta, Numero.flores)
d <- d %>%
  rename(Fecha = Dia)
d$Numero.flores <- as.integer(d$Numero.flores)
unique(d$Especie.planta)

d.21 <- d.21 %>%
  select(Ronda, Fecha, Bosque, Especie.planta, Numero.flores)
unique(d.21$Especie.planta)

##Doñana2020
#PLANTS
d$full <- rep(1, nrow(d))
d.pl <- table(d$full, d$Especie.planta)
d.pl2 <- as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[, 1:ncol(d.pl2)])
d.pl4 <- as.data.frame(d.pl3)
d.pl5 <- as.list(d.pl4)

rar2 <- iNEXT(d.pl5,
              q = 0,
              datatype = "abundance",
              endpoint = 2000)
plant.d20 <- ggiNEXT(rar2, color.var = "Assemblage", se = FALSE)
plant.d20

#calculate sampling completeness as observed/estimated species richness
samp_comp_pl <- round(rar2$AsyEst$Observed[1] / rar2$AsyEst$Estimator[1], 2)

#PLANTS PER SITE
d.pl <- table(d$Bosque, d$Especie.planta)
d.pl2 <- as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[, 1:ncol(d.pl2)])
d.pl4 <- as.data.frame(d.pl3)
d.pl5 <- as.list(d.pl4)

rar2 <- iNEXT(d.pl5,
              q = 0,
              datatype = "abundance",
              endpoint = 200)
plant.sites.d20 <- ggiNEXT(rar2, color.var = "Assemblage", se = FALSE)
plant.sites.d20

#calculate sampling completeness as observed/estimated species richness
est2 <- rar2$AsyEst

samp_comp_pl_site <- round(mean(
  c(
    est2$Observed[1] / est2$Estimator[1],
    est2$Observed[4] / est2$Estimator[4],
    est2$Observed[7] / est2$Estimator[7],
    est2$Observed[10] / est2$Estimator[10],
    est2$Observed[13] / est2$Estimator[13]
  )
), 2)

##per period
d <- d %>%
  rename(Periodo = Tanda)

d.pl <- table(d$Periodo, d$Especie.planta)
d.pl2 <- as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[, 1:ncol(d.pl2)])
d.pl4 <- as.data.frame(d.pl3)
d.pl5 <- as.list(d.pl4)

rar2 <- iNEXT(d.pl5,
              q = 0,
              datatype = "abundance",
              endpoint = 2000)
plant.period.d20 <- ggiNEXT(rar2, color.var = "Assemblage", se = FALSE)
plant.period.d20

#calculate sampling completeness as observed/estimated species richness
est2_period <- rar2$AsyEst

samp_comp_pl_period <- round(mean(
  c(
    est2_period$Observed[1] / est2_period$Estimator[1],
    est2_period$Observed[4] / est2_period$Estimator[4],
    est2_period$Observed[7] / est2_period$Estimator[7],
    est2_period$Observed[10] / est2_period$Estimator[10],
    est2_period$Observed[13] / est2_period$Estimator[13],
    est2_period$Observed[16] / est2_period$Estimator[16]
  )
), 2)

###doñana2021
#PLANTS
d.21$full <- rep(1, nrow(d.21))
d.pl <- table(d.21$full, d.21$Especie.planta)
d.pl2 <- as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[, 1:ncol(d.pl2)])
d.pl4 <- as.data.frame(d.pl3)
d.pl5 <- as.list(d.pl4)

rar2 <- iNEXT(d.pl5,
              q = 0,
              datatype = "abundance",
              endpoint = 2000)
plant.d21 <- ggiNEXT(rar2, color.var = "Assemblage", se = FALSE)
plant.d21

#calculate sampling completeness as observed/estimated species richness
samp_comp_pl.21 <- round(rar2$AsyEst$Observed[1] / rar2$AsyEst$Estimator[1], 2)

##per site
d.pl <- table(d.21$Bosque, d.21$Especie.planta)
d.pl2 <- as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[, 1:ncol(d.pl2)])
d.pl4 <- as.data.frame(d.pl3)
d.pl5 <- as.list(d.pl4)

rar2 <- iNEXT(d.pl5,
              q = 0,
              datatype = "abundance",
              endpoint = 200)
plant.sites.d21 <- ggiNEXT(rar2, color.var = "Assemblage", se = FALSE)
plant.sites.d21

#calculate sampling completeness as observed/estimated species richness
est2.21 <- rar2$AsyEst

samp_comp_pl_site.21 <- round(mean(
  c(
    est2.21$Observed[1] / est2.21$Estimator[1],
    est2.21$Observed[4] / est2.21$Estimator[4],
    est2.21$Observed[7] / est2.21$Estimator[7],
    est2.21$Observed[10] / est2.21$Estimator[10],
    est2.21$Observed[13] / est2.21$Estimator[13]
  )
), 2)

##per period
d.21 <- d.21 %>%
  rename(Periodo = Ronda)
d.pl <- table(d.21$Periodo, d.21$Especie.planta)
d.pl2 <- as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[, 1:ncol(d.pl2)])
d.pl4 <- as.data.frame(d.pl3)
d.pl5 <- as.list(d.pl4)

rar2 <- iNEXT(d.pl5,
              q = 0,
              datatype = "abundance",
              endpoint = 2000)
plant.period.d21 <- ggiNEXT(rar2, color.var = "Assemblage", se = FALSE)
plant.period.d21

#calculate sampling completeness as observed/estimated species richness
est2_period.21 <- rar2$AsyEst

samp_comp_pl_period.21 <- round(mean(
  c(
    est2_period.21$Observed[1] / est2_period.21$Estimator[1],
    est2_period.21$Observed[4] / est2_period.21$Estimator[4],
    est2_period.21$Observed[7] / est2_period.21$Estimator[7],
    est2_period.21$Observed[10] / est2_period.21$Estimator[10],
    est2_period.21$Observed[13] / est2_period.21$Estimator[13],
    est2_period.21$Observed[16] / est2_period.21$Estimator[16],
    est2_period.21$Observed[19] / est2_period.21$Estimator[19],
    est2_period.21$Observed[22] / est2_period.21$Estimator[22],
    est2_period.21$Observed[25] / est2_period.21$Estimator[25]
  )
), 2)

###GORBEA
unique(d.gorb$Especie.planta)

d.gorb <- d.gorb %>%
  select(Fecha, Bosque, Especie.planta, Numero.flores)
d.22.gorb <- d.22.gorb %>%
  select(Fecha, Bosque, Especie.planta, Numero.flores)

#2020
d.gorb$full<-rep(1, nrow(d.gorb))

d.pl.gorb<-table(d.gorb$full, d.gorb$Especie.planta)
d.pl.gorb2<-as.data.frame.array(d.pl.gorb)
d.pl.gorb3 <- t(d.pl.gorb2[,1:ncol(d.pl.gorb2)])
d.pl.gorb4<-as.data.frame(d.pl.gorb3)
d.pl.gorb5<-as.list(d.pl.gorb4)

rar.gorb2 <- iNEXT(d.pl.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.gorb.20<-ggiNEXT(rar.gorb2, color.var="Assemblage", se=FALSE) 
plant.gorb.20

#calculate sampling completeness as observed/estimated species richness
samp_comp_pl_gorb<-round(rar.gorb2$AsyEst$Observed[1]/rar.gorb2$AsyEst$Estimator[1],2)

##sites
d.pl.gorb<-table(d.gorb$Bosque, d.gorb$Especie.planta)
d.pl.gorb2<-as.data.frame.array(d.pl.gorb)
d.pl.gorb3 <- t(d.pl.gorb2[,1:ncol(d.pl.gorb2)])
d.pl.gorb4<-as.data.frame(d.pl.gorb3)
d.pl.gorb5<-as.list(d.pl.gorb4)

rar.gorb2 <- iNEXT(d.pl.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.sites.gorb20<-ggiNEXT(rar.gorb2, color.var="Assemblage", se=FALSE) 
plant.sites.gorb20

#calculate sampling completeness as observed/estimated species richness
est.gorb2<-rar.gorb2$AsyEst

samp_comp_pl_site_gorb<-round(mean(c(est.gorb2$Observed[1]/est.gorb2$Estimator[1], est.gorb2$Observed[4]/est.gorb2$Estimator[4],
                                     est.gorb2$Observed[7]/est.gorb2$Estimator[7],est.gorb2$Observed[10]/est.gorb2$Estimator[10],
                                     est.gorb2$Observed[13]/est.gorb2$Estimator[13])),2)

###periodo
#crear vriable periodo
require(lubridate)
#especificar que es  una fecha
d.gorb$Fecha<-dmy(d.gorb$Fecha)

d.gorb <- d.gorb %>% arrange(Fecha) %>%
  group_by(Bosque) %>%
  mutate(Periodo = match(Fecha, unique(Fecha)))


d.pl.gorb<-table(d.gorb$Periodo, d.gorb$Especie.planta)
d.pl.gorb2<-as.data.frame.array(d.pl.gorb)
d.pl.gorb3 <- t(d.pl.gorb2[,1:ncol(d.pl.gorb2)])
d.pl.gorb4<-as.data.frame(d.pl.gorb3)
d.pl.gorb5<-as.list(d.pl.gorb4)

rar.gorb2 <- iNEXT(d.pl.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.period.gorb20<-ggiNEXT(rar.gorb2, color.var="Assemblage", se=FALSE) 
plant.period.gorb20

#calculate sampling completeness as observed/estimated species richness
est2_period.gorb<-rar.gorb2$AsyEst

samp_comp_pl_period_gorb<-round(mean(c(est2_period.gorb$Observed[1]/est2_period.gorb$Estimator[1], est2_period.gorb$Observed[4]/est2_period.gorb$Estimator[4],
                                       est2_period.gorb$Observed[7]/est2_period.gorb$Estimator[7],est2_period.gorb$Observed[10]/est2_period.gorb$Estimator[10],
                                       est2_period.gorb$Observed[13]/est2_period.gorb$Estimator[13], est2_period.gorb$Observed[16]/est2_period.gorb$Estimator[16],
                                       est2_period.gorb$Observed[19]/est2_period.gorb$Estimator[19], est2_period.gorb$Observed[22]/est2_period.gorb$Estimator[22])),2)

##gorbea 2021
d.21.gorb$full<-rep(1, nrow(d.21.gorb))

d.pl.21.gorb<-table(d.21.gorb$full, d.21.gorb$Especie.planta)
d.pl.21.gorb2<-as.data.frame.array(d.pl.21.gorb)
d.pl.21.gorb3 <- t(d.pl.21.gorb2[,1:ncol(d.pl.21.gorb2)])
d.pl.21.gorb4<-as.data.frame(d.pl.21.gorb3)
d.pl.21.gorb5<-as.list(d.pl.21.gorb4)

rar.21.gorb2 <- iNEXT(d.pl.21.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.gorb21<-ggiNEXT(rar.21.gorb2, color.var="Assemblage", se=FALSE) 
plant.gorb21

#calculate sampling completeness as observed/estimated species richness
samp_comp_pl_gorb.21<-round(rar.21.gorb2$AsyEst$Observed[1]/rar.21.gorb2$AsyEst$Estimator[1],2)

#site
d.pl.21.gorb<-table(d.21.gorb$Bosque, d.21.gorb$Especie.planta)
d.pl.21.gorb2<-as.data.frame.array(d.pl.21.gorb)
d.pl.21.gorb3 <- t(d.pl.21.gorb2[,1:ncol(d.pl.21.gorb2)])
d.pl.21.gorb4<-as.data.frame(d.pl.21.gorb3)
d.pl.21.gorb5<-as.list(d.pl.21.gorb4)

rar.21.gorb2 <- iNEXT(d.pl.21.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.sites.gorb21<-ggiNEXT(rar.21.gorb2, color.var="Assemblage", se=FALSE) 
plant.sites.gorb21

#calculate sampling completeness as observed/estimated species richness
est.21.gorb2<-rar.21.gorb2$AsyEst

samp_comp_pl_site_gorb21<-round(mean(c(est.21.gorb2$Observed[1]/est.21.gorb2$Estimator[1], est.21.gorb2$Observed[4]/est.21.gorb2$Estimator[4],
                                       est.21.gorb2$Observed[7]/est.21.gorb2$Estimator[7],est.21.gorb2$Observed[10]/est.21.gorb2$Estimator[10],
                                       est.21.gorb2$Observed[13]/est.21.gorb2$Estimator[13])),2)

##periodo
#crear vriable periodo
require(lubridate)
#especificar que es  una fecha
d.21.gorb$Fecha<-dmy(d.21.gorb$Fecha)

d.21.gorb <- d.21.gorb %>% arrange(Fecha) %>%
  group_by(Bosque) %>%
  mutate(Periodo = match(Fecha, unique(Fecha)))

d.pl.21.gorb<-table(d.21.gorb$Periodo, d.21.gorb$Especie.planta)
d.pl.21.gorb2<-as.data.frame.array(d.pl.21.gorb)
d.pl.21.gorb3 <- t(d.pl.21.gorb2[,1:ncol(d.pl.21.gorb2)])
d.pl.21.gorb4<-as.data.frame(d.pl.21.gorb3)
d.pl.21.gorb5<-as.list(d.pl.21.gorb4)

rar.21.gorb2 <- iNEXT(d.pl.21.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.period.gorb21<-ggiNEXT(rar.21.gorb2, color.var="Assemblage", se=FALSE) 
plant.period.gorb21

#calculate sampling completeness as observed/estimated species richness
est2_period.21.gorb<-rar.21.gorb2$AsyEst

samp_comp_pl_period_gorb.21<-round(mean(c(est2_period.21.gorb$Observed[1]/est2_period.21.gorb$Estimator[1], est2_period.21.gorb$Observed[4]/est2_period.21.gorb$Estimator[4],
                                          est2_period.21.gorb$Observed[7]/est2_period.21.gorb$Estimator[7],est2_period.21.gorb$Observed[10]/est2_period.21.gorb$Estimator[10],
                                          est2_period.21.gorb$Observed[13]/est2_period.21.gorb$Estimator[13], est2_period.21.gorb$Observed[16]/est2_period.21.gorb$Estimator[16],
                                          est2_period.21.gorb$Observed[19]/est2_period.21.gorb$Estimator[19])),2)
##gorbea2022
d.22.gorb$full<-rep(1, nrow(d.22.gorb))
d.pl.22.gorb<-table(d.22.gorb$full, d.22.gorb$Especie.planta)
d.pl.22.gorb2<-as.data.frame.array(d.pl.22.gorb)
d.pl.22.gorb3 <- t(d.pl.22.gorb2[,1:ncol(d.pl.22.gorb2)])
d.pl.22.gorb4<-as.data.frame(d.pl.22.gorb3)
d.pl.22.gorb5<-as.list(d.pl.22.gorb4)

rar.22.gorb2 <- iNEXT(d.pl.22.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.gorb22<-ggiNEXT(rar.22.gorb2, color.var="Assemblage", se=FALSE) 
plant.gorb22

#calculate sampling completeness as observed/estimated species richness
samp_comp_pl_gorb.22<-round(rar.22.gorb2$AsyEst$Observed[1]/rar.22.gorb2$AsyEst$Estimator[1],2)

##sites

d.pl.22.gorb<-table(d.22.gorb$Bosque, d.22.gorb$Especie.planta)
d.pl.22.gorb2<-as.data.frame.array(d.pl.22.gorb)
d.pl.22.gorb3 <- t(d.pl.22.gorb2[,1:ncol(d.pl.22.gorb2)])
d.pl.22.gorb4<-as.data.frame(d.pl.22.gorb3)
d.pl.22.gorb5<-as.list(d.pl.22.gorb4)

rar.22.gorb2 <- iNEXT(d.pl.22.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.sites.gorb22<-ggiNEXT(rar.22.gorb2, color.var="Assemblage", se=FALSE) 
plant.sites.gorb22

#calculate sampling completeness as observed/estimated species richness
est.22.gorb2<-rar.22.gorb2$AsyEst

samp_comp_pl_site_gorb22<-round(mean(c(est.22.gorb2$Observed[1]/est.22.gorb2$Estimator[1], est.22.gorb2$Observed[4]/est.22.gorb2$Estimator[4],
                                       est.22.gorb2$Observed[7]/est.22.gorb2$Estimator[7],est.22.gorb2$Observed[10]/est.22.gorb2$Estimator[10],
                                       est.22.gorb2$Observed[13]/est.22.gorb2$Estimator[13])),2)

##periodo
#crear vriable periodo
require(lubridate)
#especificar que es  una fecha
d.22.gorb$Fecha<-dmy(d.22.gorb$Fecha)
unique(d.22.gorb$Fecha)
d.22.gorb <- d.22.gorb %>% arrange(Fecha) %>%
  group_by(Bosque) %>%
  mutate(Periodo = match(Fecha, unique(Fecha)))
unique(d.22.gorb$Periodo)

d.pl.22.gorb<-table(d.22.gorb$Periodo, d.22.gorb$Especie.planta)
d.pl.22.gorb2<-as.data.frame.array(d.pl.22.gorb)
d.pl.22.gorb3 <- t(d.pl.22.gorb2[,1:ncol(d.pl.22.gorb2)])
d.pl.22.gorb4<-as.data.frame(d.pl.22.gorb3)
d.pl.22.gorb5<-as.list(d.pl.22.gorb4)

rar.22.gorb2 <- iNEXT(d.pl.22.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.period.gorb22<-ggiNEXT(rar.22.gorb2, color.var="Assemblage", se=FALSE) 
plant.period.gorb22

#calculate sampling completeness as observed/estimated species richness
est2_period.22.gorb<-rar.22.gorb2$AsyEst

samp_comp_pl_period_gorb22<-round(mean(c(est2_period.22.gorb$Observed[1]/est2_period.22.gorb$Estimator[1], est2_period.22.gorb$Observed[4]/est2_period.22.gorb$Estimator[4],
                                         est2_period.22.gorb$Observed[7]/est2_period.22.gorb$Estimator[7],est2_period.22.gorb$Observed[10]/est2_period.22.gorb$Estimator[10],
                                         est2_period.22.gorb$Observed[13]/est2_period.22.gorb$Estimator[13], est2_period.22.gorb$Observed[16]/est2_period.22.gorb$Estimator[16],
                                         est2_period.22.gorb$Observed[19]/est2_period.22.gorb$Estimator[19])),2)




gorbea <- full_join(d.gorb,
                    d.21.gorb,
                    by = c("Fecha", "Bosque", "Especie.planta", "Numero.flores", "Periodo"))
gorbea <- full_join(gorbea,
                    d.22.gorb,
                    by = c("Fecha", "Bosque", "Especie.planta", "Numero.flores", "Periodo"))
unique(gorbea$Especie.planta)
gorbea <- gorbea %>%
  mutate(
    Especie.planta = case_when(
      Especie.planta == "Anthyllis vulneraria" ~ "Anthyllis vulneraria subsp. alpestris",
      Especie.planta == "Sedum sp." ~ "Sedum album",
      Especie.planta == "Pedicularis sylvatica" ~ "Pedicularis sylvatica subsp. sylvatica",
      Especie.planta == "Campanula sp." ~ "Campanula glomerata",
      Especie.planta == "Rhinanthus sp." ~ "Rhinanthus angustifolius",
      Especie.planta == "Erynus alpinus" ~ "Erinus alpinus",
      Especie.planta == "Eufrasia alpina" ~ "Euphrasia alpina",
      Especie.planta == "Helleborus viridis" ~ "Helleborus viridis subsp. occidentalis",
      Especie.planta == "Eufrasia sp." ~ "Euphrasia sp.",
      Especie.planta == "Vaccinium" ~ "Vaccinium myrtillus",
      Especie.planta == "Viola sp." ~ "Viola riviniana",
      Especie.planta == "Viola sp. riviniana" ~ "Viola riviniana",
      Especie.planta == "Blanca compuesta???" ~ "Hutchinsia alpina",
      Especie.planta == "Erysimum sp." ~ "Erysimum gorbeanum",
      Especie.planta == "Myosotis sp." ~ "Myosotis lamottiana",
      Especie.planta == "Compuesta blanca" ~ "Hutchinsia alpina",
      Especie.planta == "Lotus cornicularis" ~ "Lotus corniculatus",
      Especie.planta == "Pilosella sp." ~ "Pilosella officinarum",
      Especie.planta == "Lotus corniculatus " ~ "Lotus corniculatus",
      Especie.planta == "Thymus sp." ~ "Thymus praecox subsp. Polytrichus",
      Especie.planta == "Thymus praecox subsp. polytrichus  sp." ~ "Thymus praecox subsp. Polytrichus",
      Especie.planta == "Dianthus hyssopifolius " ~ "Dianthus hyssopifolius subsp. Hyssopifolius",
      Especie.planta == "Dianthus hyssopifolius" ~ "Dianthus hyssopifolius subsp. Hyssopifolius",
      Especie.planta == "Geranium robertianum " ~ "Geranium robertianum",
      Especie.planta == "Plantago" ~ "Plantago sp.",
      Especie.planta == "Helianthemum sp." ~ "Helianthemum nummularium",
      Especie.planta == "Teucrium pyrenaicum" ~ "Teucrium pyrenaicum subsp. Pyrenaicum",
      Especie.planta == "Anthyllis vulneraria subsp. alpestris" ~ "Anthyllis vulneraria subsp. Alpestris",
      Especie.planta == "Dianthus hyssopifolius subsp. hyssopifolius" ~ "Dianthus hyssopifolius subsp. Hyssopifolius",
      Especie.planta == "Erytronium dens-canis" ~ "Erythronium dens-canis",
      Especie.planta == "Globularia sp." ~ "Globularia nudicaulis",
      Especie.planta == "Linnaria propinqua" ~ "Linaria propinqua",
      Especie.planta == "Ophyrs apifera" ~ "Ophrys apifera",
      Especie.planta == "Pinguicula grandiflora" ~ "Pinguicula grandiflora subsp. Grandiflora",
      Especie.planta == "Pinguicula grandiflora subsp. grandiflora" ~ "Pinguicula grandiflora subsp. Grandiflora",
      Especie.planta == "Potentilla esterilis" ~ "Potentilla sterilis",
      Especie.planta == "Saxifraga  hirsuta" ~ "Saxifraga hirsuta",
      Especie.planta == "Saxifraga sp" ~ "Saxifraga sp.",
      Especie.planta == "Blanca estrellada" ~ "Stellaria sp.",
      Especie.planta == "Teucrium pyrenaicum L. subsp. pyrenaicum" ~ "Teucrium pyrenaicum subsp. Pyrenaicum",
      Especie.planta == "Vicia pyrenaica​" ~ "Vicia pyrenaica",
      TRUE ~ Especie.planta  # Keep other values unchanged
    )
  )

unique_sorted <- gorbea %>%
  distinct(Especie.planta) %>%  # Obtiene valores únicos en ColumnName
  arrange(Especie.planta)



str(doñana$Fecha)
doñana <- full_join(d,
                    d.21,
                    by = c("Fecha", "Bosque", "Especie.planta", "Numero.flores", "Periodo"))
unique_sorted <- doñana %>%
  distinct(Especie.planta) %>%  # Obtiene valores únicos en ColumnName
  arrange(Especie.planta)

doñana <- doñana %>%
  mutate(
    Especie.planta = case_when(
      Especie.planta == "Cistus salvifolius" ~ "Cistus salviifolius",
      Especie.planta == "cistus albidus" ~ "Cistus albidus",
      Especie.planta == "cistus crispus" ~ "Cistus crispus",
      Especie.planta == "cistus libanotis" ~ "Cistus libanotis",
      Especie.planta == "halimium commutatum" ~ "Halimium commutatum",
      Especie.planta == "Rosmarinus offincinalis" ~ "Rosmarinus officinalis",
      Especie.planta == "halimium halimifolium" ~ "Halimium halimifolium",
      Especie.planta == "rosmarinus officinales" ~ "Rosmarinus officinalis",
      TRUE ~ Especie.planta  # Keep other values unchanged
    )
  )

doñana$Fecha<-dmy(doñana$Fecha)
str(doñana$Fecha)
gorbea$Bosque <- as.character(gorbea$Bosque)
resources <- bind_rows(gorbea, doñana)
write.csv(resources, "data/all_floral_resources.csv")
