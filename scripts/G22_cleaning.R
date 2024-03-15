
rm(list = ls(all.names = TRUE)) #Limpiar objetos ocultos
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #Limpiar paquetes ocultos
pacman::p_load(dplyr, tidyverse, tidyr, readxl,readr, metafor) 

########CLEAN GORBEA 2022 DATA#########
#######################################

d.g<-read.csv("data/Datos_transecto_2022.csv")
head(d.g)
str(d.g)
d.g<-d.g[,1:11]
head(d.g)
d.g$Polinizador<-as.character(d.g$Polinizador)
unique(d.g$Polinizador)

#ids curro
id<-read.csv("data/identificaciones2022.csv", sep=";")
library(lubridate)
#especificar que es  una fecha
d.g$Fecha<-dmy(d.g$Fecha)
id$Fecha<-dmy(id$Fecha)

id$b<-paste(id$Fecha, id$Numero.ID)
d.g$b<-paste(d.g$Fecha, d.g$Codigo_frasco)

d.g$Polinizador_curro<-id$Species.ID[match(d.g$b, id$b)]

d.g2 <- d.g%>% mutate(Polinizador_curro = ifelse(is.na(Polinizador_curro),Polinizador,Polinizador_curro))
head(d.g2)
unique(d.g2$Polinizador)

d.g2$Polinizador<-recode(d.g2$Polinizador, "?=28"= "Scaeva pyrastri",
                         "?=4" ="Eupeodes corollae",
                         "?=6"="Eupeodes corollae",
                         "?=7"="Calliphora vicina",
                         "?=8" ="Bombus ruderarius",
                         "?=10"="Platycheirus albimanus",
                         "?=12"="Andrena limata",
                         "?=13"= "Halictus smaragdulus",
                         "?=34" = "Lasioglossum leucopus",
                         "?=17"="Eupeodes corollae",
                         "?=20" = "Andrena propinqua",
                         "?=18" = "Bombus soroeensis",
                         "?=60" = "Eupeodes corollae",
                         "?=30"= "Eupeodes corollae",
                         "?=24"= "Andrena nigroaenea",
                         "?=29"= "Cheilosia velutina",
                         "?=37" = "Bombus soroeensis",
                         "?=35"="Apis mellifera",
                         "?=36"="Apis mellifera",
                         "?=41"="Andrena nigroaenea",
                         "?=44"="Lasioglossum sp",
                         "?=45"="Andrena nigroaenea",
                         "?=49" = "Platycheirus albimanus",
                         "?=52" = "Calliphora vicina",
                         "?=54"= "Lasioglossum immunitum",
                         "?=56"="Bombus ruderarius",
                         "?=57"= "Andrena cineraria",
                         "?=58"="Cheilosia nigripes",
                         "?=61" ="Lasioglossum sp",
                         "?=66" ="Andrena propinqua",
                         "?=64"= "Merodon sp.",
                         "?=68" = "Melangyna sp.",
                         "?=59" = "Andrena propinqua",
                         "?=75" = "Eucera nigrescens",
                         "?=73" = "Andrena cineraria",
                         "?=80" = "Lasioglossum sp",
                         "?=89" = "Apis mellifera",                              
                         "?=91" = "Osmia emarginata",                                 
                         "?=93" = "Andrena ovatula",                                 
                         "?=94" = "Halictus confusus",
                         "?=97" = "Platycheirus albimanus",
                         "?=99" = "Syrphidae",
                         "?=100"= "Syrphus sp.",
                         "?=106"="Bombus sylvarum",
                         "?=110"="Andrena minutula",
                         "?=115"="Andrena ovatula",
                         "?=116"=" Syrphus sp.",
                         "?=118"="Bombus hortorum",
                         "?=123"= "Halictus sp")
                         
                           
#remove some with no id
d.g3<-d.g2[!(d.g2$Polinizador=="?=1"|d.g2$Polinizador=="?=3"|d.g2$Polinizador=="?=9"|d.g2$Polinizador=="11"|d.g2$Polinizador=="21"|
               d.g2$Polinizador=="31"|d.g2$Polinizador=="?=32"),]

                           
###create genus column
library("stringr")
d.g4<- d.g3%>%
  mutate(Pollinator_genus= word(Polinizador, 1))
unique(d.g4$Pollinator_genus)    

###family names
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Apis"] <- "Apidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Syrphidae"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Bombylius"] <- "Bombyliidae"

                         
                           
                           
                        



