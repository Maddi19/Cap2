########CLEAN GORBEA 2022 DATA#########
#######################################
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(dplyr, tidyverse, tidyr, readxl,readr, metafor) 


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

d.g2$Polinizador_curro<-recode(d.g2$Polinizador_curro, "?=28"= "Scaeva pyrastri",
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
                         "?=123"= "Halictus sp",
                         "Pieris sp. Argazkia"="Pieris napi",
                         "Pieris sp. Puntu beltzarekin"="Pieris brassicae",
                         "Tximeleta marroi laranja bi begi handi"="Maniola jurtina",
                         " mariposa peque\xf1a gris con puntos"="Pyrgus sp.")
                         
                           
#remove some with no id
d.g3<-d.g2[!(d.g2$Polinizador_curro=="?=1"|d.g2$Polinizador_curro=="?=3"|d.g2$Polinizador_curro=="?=9"|d.g2$Polinizador_curro=="11"|d.g2$Polinizador_curro=="21"|
               d.g2$Polinizador_curro=="31"|d.g2$Polinizador_curro=="?=32"),]

                           
###create genus column
library("stringr")
d.g4<- d.g3%>%
  mutate(Pollinator_genus= word(Polinizador_curro, 1))
unique(d.g4$Pollinator_genus)    

###family names
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Apis"] <- "Apidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Syrphidae"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Bombylius"] <- "Bombyliidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Maniola"] <- "Nymphalidae-Satyrinae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Zygaena"] <- "Zygaenidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Macroglossum"] <- "Esfingidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Bombus"] <- "Apidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Episyrphus"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Ochlodes"] <- "Hesperiidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Erebia"] <- "Nymphalidae-Satyrinae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Cupido"] <- "Lycaenidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Pyrgus"] <- "Hesperiidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Pieris"] <- "Pieridae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Pararge"] <- "Nymphalidae-Satyrinae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Syrphus"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Sphaerophoria"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Merodon"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Eupeodes"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Eristalis"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Melangyna"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Antocharis"] <- "Pieridae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Andrena"] <- "Andrenidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Eucera"] <- "Apidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Osmia"] <- "Megachilidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Lassiomata"] <- "Nymphalidae-Satyrinae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Cheilosia"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Halictus"] <- "Halictidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Calliphora"] <- "Calliphoridae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Cheilosia"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Platycheirus"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Coenonympha"] <- "Nymphalidae-Satyrinae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Nomada"] <- "Apidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Oedemera"] <- "Oedemeridae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Lucilia"] <- "Calliphoridae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Psithyrus"] <- "Apidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Anthophora"] <- "Apidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Scaeva"] <- "Syrphidae"
d.g4$Pollinator_family[d.g4$Pollinator_genus=="Lasioglossum"] <- "Halictidae"

unique(d.g4$Pollinator_genus)
unique(d.g4$Pollinator_family)
unique(d.g4$Planta)
unique(d.g4$Ronda)
unique(d.g4$Polinizador_curro)

write.csv(d.g4, "data/clean/trans_gorbea_20_clean.csv")
clean_dg<-read.csv("data/clean/trans_gorbea_20_clean.csv")

unique(clean_dg$Polinizador_curro)



                         
                           
                           
                        



