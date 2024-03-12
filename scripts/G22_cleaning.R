
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
                         
                        



