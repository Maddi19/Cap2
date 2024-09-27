rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,ourr,iNEXT,wesandersom,
               here,plyr, lme4, carData,effects,
               performance,see,gridExtra,car, lattice,ggplot2,bipartite)



sitems <- read.csv("data/SITE_network_level_metrics.csv", sep=",")
sitems1 <- read.csv("data/SITE_species_level_metrics.csv")
sitems2 <- read.csv("data/SITE_plant_species_level_metrics.csv")



#calculate total number of visits per plant sps
##cargar all data
all_df <- read.csv("./data/all_data.csv")

pl.vis <- all_df %>%
  dplyr::group_by(Site, Year, Bosque, Periodo, Planta) %>%
  dplyr::summarize(tot.visits.pl = n(), .groups = 'drop')

all_df_with_visits <- all_df %>%
  dplyr::left_join(pl.vis, by = c("Site", "Year", "Bosque", "Periodo", "Planta"))


all_df_with_visits$j <-
  paste(
    all_df_with_visits$Site,
    all_df_with_visits$Year,
    all_df_with_visits$Bosque,
    all_df_with_visits$Periodo,
    all_df_with_visits$Planta,
    sep = "_"
  )

###add to plant species network data
sitems2$match <-
  paste(sitems2$Site,
        sitems2$Year,
        sitems2$Bosque,
        sitems2$Periodo,
        sitems2$Especies,
        sep = "_")

sitems2$tot.visits.pl <-
  all_df_with_visits$tot.visits.pl[match(sitems2$match, all_df_with_visits$j)]
sitems2$tot.visits.pl[is.na(sitems2$tot.visits.sinout)] <- 0
write.csv(sitems2, "data/sitems2_totvisits.csv")

####reproductive success data

all_df <- read.csv("./data/all_data.csv")

d21.rs <- read.csv("./data/rep.suc/fruit_set_D2021.csv")
d21.fs <- d21.rs %>%
  mutate(
    fruit_set = frutos.recogidos / flores.etiquetadas,
    Year = 2021,
    Site = "Doñana"
  )
d21.fs$Periodo <- d21.fs$ronda

d21.meanfs <- d21.fs %>%
  group_by(Periodo, Bosque, Year, Site) %>%
  dplyr::summarise(mean.fs = mean(fruit_set, na.rm = TRUE))


g21.rs <- read.csv("./data/rep.suc/frutos_G21.csv", sep = ";")
g21.rs$Fecha_flores <- as.character(g21.rs$Fecha_flores)
g21.rs$Fecha_flores <- dplyr::recode(g21.rs$Fecha_flores,
                                     "31/05/1931" = "31/05/2021",
                                     "11/05/2027" = "11/05/2021")


#create column periodo
require(lubridate)
#especificar que es  una fecha
g21.rs$Fecha2 <- dmy(g21.rs$Fecha_flores)
g21.rs$Fecha2 <- as.Date(g21.rs$Fecha2, format = "%Y-%m-%d")

##añadir columna periodo
g21 <- all_df %>%
  filter(Site == "Gorbea" & Year == "2021")
g21$Fecha2 <- as.Date(g21$Fecha2)

g21.rs$Periodo <- g21$Periodo[match(g21.rs$Fecha2, g21$Fecha2)]

g21.rs <- g21.rs %>%
  mutate(Periodo = if_else(Fecha2 == as.Date('2021-05-19'), 4, Periodo)) %>%
  mutate(Periodo = if_else(Fecha2 == as.Date('2021-04-22'), 1, Periodo)) %>%
  mutate(Periodo = if_else(Fecha2 == as.Date('2021-05-11'), 2, Periodo)) %>%
  mutate(Periodo = if_else(Fecha2 == as.Date('2021-06-12'), 6, Periodo)) %>%
  mutate(Periodo = if_else(Fecha2 == as.Date('2021-06-13'), 6, Periodo)) %>%
  mutate(Periodo = if_else(Fecha2 == as.Date('2021-06-14'), 4, Periodo))


g21.fs <- g21.rs %>%
  filter(!(is.na(
    Frutos_cuajados..ultimo.dato.registrado.
  ))) %>%
  dplyr::group_by(Punto,Periodo,Planta, num) %>%
  summarise(
    total_frutos_cuajados = first(Frutos_cuajados..ultimo.dato.registrado.),
    total_flores_yemas = first(Flores_yemas_totales),
    Year = 2021,
    Site = "Gorbea",
    Periodo = first(Periodo),
    Trat = first(Trat),
    Bosque = first(Punto)) %>%
  mutate(
    fruit_set = total_frutos_cuajados / total_flores_yemas,
    fruit_set = if_else(
      Planta %in% c("Hepatica nobilis", "Erysimum gorbeanum", "Pedicularis sylvatica", "Hutchinsia alpina",
                    "Scilla verna", "Primula veris", "Helianthemum nummularium") & fruit_set > 1,
      1,
      fruit_set
    )
  ) 

g21.fs <- g21.fs %>%
  mutate(
    fruit_set = case_when(
      Planta == "Helleborus viridis" ~ fruit_set / 5,
      TRUE ~ fruit_set  # Mantiene los valores de fruit_set ya calculados para otras especies
    )
  )

g21.meanfs <- g21.fs %>%
  filter(!Trat == "Embolsada") %>%
  group_by(Periodo, Bosque, Year, Site) %>%
  dplyr::summarise(mean.fs = mean(fruit_set, na.rm = TRUE))



library(readxl)
g22.rs <- read_xlsx("./data/rep.suc/frutos_gorbea222.xlsx")
g22.rs <- g22.rs %>%
  dplyr::rename(Flores = Flores_yemas_totales,
                Frutos_cuajados = `Frutos_cuajados (ultimo dato registrado)`)

g22.rs$Frutos_cuajados <-as.numeric(g22.rs$Frutos_cuajados)
g22.rs$Flores <-as.numeric(g22.rs$Flores)

g22.rs$Periodo <- g22.rs$Ronda
unique(g22.rs$Periodo)
g22.rs$Periodo <- as.character(g22.rs$Periodo)
g22.rs$Periodo <- dplyr::recode(g22.rs$Periodo,"18"= "1")
g22.rs$Bosque <- g22.rs$Punto

g22.fs <- g22.rs %>%
  filter(!(is.na(
    Frutos_cuajados
  ))) %>%
  dplyr::group_by(Bosque, Periodo,`Especie planta`, num) %>%
  summarise(
    total_frutos_cuajados = first(Frutos_cuajados),
    total_flores = first(Flores),
    Year = 2022,
    Site = "Gorbea",
    Periodo = first(Periodo),
    Embolsada = first(Embolsada),
    Bosque = first(Bosque)) %>%
  mutate(
    fruit_set = total_frutos_cuajados / total_flores,
    fruit_set = if_else(
      `Especie planta` %in% c("Hepatica nobilis", "Erysimum gorbeanum", "Pedicularis sylvatica", "Hutchinsia alpina",
                    "Scilla verna", "Primula veris", "Helianthemum nummularium") & fruit_set > 1,
      1,
      fruit_set
    )
  ) 

g22.fs <- g22.fs %>%
  mutate(
    fruit_set = case_when(
      `Especie planta` == "Helleborus viridis" ~ fruit_set / 5,
      TRUE ~ fruit_set  # Mantiene los valores de fruit_set ya calculados para otras especies
    )
  )

                                
g22.meanfs <- g22.fs %>%
  filter(Embolsada == "no") %>%
  group_by(Periodo, Bosque, Year, Site) %>%
  dplyr::summarise(mean.fs = mean(fruit_set, na.rm = TRUE))


g22.meanfs$Bosque <- as.character(g22.meanfs$Bosque)
g21.meanfs$Bosque <- as.character(g21.meanfs$Bosque)
g21.meanfs$Periodo <- as.character(g21.meanfs$Periodo)
d21.meanfs$Periodo <- as.character(d21.meanfs$Periodo)

fruit_set <- rbind (d21.meanfs, g21.meanfs, g22.meanfs)
unique(fruit_set$Bosque)
fruit_set$Bosque <- dplyr::recode(fruit_set$Bosque,"Pinar Villamanrique Este"= "Villamanrique Chaparral",
                           "Pinar Villamanrique Sur"="Villamanrique Sur")
fruit_set$match <-
  paste(fruit_set$Site,
        fruit_set$Year,
        fruit_set$Bosque,
        fruit_set$Periodo,
        sep = "_")  

##fruit set per plant sp
d21.pl.fs <- d21.fs %>%
  group_by(Periodo, Bosque, Year, Site, SP) %>%
  dplyr::summarise(pl.mean.fs = mean(fruit_set, na.rm = TRUE))

g21.pl.fs <- g21.fs %>%
  filter(!Trat == "Embolsada") %>%
  group_by(Periodo, Bosque, Year, Planta) %>%
  dplyr::summarise(pl.mean.fs = mean(fruit_set, na.rm = TRUE))


g22.pl.fs <- g22.fs %>%
  filter(Embolsada == "no") %>%
  group_by(Periodo, Bosque, Year, Site, `Especie planta`) %>%
  dplyr::summarise(pl.mean.fs = mean(fruit_set, na.rm = TRUE))

d21.pl.fs <- d21.pl.fs %>%
  rename(Planta=SP)
g22.pl.fs <- g22.pl.fs %>%
  rename(Planta=`Especie planta`)
g21.pl.fs <- g21.pl.fs %>%
  mutate(Site="Gorbea")


g21.pl.fs$Bosque <- as.character(g21.pl.fs$Bosque)
g22.pl.fs$Bosque <- as.character(g22.pl.fs$Bosque)
g21.pl.fs$Periodo <- as.character(g21.pl.fs$Periodo)
g22.pl.fs$Periodo <- as.character(g22.pl.fs$Periodo)
d21.pl.fs$Periodo <- as.character(d21.pl.fs$Periodo)
pl.fruit_set <- rbind (d21.pl.fs, g21.pl.fs, g22.pl.fs)

pl.fruit_set$Bosque <- dplyr::recode(pl.fruit_set$Bosque,"Pinar Villamanrique Este"= "Villamanrique Chaparral",
                                  "Pinar Villamanrique Sur"="Villamanrique Sur")
pl.fruit_set$match <-
  paste(pl.fruit_set$Site,
        pl.fruit_set$Year,
        pl.fruit_set$Bosque,
        pl.fruit_set$Periodo,
        sep = "_")  

sitems2$Bosque <- as.character(sitems2$Bosque)
sitems2$match <-
  paste(sitems2$Site_id,
        sitems2$Year,
        sitems2$Bosque,
        sitems2$Periodo,
        sep = "_")

sitems2$pl.mean.fs<-pl.fruit_set$pl.mean.fs[match(sitems2$match, pl.fruit_set$match)]
write.csv(sitems2, "data/sitems2_totvisits.fs.csv")

####COMMUNITY NETWORK ANALYSIS
sitems$Bosque <- as.character(sitems$Bosque)
sitems$match <-
  paste(sitems$Site_id,
        sitems$Year,
        sitems$Bosque,
        sitems$Periodo,
        sep = "_")

sitems$mean.fs<-fruit_set$mean.fs[match(sitems$match, fruit_set$match)]

tot.visits<- pl.vis %>%
  dplyr::group_by(Site,Year,Bosque, Periodo) %>%
  dplyr::summarise(tot.vis = sum(tot.visits.pl))

tot.visits$match <-
  paste(tot.visits$Site,
        tot.visits$Year,
        tot.visits$Bosque,
        tot.visits$Periodo,
        sep = "_")

sitems$total.visits<-tot.visits$tot.vis[match(sitems$match, tot.visits$match)]

write.csv(sitems, "data/sitems_meanfs.csv")

#scale all variables to be able to compare effect sizes 
sitems$Anidamiento <- scale(sitems$Anidamiento , center = T, scale = T)
sitems$comp.fun.pol <- scale(sitems$comp.fun.pol  , center = T, scale = T)
sitems$comp.fun.pl <- scale(sitems$comp.fun.pl  , center = T, scale = T)
sitems$poll.sp <- scale(sitems$poll.sp  , center = T, scale = T)
sitems$plant.sp <- scale(sitems$plant.sp  , center = T, scale = T)
sitems$total.visits <- scale(sitems$total.visits  , center = T, scale = T)
sitems$total.visits <- scale(sitems$total.visits  , center = T, scale = T)
