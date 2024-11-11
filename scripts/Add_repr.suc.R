rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,purr,iNEXT,wesanderson,
               here,plyr, lme4, carData,effects,
               performance,see,gridExtra,car, lattice,ggplot2,bipartite)



sitems <- read.csv("data/SITE_network_level_metrics.csv", sep=",")
sitems1 <- read.csv("data/SITE_species_level_metrics.csv")
sitems2 <- read.csv("data/SITE_plant_species_level_metrics.csv")



#calculate total number of visits per plant sps
##cargar all data
all_df <- read.csv("./data/useful/all_data.csv")

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

plants <- sitems2%>%
  distinct(Especies) %>%    
  arrange(Especies)        
print(plants)



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

all_df <- read.csv("./data/useful/all_data.csv")

d21.rs <- read.csv("./data/rep.suc/fruit_set_D2021.csv")
d21.fs <- d21.rs %>%
  mutate(
    fruit_set = frutos.recogidos / flores.etiquetadas,
    Year = 2021,
    Site = "Doñana"
  )
d21.fs$Periodo <- d21.fs$ronda
d21.fs$Planta <- d21.fs$SP

d21.meanfs <- d21.fs %>%
  group_by(Periodo, Bosque, Year, Site) %>%
  dplyr::summarise(mean.fs = mean(fruit_set, na.rm = TRUE),
                   n = n(),
                   se.fs = sd(fruit_set)/sqrt(n))

unique(d21.meanfs$Bosque)

library(readxl)
d21.seed <- read_xlsx("./data/rep.suc/DoñanaFrutos2021.xlsx")

d21.seed <- d21.seed %>%
  mutate(Year ="2021",Site="Doñana", Periodo= `Ronda marcados`, Bosque = Punto)%>%
  group_by(Planta) %>%
  mutate(scaled_seeds = scale(`Semillas totales`)) %>%
  ungroup() %>%
  select(Planta, Periodo, Bosque, Year, Site, `Semillas totales`, scaled_seeds)

unique(d21.seed$Bosque)

d21.seed <- d21.seed %>%
  mutate(Bosque = dplyr::recode(Bosque,
                         "Puebla" = "Pinar Puebla",
                         "Hinojos 2021" = "Pinar Hinojos",
                         "Aznalcazar" = "Pinar Aznalcazar",
                         "Villa Manrique Sur" = "Pinar Villamanrique Sur",
                         "Villa Manrique Este" = "Pinar Villamanrique Este"))

d21.meanseed <- d21.seed %>%
  group_by(Periodo, Bosque, Year, Site) %>%
  dplyr::summarise(mean.sn = mean(scaled_seeds, na.rm = TRUE),
                   n = n(),
                   se.sn = sd(scaled_seeds)/sqrt(n()))
str(d21.meanseed)
d21.meanseed$Periodo <- as.integer(d21.meanseed$Periodo)
d21.meanseed$Year <- as.numeric(d21.meanseed$Year)
d21.meanseed <- d21.meanseed %>% 
  select(-n)
d21.meanfs <- d21.meanfs %>%
  select(-n)

d21.repro <- left_join(d21.meanfs, d21.meanseed)


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



hut <- g21.fs %>%
  filter(str_starts(Planta, "Hutchinsia") | str_starts(Planta, "Helleborus"))


g21.rs <- g21.rs %>%
  mutate(Semillas_viables = case_when(
    str_starts(Planta, "Hutchinsia") ~ Semillas_viables / Frutos_cuajados..ultimo.dato.registrado.,
    str_starts(Planta, "Helleborus") & Semillas_viables == 33 ~ Semillas_viables / 5,
    TRUE ~ Semillas_viables  # En todos los demás casos, no cambia el valor
  ))

g21.fs <- g21.rs %>%
  filter(!(is.na(
    Frutos_cuajados..ultimo.dato.registrado.
  ))) %>%
  group_by(Planta) %>%
  mutate(scaled_seeds = scale(Semillas_viables)) %>%
  ungroup()

g21.fs <- g21.fs %>%
  dplyr::rename(Frutos_cuajados = Frutos_cuajados..ultimo.dato.registrado.)
  
g21.fs <- g21.fs %>%
  mutate(
    total_flores_yemas = Flores_yemas_totales,
    Semillas_viables = Semillas_viables,
    Year = 2021,
    Site = "Gorbea",
    scaled_seeds = scaled_seeds,
    Periodo = Periodo,
    Trat = Trat,
    Bosque = Punto,
    fruit_set = Frutos_cuajados / total_flores_yemas,
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


g21.fs$Planta <- dplyr::recode(g21.fs$Planta, "Vicia Pyrenaica" = "Vicia pyrenaica")


g21.mean.fs <- g21.fs %>%
  filter(!Trat == "Embolsada") %>%
  group_by(Periodo, Bosque, Year, Site) %>%
  dplyr::summarise(mean.fs = mean(fruit_set, na.rm = TRUE),
                   n = n(),
                   se.fs = sd(fruit_set)/sqrt(n))

g21.mean.sn <- g21.fs %>%
  filter(!Trat == "Embolsada") %>%
  filter(!(is.na(scaled_seeds)))%>%
  group_by(Periodo, Bosque, Year, Site) %>%
  dplyr::summarise(mean.sn = mean(scaled_seeds, na.rm = TRUE),
                   n = n(),
                   se.sn = sd(scaled_seeds)/sqrt(n))

gorbea2021_mean.rep<- g21.mean.fs %>%
  left_join(g21.mean.sn, by = c("Year", "Bosque", "Periodo", "Site"))




library(readxl)
g22.rs <- read_xlsx("./data/rep.suc/frutos_gorbea222.xlsx")
g22.rs <- g22.rs %>%
  dplyr::rename(Flores = Flores_yemas_totales,
                Planta = `Especie planta`,
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
  group_by(Planta) %>%
  mutate(scaled_seeds = scale(Semillas_viables)) %>%
  ungroup()

g22.fs <- g22.fs %>%
  mutate(
    Flores = Flores,
    Semillas_viables = Semillas_viables,
    Year = 2022,
    Site = "Gorbea",
    scaled_seeds = scaled_seeds,
    Periodo = Periodo,
    Embolsada = Embolsada,
    Bosque = Punto,
    fruit_set = Frutos_cuajados / Flores,
    fruit_set = if_else(
      Planta %in% c("Hepatica nobilis", "Erysimum gorbeanum", "Pedicularis sylvatica", "Hutchinsia alpina",
                    "Scilla verna", "Primula veris", "Helianthemum nummularium") & fruit_set > 1,
      1,
      fruit_set
    )
  ) 

g22.fs <- g22.fs %>%
  mutate(
    fruit_set = case_when(
      Planta == "Helleborus viridis" ~ fruit_set / 5,
      TRUE ~ fruit_set  # Mantiene los valores de fruit_set ya calculados para otras especies
    )
  )


g22.mean.fs <- g22.fs %>%
  filter(Embolsada == "no") %>%
  group_by(Periodo, Bosque, Year, Site) %>%
  dplyr::summarise(mean.fs = mean(fruit_set, na.rm = TRUE),
                   n = n(),
                   se.fs = sd(fruit_set)/sqrt(n))

g22.mean.sn <- g22.fs %>%
  filter(Embolsada == "no")%>%
  filter(!(is.na(scaled_seeds)))%>%
  group_by(Periodo, Bosque, Year, Site) %>%
  dplyr::summarise(mean.sn = mean(scaled_seeds, na.rm = TRUE),
                   n = n(),
                   se.sn = sd(scaled_seeds)/sqrt(n))

gorbea2022_mean.rep<- g22.mean.fs %>%
  left_join(g22.mean.sn, by = c("Year", "Bosque", "Periodo", "Site"))


                              

gorbea2022_mean.rep$Bosque <- as.character(gorbea2022_mean.rep$Bosque)
gorbea2021_mean.rep$Bosque <- as.character(gorbea2021_mean.rep$Bosque)
gorbea2021_mean.rep$Periodo <- as.character(gorbea2021_mean.rep$Periodo)
d21.repro$Periodo <- as.character(d21.repro$Periodo)



fruit_set <- rbind (d21.repro, gorbea2021_mean.rep, gorbea2022_mean.rep)
unique(fruit_set$Bosque)
fruit_set$Bosque <- dplyr::recode(fruit_set$Bosque,"Pinar Villamanrique Este"= "Villamanrique Chaparral",
                          "Pinar Villamanrique Sur"="Villamanrique Sur")

fruit_set$Bosque <- as.character(fruit_set$Bosque)
fruit_set$Year <- as.factor(fruit_set$Year)
fruit_set$Periodo<- as.factor(fruit_set$Periodo)
fruit_set$match <-
  paste(fruit_set$Site,
        fruit_set$Year,
        fruit_set$Bosque,
        fruit_set$Periodo,
        sep = "_")  

##fruit set per plant sp
sitems2 <- read.csv("data/sitems2_totvisits.csv")

d21.fs.pl <- d21.fs %>%
  group_by(Periodo, Bosque, Year, Site, Planta) %>%
  dplyr::summarise(pl.mean.fs = mean(fruit_set, na.rm = TRUE),
                   n = n(),
                   se.pl.fs = sd(fruit_set)/sqrt(n))
d21.fs.pl$Year <- as.character(d21.fs.pl$Year)
d21.fs.pl <- d21.fs.pl %>%
  select(-n)

d21.seed.pl <- d21.seed %>%
  group_by(Periodo, Bosque, Year, Site, Planta) %>%
  dplyr::summarise(pl.mean.sn = mean(scaled_seeds, na.rm = TRUE),
                   n = n(),
                   se.pl.sn = sd(scaled_seeds)/sqrt(n))

d21.seed.pl <- d21.seed.pl %>%
  select(-n)

d21.pl.repro <- left_join(d21.fs.pl, d21.seed.pl)


g21.pl.fs <- g21.fs %>%
  filter(!Trat == "Embolsada") %>%
  group_by(Periodo, Bosque, Year, Planta) %>%
  dplyr::summarise(pl.mean.fs = mean(fruit_set, na.rm = TRUE),
                   n = n(),
                   se.fs = sd(fruit_set)/sqrt(n))

g21.pl.sn <- g21.fs %>%
  filter(!Trat == "Embolsada") %>%
  filter(!(is.na(scaled_seeds)))%>%
  group_by(Periodo, Bosque, Year, Planta) %>%
  dplyr::summarise(pl.mean.sn = mean(scaled_seeds, na.rm = TRUE),
                   n = n(),
                   se.sn = sd(scaled_seeds)/sqrt(n))
                   
gorbea2021_combined <- g21.pl.fs %>%
  left_join(g21.pl.sn, by = c("Year", "Bosque", "Periodo", "Planta"))


g22.pl.fs <- g22.fs %>%
  filter(Embolsada == "no") %>%
  group_by(Periodo, Bosque, Year, Planta) %>%
  dplyr::summarise(pl.mean.fs = mean(fruit_set, na.rm = TRUE),
                   n = n(),
                   se.fs = sd(fruit_set)/sqrt(n))

g22.pl.sn <- g22.fs %>%
  filter(Embolsada == "no") %>%
  filter(!(is.na(scaled_seeds)))%>%
  group_by(Periodo, Bosque, Year, Planta) %>%
  dplyr::summarise(pl.mean.sn = mean(scaled_seeds, na.rm = TRUE),
                   n = n(),
                   se.sn = sd(scaled_seeds)/sqrt(n))

gorbea2022_combined <- g22.pl.fs %>%
  left_join(g22.pl.sn, by = c("Year", "Bosque", "Periodo", "Planta"))

gorbea2021_combined<- gorbea2021_combined %>%
  mutate(Site="Gorbea")


gorbea2021_combined$Bosque <- as.character(gorbea2021_combined$Bosque)
gorbea2022_combined$Bosque <- as.character(gorbea2022_combined$Bosque)
gorbea2021_combined$Periodo <- as.character(gorbea2021_combined$Periodo)
gorbea2022_combined$Periodo <- as.character(gorbea2022_combined$Periodo)
d21.pl.repro$Periodo <- as.character(d21.pl.repro$Periodo)
d21.pl.repro$Year <- as.numeric (d21.pl.repro$Year)

pl.fruit_set <- rbind (d21.pl.repro, gorbea2021_combined, gorbea2022_combined)

pl.fruit_set$Bosque <- dplyr::recode(pl.fruit_set$Bosque,"Pinar Villamanrique Este"= "Villamanrique Chaparral",
                                  "Pinar Villamanrique Sur"="Villamanrique Sur")
pl.fruit_set$match <-
  paste(pl.fruit_set$Site,
        pl.fruit_set$Year,
        pl.fruit_set$Bosque,
        pl.fruit_set$Periodo,
        pl.fruit_set$Planta,
        sep = "_")  

unique(pl.fruit_set$Planta)

sitems2 <- read.csv("./data/sitems2_totvisits.csv")
unique(sitems2$Especies)

sitems2$Bosque <- as.character(sitems2$Bosque)
sitems2 <- sitems2 %>%
  dplyr::rename(Planta = Especies)

sitems2$match <-
  paste(sitems2$Site_id,
        sitems2$Year,
        sitems2$Bosque,
        sitems2$Periodo,
        sitems2$Planta,
        sep = "_")

sitems2$pl.mean.fs<-pl.fruit_set$pl.mean.fs[match(sitems2$match, pl.fruit_set$match)]
sitems2$se.pl.fs<-pl.fruit_set$se.pl.fs[match(sitems2$match, pl.fruit_set$match)]
sitems2$pl.mean.sn<-pl.fruit_set$pl.mean.sn[match(sitems2$match, pl.fruit_set$match)]
sitems2$se.pl.sn<-pl.fruit_set$se.pl.sn[match(sitems2$match, pl.fruit_set$match)]


write.csv(sitems2, "data/useful/sitems2_totvisits.fs.csv")

####COMMUNITY NETWORK ANALYSIS

sitems$Bosque <- as.character(sitems$Bosque)
sitems$Year<- as.factor(sitems$Year)
sitems$Periodo<- as.factor(sitems$Periodo)

sitems$match <-
  paste(sitems$Site_id,
        sitems$Year,
        sitems$Bosque,
        sitems$Periodo,
        sep = "_")

unique(sitems$match)
unique(fruit_set$match)

sitems <- sitems %>%
  left_join(fruit_set %>% select(match, se.fs, mean.sn, se.sn), by = "match") %>%
  filter(match %in% unique(sitems$match))

##edo hola:
sitems$mean.fs<-fruit_set$mean.fs[match(sitems$match, fruit_set$match)]
sitems$se.fs<-fruit_set$se.fs[match(sitems$match, fruit_set$match)]

sitems$mean.sn<-fruit_set$mean.sn[match(sitems$match, fruit_set$match)]
sitems$se.sn<-fruit_set$se.sn[match(sitems$match, fruit_set$match)]


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
unique(sitems$mean.fs)
write.csv(sitems, "data/useful/sitems_meanrepro.csv")

