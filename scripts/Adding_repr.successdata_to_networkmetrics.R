rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,purrr,iNEXT,wesanderson,
               here,plyr, lme4, carData,effects,
               performance,see,gridExtra,car, lattice,ggplot2,bipartite)


###ADD REPRODUCTIVE SUCCESS DATA TO NETWORK METRICS
sitems <- read.csv("data/SITE_network_level_metrics.csv", sep=",")
sitems1 <- read.csv("data/SITE_species_level_metrics.csv")
sitems2 <- read.csv("data/SITE_plant_species_level_metrics.csv")
sitems3 <- read.csv("SITE_plant_species_level_metrics_sinout.csv")

sitems3 <- sitems3 %>%
  mutate(Species = case_when(Species =="	Thymus praecox subsp. polytrichus" ~ "Thymus praecox subsp. polytrichus",
                            TRUE ~ Species))
         


#calculate total number of visits per plant sps
##cargar all data
all_df <- read.csv("./data/useful/all_data.csv")

horas <- all_df %>%
  group_by(Site,Year,Bosque,Periodo)%>%
  summarise(horas_dia = n_distinct(Transecto))%>%
  ungroup()%>%
  group_by(Site)%>%
  summarise(horas=sum(horas_dia))

#### BETA DIVERSITY DE ESPECIES DE PLANTAS
flor_reso<- read.csv("./data/useful/all_floral_resources.csv")
flor_reso <- flor_reso %>%
  mutate(
    Site = ifelse(str_detect(Bosque, "^[0-9]"), "Gorbea", "Doñana"),
    Fecha = ymd(Fecha),
    Year = year(Fecha)
  )

flor_reso <- flor_reso %>%
  mutate(
    Bosque = case_when(
      Bosque == "Pinar Villamanrique Este (Chaparral)" ~ "Villamanrique Chaparral",
      Bosque =="Pinar Villamanrique Sur" ~ "Villamanrique Sur",
      Bosque == "Pinares Hinojos" ~ "Pinar Hinojos",
      Bosque == "Pinares aznalcazar" ~ "Pinar Aznalcazar",
      Bosque == "Pinares Puebla" ~ "Pinar Puebla",
      Bosque == "Pinares de Hinojos" ~ "Pinar Hinojos",
      Bosque == "Villamanrique Chaparrral" ~ "Villamanrique Chaparral",
      Bosque == "Villamanrique chaparral" ~ "Villamanrique Chaparral",
      TRUE ~ Bosque  # Keep all other values as they are
    )
  )
unique(flor_reso$Bosque)
write.csv(flor_reso,"./data/useful/all_floral_resources.csv")

glimpse(flor_reso)

f.r.g <- flor_reso %>%
  filter(Site =="Gorbea")
f.r.d <- flor_reso %>%
  filter(Site =="Doñana")


div <- f.r.g%>%
  group_by(Bosque, Especie.planta) %>%
  summarise(interacciones = sum(Numero.flores, na.rm = TRUE)) %>%  
  ungroup()

div <- div %>%
  filter(!Especie.planta=="")


div.d <- f.r.d %>%
  filter(!Especie.planta=="")

div.d <- div.d%>%
  group_by(Bosque, Especie.planta) %>%
  summarise(interacciones = sum(Numero.flores, na.rm = TRUE)) %>%  
  ungroup()

div.d <- div.d %>%
  group_by(Bosque) %>%
  mutate(
    Bosque_Numerico = cur_group_id()
  ) %>%
  ungroup() %>%
  mutate(
    Bosque = as.character(Bosque_Numerico) # Convertimos a string para mantener la consistencia con la columna original
  ) %>%
  select(-Bosque_Numerico)

unique(div.d$Bosque)

matriz_interacciones <- div %>%
  pivot_wider(names_from = Especie.planta,    
              values_from = interacciones,      
              values_fill = 0) %>%           
  as.data.frame()   


matriz_d <- div.d %>%
  pivot_wider(names_from = Especie.planta,    
              values_from = interacciones,      
              values_fill = 0) %>%           
  as.data.frame()


# Paso 4: Elimina la columna "Bosque" (si es necesario) y convierte a matriz
row.names(matriz_interacciones) <- matriz_interacciones$Bosque  # Los nombres de las filas son los bosques
matriz_interacciones <- matriz_interacciones %>% select(-Bosque) # Elimina la columna "Bosque"
matriz_interacciones <- as.matrix(matriz_interacciones)          # Convierte a matriz

row.names(matriz_d) <- matriz_d$Bosque  # Los nombres de las filas son los bosques
matriz_d <- matriz_d %>% select(-Bosque) # Elimina la columna "Bosque"
matriz_d <- as.matrix(matriz_d)          # Convierte a matriz



# Paso 5 (opcional): Convierte a matriz de presencia/ausencia (1/0)
matriz_presencia_ausencia <- ifelse(matriz_interacciones > 0, 1, 0)

matriz_presencia_ausencia.d <- ifelse(matriz_d > 0, 1, 0)

# Imprime las matrices resultantes para ver cómo quedaron
print("Matriz de Interacciones:")
print(matriz_interacciones)

print("Matriz de Presencia/Ausencia:")
print(matriz_presencia_ausencia.d)


install.packages("betapart")
library(betapart)
install.packages(c("vegan", "ade4", "sp"))
beta_sorensen_abund <- beta.pair.abund(matriz_interacciones, index.family = "sorensen")

# Imprimir los resultados para la matriz de interacciones
print("Resultados de beta-diversidad de Sørensen (con matriz de interacciones):")
print(beta_sorensen_abund)

# Calcular la diversidad beta de Sørensen con la matriz de presencia/ausencia
beta_sorensen_presencia <- beta.pair(matriz_presencia_ausencia, index.family = "sorensen")


beta_sorensen_presencia.d <- beta.pair(matriz_presencia_ausencia.d, index.family = "sorensen")

# Imprimir los resultados para la matriz de presencia/ausencia
print("Resultados de beta-diversidad de Sørensen (con matriz de presencia/ausencia):")
print(beta_sorensen_presencia)

print(beta_sorensen_presencia.d)

g <- (0.2200000 + 0.2978723 + 0.4117647 + 0.3846154 + 0.3191489 + 0.4411765 + 0.3589744 + 0.2941176 + 0.4871795 + 0.2352941)/10
d <- (0.5 + 0.29 + 0.26 + 0.24 + 0.54 + 0.53 + 0.381 + 0.13 + 0.09 + 0.2)/10


# 4. Imprimir los resultados
cat("Gorbea: Media =", media_gorbea, ", Error estándar =", ee_gorbea, "\n")
cat("Doñana: Media =", media_donana, ", Error estándar =", ee_donana, "\n")

######
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

sitems3$match <-
  paste(sitems3$Site,
        sitems3$Year,
        sitems3$Bosque,
        sitems3$Periodo,
        sitems3$Species,
        sep = "_")

sitems3$tot.visits.pl <-
  all_df_with_visits$tot.visits.pl[match(sitems3$match, all_df_with_visits$j)]
sitems3$tot.visits.pl[is.na(sitems3$tot.visits.sinout)] <- 0
write.csv(sitems3, "data/sitems3.pl_totvisits.csv")

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

d21.fs <- d21.fs %>%
  mutate(
    Y_formateado = sprintf("%02d", Y), # Formatear Y
    id_planta = case_when(
      !is.na(planta) & planta != "" ~ paste0("(", X, ",", Y_formateado, ")", planta),
      TRUE ~ paste0(X, ",", Y_formateado)
    )
  ) %>%
  select(-Y_formateado)

unique(d21.meanfs$Bosque)

d21.fs <- d21.fs %>%
  select(Site, Year, Bosque, Periodo, Planta, fruit_set, id_planta)

library(readxl)
d21.seed <- read_xlsx("./data/rep.suc/DoñanaFrutos2021.xlsx")

d21.seed <- d21.seed %>%
  mutate(Year ="2021",Site="Doñana", Periodo= `Ronda marcados`, Bosque = Punto)%>%
  group_by(Planta) %>%
  mutate(scaled_seeds = scale(`Semillas totales`)) %>%
  ungroup() %>%
  select(Planta, Periodo, Bosque, Year, Site, `Semillas totales`, scaled_seeds, `Cuadrado/FUERA del cuadrado` )

d21.seed <- d21.seed %>%
  rename(id_planta = `Cuadrado/FUERA del cuadrado`)

unique(d21.seed$Bosque)

d21.seed <- d21.seed %>%
  mutate(Bosque = dplyr::recode(Bosque,
                         "Puebla" = "Pinar Puebla",
                         "Hinojos 2021" = "Pinar Hinojos",
                         "Aznalcazar" = "Pinar Aznalcazar",
                         "Villa Manrique Sur" = "Pinar Villamanrique Sur",
                         "Villa Manrique Este" = "Pinar Villamanrique Este"))


d21.repro <- d21.fs %>%
  left_join(d21.meanseed, by = c("Bosque", "Site", "Year", "Periodo", "Planta", "id_planta"))

d21.repro <- left_join(d21.meanfs, d21.meanseed)
d21.repro <- d21.repro %>%
  select(Site, Year, Bosque, Periodo, Planta, id_planta, fruit_set,mean.sn, se.sn)



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
    fruit_set = Frutos_cuajados / total_flores_yemas)

g21.fs <- g21.fs %>%
  mutate(fruit_set = if_else(
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


g21.fs$Planta <- dplyr::recode(g21.fs$Planta, "Vicia Pyrenaica" = "Vicia pyrenaica",
                               "Helleborus viridis"= "Helleborus viridis subsp. occidentalis")


embols.21<- g21.fs %>%
  filter(Trat == "Embolsada") 


embols <- embols %>%
  filter(fruit_set > 0)

g21.fs<- g21.fs %>%
  filter(!Trat == "Embolsada") 


observaciones_por_individuo <- g21.fs %>%
  group_by(Site, Bosque, Periodo, Year, Planta, num) %>%
  summarise(fruit_set = mean(fruit_set))

g21.fs <- g21.fs %>%
  rename(id_planta = num) 

g21.fs <- g21.fs %>%
  select(Site, Year, Bosque, Periodo, Planta, Semillas_viables, scaled_seeds, id_planta)



g21.fs.fruit <- observaciones_por_individuo %>%
  rename(id_planta = num)

d21.seed <- d21.seed %>%
  rename(Semillas_viables = `Semillas totales`)
d21.seed$Year <- as.factor(d21.seed$Year)
g21.fs$Year <- as.factor (g21.fs$Year)
g21.fs$Bosque <- as.character (g21.fs$Bosque)
g21.fs.fruit$Bosque <- as.character(g21.fs.fruit$Bosque)


seeds <- bind_rows(d21.seed, g21.fs)

fruits <- bind_rows(d21.fs, g21.fs.fruit)







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
    fruit_set = Frutos_cuajados / Flores)

g22.fs <- g22.fs %>%
  mutate(fruit_set = if_else(
  Planta %in% c("Hepatica nobilis", "Erysimum gorbeanum", "Pedicularis sylvatica", "Hutchinsia alpina",
                "Scilla verna", "Primula veris", "Helianthemum nummularium", "Vicia pyrenaica", "Lathyrus linifolius") & fruit_set > 1,
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


g22.fs$Planta <- dplyr::recode(g22.fs$Planta, "Helleborus viridis"= "Helleborus viridis subsp. occidentalis")


embols.22<- g22.fs %>%
  filter(Embolsada =="si" ) 
embols.22 <- embols.22 %>%
  rename("Trat"="Embolsada",
         "Flores_yemas_totales"="Flores",
         "Fecha_recogida_ultimo.dato"="Fecha_recogida_ultimo dato",
         "Peso.Fruto.g"="Peso Fruto g",
         "Num_semillas.totales"="Num_semillas totales")

embols.22 <- embols.22 %>%
  select(Site, Year, Bosque, Periodo, Planta, Trat, num, Flores_yemas_totales, Fecha_flores, Frutos_cuajados, frutos, Fecha_recogida_ultimo.dato,
         Peso.Fruto.g, Num_semillas.totales, Semillas_viables, scaled_seeds, fruit_set )


embols.21$Fecha_flores <- as.Date(embols.21$Fecha_flores)
embols.21$Periodo <- as.character(embols.21$Periodo)
embols.21 <- embols.21 %>%
  select(Site, Year, Bosque, Periodo, Planta, Trat, num, Flores_yemas_totales, Fecha_flores, Frutos_cuajados, frutos, Fecha_recogida_ultimo.dato,
         Peso.Fruto.g, Num_semillas.totales, Semillas_viables, scaled_seeds, fruit_set )

embols.21$Num_semillas.totales <- as.numeric(embols.21$Num_semillas.totales)
embols.22$Num_semillas.totales <- as.numeric(embols.22$Num_semillas.totales)
embolsadas <- bind_rows(embols.21, embols.22)

write.csv(embolsadas, "./embolsadas_recogidas21-22.csv")


embols <- embols %>%
  filter(fruit_set > 0)

g22.fs<- g22.fs %>%
  filter(!Embolsada == "si") 


observaciones_por_individuo <- g22.fs %>%
  group_by(Site, Bosque, Periodo, Year, Planta, num) %>%
  summarise(fruit_set = mean(fruit_set))

g22.fs <- g22.fs %>%
  rename(id_planta = num) 
g22.fs <- g22.fs %>%
  mutate(Site = "Gorbea")

g22.fs <- g22.fs %>%
  select(Site, Year, Bosque, Periodo, Planta, Semillas_viables, scaled_seeds, id_planta)



g22.fs.fruit <- observaciones_por_individuo %>%
  rename(id_planta = num)

g22.fs.fruit <- g22.fs.fruit %>%
  mutate(Site = "Gorbea")

g22.fs$Periodo <- as.numeric (g22.fs$Periodo)
g22.fs$Bosque <- as.character(g22.fs$Bosque)
g22.fs$Year <- as.factor(g22.fs$Year)

seeds <- bind_rows(seeds, g22.fs)

g22.fs.fruit$Periodo <- as.numeric (g22.fs.fruit$Periodo)
g22.fs.fruit$Bosque <- as.character(g22.fs.fruit$Bosque)
g22.fs.fruit$Year <- as.numeric(g22.fs.fruit$Year)

fruits <- bind_rows(fruits, g22.fs.fruit)

unique(fruits$Bosque)
seeds$Bosque <- dplyr::recode(seeds$Bosque,"Pinar Villamanrique Este"= "Villamanrique Chaparral",
                                  "Pinar Villamanrique Sur"="Villamanrique Sur")


fruits$Bosque <- dplyr::recode(fruits$Bosque,"Pinar Villamanrique Este"= "Villamanrique Chaparral",
                              "Pinar Villamanrique Sur"="Villamanrique Sur")

write.csv(fruits, "./data/total_fruits.csv")
write.csv(seeds, "./data/total_seeds.csv")


fruit_set$match <-
  paste(fruit_set$Site,
        fruit_set$Year,
        fruit_set$Bosque,
        fruit_set$Periodo,
        sep = "_")  

##fruit set per plant sp
sitems3 <- read.csv("data/sitems3.pl_totvisits.csv")
sitems <- read.csv("data/SITE_network_level_metrics.csv", sep=",")
fs <- read.csv("./data/total_fruits.csv")
seeds <- read.csv("./data/total_seeds.csv")

sitems <- sitems %>%
  rename(Site=Site_id)

sitems3 <- sitems3 %>%
  left_join(sitems %>% select(Year, Site, Periodo, Bosque, poll.sp, plant.sp),
            by = c("Year", "Site", "Periodo", "Bosque"))

write.csv(sitems3, "data/sitems3.pl_totvisits.csv")

sitems3 <- sitems3 %>%
  rename(Planta = Species)

seeds_con_indices <- seeds %>%
  left_join(sitems3, by = c("Site", "Year", "Bosque", "Periodo", "Planta"))


fruits_con_indices <- fruits %>%
  left_join(sitems3, by = c("Site", "Year", "Bosque", "Periodo", "Planta"))

 
write.csv(fruits_con_indices, "./data/plants_fs.csv")
write.csv(seeds_con_indices, "./data/plants_seeds.csv")

####COMMUNITY NETWORK ANALYSIS
sitems <- read.csv("data/SITE_network_level_metrics.csv", sep=",")

sitems$Bosque <- as.character(sitems$Bosque)

sitems <- sitems %>%
  rename(Site = Site_id)

ntw_fruits <- fruits %>%
  left_join(sitems, by = c("Site", "Year", "Bosque", "Periodo")) 

ntw_seeds <- seeds %>%
  left_join(sitems, by = c("Site", "Year", "Bosque", "Periodo")) 

##edo hola:
sitems$mean.fs<-fruit_set$mean.fs[match(sitems$match, fruit_set$match)]
sitems$se.fs<-fruit_set$se.fs[match(sitems$match, fruit_set$match)]

sitems$mean.sn<-fruit_set$mean.sn[match(sitems$match, fruit_set$match)]
sitems$se.sn<-fruit_set$se.sn[match(sitems$match, fruit_set$match)]

##add visits

tot.visits<- pl.vis %>%
  dplyr::group_by(Site,Year,Bosque, Periodo) %>%
  dplyr::summarise(tot.vis = sum(tot.visits.pl))

sitems$match <-
  paste(sitems$Site,
        sitems$Year,
        sitems$Bosque,
        sitems$Periodo,
        sep="_")

tot.visits$match <-
  paste(tot.visits$Site,
        tot.visits$Year,
        tot.visits$Bosque,
        tot.visits$Periodo,
        sep = "_")

ntw_fruits$match <-
  paste(ntw_fruits$Site,
        ntw_fruits$Year,
        ntw_fruits$Bosque,
        ntw_fruits$Periodo,
        sep = "_")

ntw_seeds$match <-
  paste(ntw_seeds$Site,
        ntw_seeds$Year,
        ntw_seeds$Bosque,
        ntw_seeds$Periodo,
        sep = "_")

ntw_fruits$total.visits<-tot.visits$tot.vis[match(ntw_fruits$match, tot.visits$match )]
ntw_seeds$total.visits<-tot.visits$tot.vis[match(ntw_seeds$match, tot.visits$match )]

sitems$total.visits<-tot.visits$tot.vis[match(sitems$match, tot.visits$match )]
write.csv(ntw_fruits, "data/useful/sitems_ntwfruits.csv")
write.csv(ntw_seeds, "data/useful/sitems_ntwseeds.csv")
write.csv(sitems, "data/useful/sitems_visits.csv")



