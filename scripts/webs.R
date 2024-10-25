##preparar consola
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,purrr,iNEXT,wesanderson,ggplot2,bipartite)
install.packages("devtools") 
library(devtools)
pkgbuild::check_build_tools(debug = TRUE)
devtools::install_github("christophhoeppke/maxnodf")
library(maxnodf)
library(vegan)
library(bipartite)
source("./scripts/toolbox.R")

##cargar all data
all_df<-read.csv("./data/useful/all_data.csv")

gor<-all_df%>%
  filter(Site=="Gorbea")
unique(gor$Pollinator_id)

doñ<-all_df%>%
  filter(Site=="Doñana")
unique_pl<- gor %>%
  distinct(Pollinator_id) %>%  
  arrange(Pollinator_id)


##################VIOLIN PLOT#############
install.packages("hrbrthemes")
library(hrbrthemes)
library(ggsci)
unique(all_df$Pollinator_family)

crear_violin_plot <- function(dataset, title) {
  dataset_grouped <- dataset %>%
    group_by(Order, Periodo, Year) %>%
    summarise(count = n(), .groups = 'drop') %>%
    ungroup() 
  # Asegúrate de que el valor de 'Suma_Observaciones' sea usado para ajustar el ancho del violín
  ggplot(dataset_grouped, aes(x = Periodo, y = Order, fill = Order)) +
    geom_violin(aes(weight = count), scale = "count", trim = FALSE) +  # Usa el peso para ajustar el área del violín
    scale_fill_npg() +                                  # Colores de NPG para el Order
    theme_ipsum() +                                     # Tema minimalista
    labs(title = title, x = "Periodo (semana del año)", y = "Order") +  # Etiquetas de los ejes
    theme(legend.position = "right") +                   # Posición de la leyenda a la derecha
    facet_wrap(~ Year, scales = "free_x") +               # Crear un panel para cada año
    scale_x_continuous(breaks = 1:9, limits = c(1, 9))   # Configurar el rango y los ticks del eje x
}

# Crear una lista de datasets y títulos
datasets <- list(gor = gor, doñ = doñ)
titles <- c("Cantidad de Observaciones por Orden en cada Periodo (Gorbea)",
            "Cantidad de Observaciones por Orden en cada Periodo (Doñana)")

# Aplicar la función a cada dataset usando purrr::map2
plots <- map2(datasets, titles, crear_violin_plot)

# Mostrar los gráficos
print(plots[[1]])  # Gorbea
print(plots[[2]])  # Doñana

######INFO WEBS
unique(gor$Pollinator_id)
gor$Planta <- as.factor(gor$Planta)
gor$Pollinator_id <- as.factor(gor$Pollinator_id)

webs <- frame2webs(gor, varnames = c("Planta", "Pollinator_id", "Year"))

# Esto te da una lista con una matriz de interacciones por año
web_2020 <- webs[[1]]
web_2021 <- webs[[2]]
web_2022 <- webs[[3]]

# Unir todas las plantas y polinizadores de los tres años
all_plants <- unique(c(rownames(web_2020), rownames(web_2021), rownames(web_2022)))
all_pollinators <- unique(c(colnames(web_2020), colnames(web_2021), colnames(web_2022)))

# Crear nuevas matrices que tengan todas las plantas y polinizadores
# Rellenamos con 0 las interacciones faltantes para los años que no las tengan

web_2020_fixed <- matrix(0, nrow = length(all_plants), ncol = length(all_pollinators),
                         dimnames = list(all_plants, all_pollinators))
web_2020_fixed[rownames(web_2020), colnames(web_2020)] <- web_2020

web_2021_fixed <- matrix(0, nrow = length(all_plants), ncol = length(all_pollinators),
                         dimnames = list(all_plants, all_pollinators))
web_2021_fixed[rownames(web_2021), colnames(web_2021)] <- web_2021

web_2022_fixed <- matrix(0, nrow = length(all_plants), ncol = length(all_pollinators),
                         dimnames = list(all_plants, all_pollinators))
web_2022_fixed[rownames(web_2022), colnames(web_2022)] <- web_2022
# Ahora podemos hacer la comparación sin problemas de dimensiones
common_interactions <- (web_2020_fixed > 0) & (web_2021_fixed > 0) & (web_2022_fixed > 0)

# Mostrar las interacciones comunes
common_interactions

# Interacciones únicas para cada año
unique_2020 <- (web_2020_fixed > 0) & !(web_2021_fixed > 0) & !(web_2022_fixed > 0)
unique_2021 <- (web_2021_fixed > 0) & !(web_2020_fixed > 0) & !(web_2022_fixed > 0)
unique_2022 <- (web_2022_fixed > 0) & !(web_2020_fixed > 0) & !(web_2021_fixed > 0)

# También puedes encontrar interacciones compartidas entre dos años, por ejemplo:
common_2020_2021 <- (web_2020_fixed > 0) & (web_2021_fixed > 0) & !(web_2022_fixed > 0)
common_2020_2022 <- (web_2020_fixed > 0) & (web_2022_fixed > 0) & !(web_2021_fixed > 0)
common_2021_2022 <- (web_2021_fixed > 0) & (web_2022_fixed > 0) & !(web_2020_fixed > 0)

common_interactions <- (web_2020_fixed > 0) & (web_2021_fixed > 0) |
  (web_2020_fixed > 0) & (web_2022_fixed > 0) |
  (web_2021_fixed > 0) & (web_2022_fixed > 0)


combined_interactions <- web_2020_fixed > 0 | web_2021_fixed > 0 | web_2022_fixed > 0 
# Calcular la riqueza total de visitas únicas
total_unique_visits <- sum(combined_interactions)

# Total de interacciones comunes en los tres años
total_common <- sum(common_interactions)

# Calcular el porcentaje de interacciones comunes respecto al total de interacciones distintas
percentage_common <- (total_common / total_unique_visits) * 100

# Mostrar el resultado
percentage_common

# Total de interacciones únicas por año
unique_visits_2020 <- sum(unique_2020)
unique_visits_2021 <- sum(unique_2021)
unique_visits_2022 <- sum(unique_2022)

# Calcular el porcentaje de visitas únicas en cada año respecto al total de visitas únicas
percentage_unique_2020 <- (unique_visits_2020 / total_unique_visits) * 100
percentage_unique_2021 <- (unique_visits_2021 / total_unique_visits) * 100
percentage_unique_2022 <- (unique_visits_2022 / total_unique_visits) * 100

# Mostrar resultados
percentage_unique_2020
percentage_unique_2021
percentage_unique_2022


##interacciones totales registradas
total_interactions_2020 <- sum(web_2020_fixed)
total_interactions_2021 <- sum(web_2021_fixed)
total_interactions_2022 <- sum(web_2022_fixed)

# Total riqueza de interacciones por año
total_2020 <- sum(web_2020_fixed > 0)
total_2021 <- sum(web_2021_fixed > 0)
total_2022 <- sum(web_2022_fixed > 0)

# Plantas únicas en cada año
unique_plants_2020 <- (rowSums(web_2020_fixed > 0) > 0) & (rowSums(web_2021_fixed > 0) == 0) & (rowSums(web_2022_fixed > 0) == 0)
unique_plants_2021 <- (rowSums(web_2020_fixed > 0) == 0) & (rowSums(web_2021_fixed > 0) > 0) & (rowSums(web_2022_fixed > 0) == 0)
unique_plants_2022 <- (rowSums(web_2020_fixed > 0) == 0) & (rowSums(web_2021_fixed > 0) == 0) & (rowSums(web_2022_fixed > 0) > 0)

# Contar cuántas plantas son únicas en cada año
num_unique_plants_2020 <- sum(unique_plants_2020)
num_unique_plants_2021 <- sum(unique_plants_2021)
num_unique_plants_2022 <- sum(unique_plants_2022)

unique_pollinators_2020 <- (colSums(web_2020_fixed > 0) > 0) & (colSums(web_2021_fixed > 0) == 0) & (colSums(web_2022_fixed > 0) == 0)
unique_pollinators_2021 <- (colSums(web_2020_fixed > 0) == 0) & (colSums(web_2021_fixed > 0) > 0) & (colSums(web_2022_fixed > 0) == 0)
unique_pollinators_2022 <- (colSums(web_2020_fixed > 0) == 0) & (colSums(web_2021_fixed > 0) == 0) & (colSums(web_2022_fixed > 0) > 0)

# Contar cuántos polinizadores son únicos en cada año
num_unique_pollinators_2020 <- sum(unique_pollinators_2020)
num_unique_pollinators_2021 <- sum(unique_pollinators_2021)
num_unique_pollinators_2022 <- sum(unique_pollinators_2022)

# Número total de especies de plantas (filas)
total_plants <- sum(rowSums(web_2020_fixed > 0 | web_2021_fixed > 0 | web_2022_fixed > 0) > 0)

# Número total de especies de polinizadores (columnas)
total_pollinators <- sum(colSums(web_2020_fixed > 0 | web_2021_fixed > 0 | web_2022_fixed > 0) > 0)


# Porcentaje de plantas únicas en cada año
percentage_unique_plants_2020 <- (num_unique_plants_2020 / total_plants) * 100
percentage_unique_plants_2021 <- (num_unique_plants_2021 / total_plants) * 100
percentage_unique_plants_2022 <- (num_unique_plants_2022 / total_plants) * 100

# Porcentaje de polinizadores únicos en cada año
percentage_unique_pollinators_2020 <- (num_unique_pollinators_2020 / total_pollinators) * 100
percentage_unique_pollinators_2021 <- (num_unique_pollinators_2021 / total_pollinators) * 100
percentage_unique_pollinators_2022 <- (num_unique_pollinators_2022 / total_pollinators) * 100

# Número de polinizadores distintos en 2020 (polinizadores que interactuaron al menos una vez)
distinct_pollinators_2020 <- sum(colSums(web_2020_fixed > 0) > 0)
distinct_pollinators_2021 <- sum(colSums(web_2021_fixed > 0) > 0)
distinct_pollinators_2022 <- sum(colSums(web_2022_fixed > 0) > 0)

# Número de plantas distintas en 2020 (plantas que interactuaron al menos una vez)
distinct_plants_2020 <- sum(rowSums(web_2020_fixed > 0) > 0)
distinct_plants_2021 <- sum(rowSums(web_2021_fixed > 0) > 0)
distinct_plants_2022 <- sum(rowSums(web_2022_fixed > 0) > 0)

###doñana
webs <- frame2webs(doñ, varnames = c("Planta", "Pollinator_id", "Year"))

# Esto te da una lista con una matriz de interacciones por año
web_2020 <- webs[[1]]
web_2021 <- webs[[2]]


# Unir todas las plantas y polinizadores de los tres años
all_plants <- unique(c(rownames(web_2020), rownames(web_2021)))
all_pollinators <- unique(c(colnames(web_2020), colnames(web_2021)))

# Crear nuevas matrices que tengan todas las plantas y polinizadores
# Rellenamos con 0 las interacciones faltantes para los años que no las tengan

web_2020_fixed <- matrix(0, nrow = length(all_plants), ncol = length(all_pollinators),
                         dimnames = list(all_plants, all_pollinators))
web_2020_fixed[rownames(web_2020), colnames(web_2020)] <- web_2020

web_2021_fixed <- matrix(0, nrow = length(all_plants), ncol = length(all_pollinators),
                         dimnames = list(all_plants, all_pollinators))
web_2021_fixed[rownames(web_2021), colnames(web_2021)] <- web_2021

# Ahora podemos hacer la comparación sin problemas de dimensiones
common_interactions <- (web_2020_fixed > 0) & (web_2021_fixed > 0)

# Total de interacciones comunes en los tres años
total_common <- sum(common_interactions)

# Interacciones únicas para cada año
unique_2020 <- (web_2020_fixed > 0) & !(web_2021_fixed > 0)
unique_2021 <- (web_2021_fixed > 0) & !(web_2020_fixed > 0) 

# Total de interacciones distintas por año
total_2020 <- sum(web_2020_fixed > 0)
total_2021 <- sum(web_2021_fixed > 0)

combined_interactions <- web_2020_fixed > 0 | web_2021_fixed > 0 
# Calcular la riqueza total de visitas únicas
total_unique_visits <- sum(combined_interactions)


# Total de interacciones únicas por año
unique_visits_2020 <- sum(unique_2020)
unique_visits_2021 <- sum(unique_2021)

# Calcular el porcentaje de visitas únicas en cada año respecto al total de visitas únicas
percentage_unique_2020 <- (unique_visits_2020 / total_unique_visits) * 100
percentage_unique_2021 <- (unique_visits_2021 / total_unique_visits) * 100

# Mostrar resultados
percentage_unique_2020
percentage_unique_2021

percentage_common <- (total_common / total_unique_visits) * 100
##interacciones totales registradas
total_interactions_2020 <- sum(web_2020_fixed)
total_interactions_2021 <- sum(web_2021_fixed)

# Número total de especies de plantas (filas)
total_plants <- sum(rowSums(web_2020_fixed > 0 | web_2021_fixed  > 0) > 0)

# Número total de especies de polinizadores (columnas)
total_pollinators <- sum(colSums(web_2020_fixed > 0 | web_2021_fixed > 0) > 0)

# Plantas únicas en cada año
unique_plants_2020 <- (rowSums(web_2020_fixed > 0) > 0) & (rowSums(web_2021_fixed > 0) == 0) 
unique_plants_2021 <- (rowSums(web_2020_fixed > 0) == 0) & (rowSums(web_2021_fixed > 0) > 0) 

# Contar cuántas plantas son únicas en cada año
num_unique_plants_2020 <- sum(unique_plants_2020)
num_unique_plants_2021 <- sum(unique_plants_2021)

unique_pollinators_2020 <- (colSums(web_2020_fixed > 0) > 0) & (colSums(web_2021_fixed > 0) == 0)
unique_pollinators_2021 <- (colSums(web_2020_fixed > 0) == 0) & (colSums(web_2021_fixed > 0) > 0) 

# Contar cuántos polinizadores son únicos en cada año
num_unique_pollinators_2020 <- sum(unique_pollinators_2020)
num_unique_pollinators_2021 <- sum(unique_pollinators_2021)

#Porcentaje de plantas únicas en cada año
percentage_unique_plants_2020 <- (num_unique_plants_2020 / total_plants) * 100
percentage_unique_plants_2021 <- (num_unique_plants_2021 / total_plants) * 100

# Porcentaje de polinizadores únicos en cada año
percentage_unique_pollinators_2020 <- (num_unique_pollinators_2020 / total_pollinators) * 100
percentage_unique_pollinators_2021 <- (num_unique_pollinators_2021 / total_pollinators) * 100

# Número de polinizadores distintos en 2020 (polinizadores que interactuaron al menos una vez)
distinct_pollinators_2020 <- sum(colSums(web_2020_fixed > 0) > 0)
distinct_pollinators_2021 <- sum(colSums(web_2021_fixed > 0) > 0)

# Número de plantas distintas en 2020 (plantas que interactuaron al menos una vez)
distinct_plants_2020 <- sum(rowSums(web_2020_fixed > 0) > 0)
distinct_plants_2021 <- sum(rowSums(web_2021_fixed > 0) > 0)


gor<-all_df%>%
  filter(Site=="Gorbea")
gor.20$Planta <- as.factor(gor$Planta)
gor.20$Pollinator_id <- as.factor(gor.20$Pollinator_id)

web=matrix(table(gor.20$Planta,gor.20$Pollinator_id),nrow=length(levels(gor.20$Planta)),ncol=length(levels(gor.20$Pollinator_id)))


doñ.20<-all_df%>%
  filter(Site=="Doñana" & Year =="2020")
doñ$Planta <- as.factor(doñ$Planta)
doñ$Pollinator_id <- as.factor(doñ$Pollinator_id)

web1=matrix(table(doñ.20$Planta,doñ.20$Pollinator_id),nrow=length(levels(doñ.20$Planta)),ncol=length(levels(doñ.20$Pollinator_id)))
web1=matrix(table(doñ$Planta,doñ$Pollinator_id),nrow=length(levels(doñ$Planta)),ncol=length(levels(doñ$Pollinator_id)))


(V=sum(web)) #Frecuencia de la interacción, en este caso número total de visitas
(I=sum(web!=0))

(V=sum(web1)) 
(I=sum(web1!=0))


##########WEBS############
########################

# Lista de sitios
sites <- unique(all_df$Site)
all_df$Site <- as.character(all_df$Site)
all_df$Bosque <- as.character(all_df$Bosque)
all_df$Periodo <- as.character(all_df$Periodo)
all_df$Year<- as.character(all_df$Year)

library(dplyr)
all_df <- all_df %>%
  select(Site, Year, Bosque, Periodo, Planta, Pollinator_id)
str(all_df)
#######PURR########
###################
library(purrr)
library(Matrix)  # Para matrices dispersas
install.packages("furrr")
library(furrr)   # Para paralelización
install.packages("progressr")
library(progressr)
library(tidyr)

plan(multisession, workers = 3)# Permitir el uso de múltiples núcleos

# Habilitar el progreso
handlers(global = TRUE)  # Para ver el progreso globalmente
handlers("txtprogressbar")  # Barra de progreso en consola

total_steps <- length(sites) * 87 
# Envuelve tu future_map dentro de un progreso
with_progress({
  p <- progressr::progressor(steps = total_steps)  # Progresor para la cantidad de 'sites'
  
  results <- future_map(sites, function(site) {
    p()  # Incrementa la barra de progreso
    site_data <- all_df %>%
    filter(Site == site) %>%
    drop_na(Planta, Pollinator_id)
  
  nested_data <- site_data %>%
    group_by(Site, Bosque, Periodo, Year) %>%
    nest()
  
  nested_data <- nested_data %>%
    mutate(metrics = map(data, function(data_subset) {
      
      p()  # Actualizamos el progreso dentro del grupo
      
    web <- table(data_subset$Planta, data_subset$Pollinator_id)
      
      if (length(web) > 0) {
        web <- as.matrix(web) # Convertir a matriz dispersa
        
        tryCatch({
          spntw <- specieslevel(web)
          netw_metrics <- networklevel(web)
          
          # Quitar filas y columnas vacías
          web3 <- web[, colSums(web != 0) > 0, drop = FALSE]
          web4 <- web3[rowSums(web3 != 0) > 0, , drop = FALSE]
          
          # Aquí es donde podría volverse una matriz vacía después de eliminar filas/columnas vacías
          if (nrow(web4) > 0 && ncol(web4) > 0) {  # Verifica si 'web4' tiene filas/columnas suficientes
            nodf <- nestedness_NODF(web4)
            
            num_links <- sum(web4 > 0)  # Contar el número de enlaces
            max_possible_links <- nrow(web4) * ncol(web4)  # Máximo número de enlaces posible
            
            if (num_links <= max_possible_links) {
              max_nodf_value <- if (site %in% c(6, 15, 16)) {
                max_nest(web4[, -1])
              } else {
                max_nodf_result <- maxnodf(web = web4, quality = 2)
                max_nodf_result$max_nodf
              }
              
              combined_nodf <- if (site %in% c(6, 15, 16)) {
                comb_nest(web4[, -1], nodf, max_nodf_value)
              } else {
                comb_nest(web4, nodf, max_nodf_value)
              }
              
              if (!(site %in% c(6, 15, 16))) {
                simmons_max_nodf <- max_nodf_result$max_nodf
                song_max_nodf <- max_nest(web4)
                print(paste("Simmons = ", simmons_max_nodf, ", Song = ", song_max_nodf))
              }
              
              # Combinar métricas en una lista
              metrics_list <- list(
                spntw = spntw,
                nodf = nodf,
                max_nodf = max_nodf_value,
                combined_nodf = combined_nodf,
                netw_metrics = netw_metrics
              )
              return(metrics_list)
            } else {
              message("Número de enlaces excede el número máximo posible. Saltando este grupo.")
              return(NULL)
            }
          } else {
            message("La matriz web4 está vacía o no tiene suficientes filas y columnas. Saltando este grupo.")
            return(NULL)
          }
          
        }, error = function(e) {
          message("Error in calculating metrics: ", e)
          return(NULL)
        })
      } else {
        return(NULL)
      }
    }))



results

# Convertir a tibble
results_df <- bind_rows(results)

####dataframe polinizadores:
extract_vars <- function(x) {
  maddi <- data.frame(
    Site_id = results_df$Site[[x]],
    Year=results_df$Year[[x]],
    Bosque = results_df$Bosque[[x]],
    Periodo = results_df$Periodo[[x]],
    Especies = unique(results_df$data[[x]]$Pollinator_id),
    norm_degree = results_df$metrics[[x]][["spntw"]][["higher level"]][["normalised.degree"]],
    weight_closeness = results_df$metrics[[x]][["spntw"]][["higher level"]][["weighted.closeness"]],
    d = results_df$metrics[[x]][["spntw"]][["higher level"]][["d"]]
  )
  return(maddi)
}

sp.pl<-map(1:nrow(results_df), possibly(extract_vars, NA))

prueba<- map(sp.pl, function(x)sum(is.na(x)))
na_df_list <- map_dbl(sp.pl, \(x)sum(is.na(x)))

contains_one_list <- map_lgl(na_df_list, \(x)any(x == 1))

df_list_filtered <- sp.pl[!contains_one_list]
gorde<- map_df(df_list_filtered,bind_rows)

write.table(gorde, file = "data/SITE_species_level_metrics.csv", row.names = FALSE, sep = ",")


##plantas:
extract_vars.plant <- function(x) {
  maddi.pl <- data.frame(
    Site_id = results_df$Site[[x]],
    Year=results_df$Year[[x]],
    Bosque = results_df$Bosque[[x]],
    Periodo = results_df$Periodo[[x]],
    Especies = unique(results_df$data[[x]]$Planta),
    norm_degree = results_df$metrics[[x]][["spntw"]][["lower level"]][["normalised.degree"]],
    weight_closeness = results_df$metrics[[x]][["spntw"]][["lower level"]][["weighted.closeness"]],
    d = results_df$metrics[[1]][["spntw"]][["lower level"]][["d"]]
  )
  return(maddi.pl)
}

sp.plant<-map(1:nrow(results_df), possibly(extract_vars.plant, NA))


prueba.plant<- map(sp.plant, function(x)sum(is.na(x)))
na_df_list.plant <- map_dbl(sp.plant, \(x)sum(is.na(x)))

contains_one_list.plant <- map_lgl(na_df_list.plant, \(x)any(x == 1))

df_list_filtered.plant <- sp.plant[!contains_one_list.plant]
gorde.plant<- map_df(df_list_filtered.plant,bind_rows)

write.table(gorde.plant, file = "data/SITE_plant_species_level_metrics.csv", row.names = FALSE, sep = ",")


##network metrics
extract_vars.ntw <- function(x) {
  maddi.ntw <- data.frame(
    Site_id = results_df$Site[[x]],
    Year=results_df$Year[[x]],
    Bosque = results_df$Bosque[[x]],
    Periodo = results_df$Periodo[[x]],
    Anidamiento = results_df$metrics[[x]][["combined_nodf"]],
    poll.sp = results_df$metrics[[x]][["netw_metrics"]][["number.of.species.HL"]],
    plant.sp = results_df$metrics[[x]][["netw_metrics"]][["number.of.species.LL"]],
    comp.fun.pol = results_df$metrics[[x]][["netw_metrics"]][["functional.complementarity.HL"]],
    comp.fun.pl =results_df$metrics[[x]][["netw_metrics"]][["functional.complementarity.LL"]],
    robustness.pol=results_df$metrics[[x]][["netw_metrics"]][["robustness.HL"]],
    robustness.pl =results_df$metrics[[x]][["netw_metrics"]][["robustness.LL"]],
    connectance =results_df$metrics[[x]][["netw_metrics"]][["connectance"]],
    asymmetry= results_df$metrics[[x]][["netw_metrics"]][["web asymmetry"]]
  )
  return(maddi.ntw)
}

ntw<-map(1:nrow(results_df), possibly(extract_vars.ntw, NA))


prueba.ntw<- map(ntw, function(x)sum(is.na(x)))
na_df_list.ntw <- map_dbl(ntw, \(x)sum(is.na(x)))

contains_one_list.ntw <- map_lgl(na_df_list.ntw, \(x)any(x == 1))

df_list_filtered.ntw <- ntw[!contains_one_list.ntw]
gorde.ntw<- map_df(df_list_filtered.ntw,bind_rows)

write.table(gorde.ntw, file = "data/SITE_network_level_metrics.csv", row.names = FALSE, sep = ",")

results



######LOOP NORMAL
# Lista de sitios
sites <- unique(all_df$Site)
all_df$Site <- as.character(all_df$Site)
all_df$Bosque <- as.character(all_df$Bosque)
all_df$Periodo <- as.character(all_df$Periodo)

##Se crean 3 data.frames para analizar diferentes cosas.
out.site <- data.frame(Site_id = NA, Year=NA, Bosque=NA, Periodo=NA, Anidamiento= NA, Uniformidad=NA, H2=NA, 
                       species_poll = NA, species_pl = NA, 
                       functional_comp_poll = NA, functional_comp_plant = NA, 
                       nodf_song = NA)

outsp.site <- data.frame(Site_id = NA, Year= NA, Bosque = NA, Periodo = NA, 
                         Especies = NA, 
                         norm_degree = NA, 
                         weight_closeness= NA,
                         d=NA)

outsp.pl.site <- data.frame(Site_id = NA, Year= NA, Bosque = NA, Periodo = NA, Especies = NA, 
                            norm_degree = NA, weight_closeness=NA, d=NA)

library(dplyr)

webs <- list()
for (i in 1:length(sites)) {
  print(i)
  temp <- subset(all_df, Site == sites[i])
  
  years <- unique(temp$Year)
  for (l in 1:length(years)) {  # Loop sobre los años
    temp1 <- subset(temp, Year == years[l])
    
    bosque <- unique(temp1$Bosque)
    
    for (j in 1:length(bosque)) {
      temp2 <- subset(temp1, Bosque == bosque[j])
      periodo <- unique(temp2$Periodo)
      
      for (k in 1:length(periodo)) {
        temp3 <- subset(temp2, Periodo == periodo[k])
        
        web <- table(temp3$Planta, temp3$Pollinator_id)
        web <- as.matrix(web)  # Asegúrate de convertir a matriz
        spntw <- try(specieslevel(web), TRUE)
        ntw <- networklevel(web)
        
        if (isTRUE(class(spntw) == "try-error")) { next }
        
        # Remove empty rows and columns from web
        web3 <- web[, colSums(web != 0) > 0, drop = FALSE] 
        web4 <- web3[rowSums(web3 != 0) > 0, , drop = FALSE]
        
        # Comprobar si web4 tiene tamaño adecuado
        if (nrow(web4) < 2 || ncol(web4) < 2) {
          next  # Omitir si la matriz es demasiado pequeña
        }
        
        NODF <- nestedness_NODF(web4)
        
        if (i %in% c(6, 15, 16)) {
          max_NODF <- max_nest(web4[, -1])
          combined_NODF <- comb_nest(web4[, -1], NODF, max_NODF)
        } else {
          max_NODF <- maxnodf(web = web4, quality = 2)
          print(paste("Simmons = ", max_NODF$max_nodf, ", Song = ", max_nest(web4)))
          combined_NODF <- comb_nest(web4, NODF, max_NODF$max_nodf)
        }
        
        n <- nrow(out.site)
        webs[[n + 1]] <- web4
        n2 <- nrow(outsp.site)
        n3 <- nrow(outsp.pl.site)
        
        out.site[n + 1,1] <- as.character(sites[i])
        out.site[n + 1,2] <- as.character(years[l])
        out.site[n + 1,3] <- as.character(bosque[j])
        out.site[n + 1,4] <- as.character(periodo[k])
        out.site[n + 1,5:11] <- c(ntw[10], ntw[17], ntw[19], ntw[20:21], ntw[42:43])
        
        
        if (!is.null(spntw)) {
          outsp.site[n2 + seq_len(nrow(spntw$`higher level`)), ] <- data.frame(
            Site_id = as.character(sites[i]),
            Year = years[l],
            Bosque = bosque[j],
            Periodo = periodo[k],
            Especies = rownames(spntw$`higher level`),
            norm_degree = spntw$`higher level`[, 2],
            weight_closeness = spntw$`higher level`[, 14],
            d = spntw$`higher level`[, 20]
          )
          
          outsp.pl.site[n3 + seq_len(nrow(spntw$`lower level`)), ] <- data.frame(
            Site_id = as.character(sites[i]),
            Year = years[l],
            Bosque = bosque[j],
            Periodo = periodo[k],
            Especies = rownames(spntw$`lower level`),
            norm_degree= spntw$`lower level`[, 2],
            weight_closeness = spntw$`lower level`[, 14],
            d = spntw$`lower level`[, 20]
          )
        }
      }
    }
  }
}

# Verificar los dataframes llenados
print(out.site)
print(outsp.site)
print(outsp.pl.site)

# Opcionalmente, escribir a archivos CSV
write.table(out.site, file = "data/SITE_network_level_metrics.csv", row.names = FALSE, sep = ",")
write.table(outsp.site, file = "data/SITE_species_level_metrics.csv", row.names = FALSE, sep = ",")
write.table(outsp.pl.site, file = "data/SITE_plant_species_level_metrics.csv", row.names = FALSE, sep = ",")

