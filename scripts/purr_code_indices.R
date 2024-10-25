##preparar consola
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,purrr,iNEXT,wesandersom,ggplot2,bipartite)
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


# Lista de sitios
sites <- unique(all_df$Site)
all_df$Site <- as.character(all_df$Site)
all_df$Bosque <- as.character(all_df$Bosque)
all_df$Periodo <- as.character(all_df$Periodo)
all_df$Year<- as.character(all_df$Year)

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
})
})


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