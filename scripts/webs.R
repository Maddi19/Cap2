##preparar consola
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,ourr,iNEXT,wesandersom,ggplot2,bipartite)
installed.packages("maxnodf")
library(maxnodf)
library(vegan)
library(bipartite)
source("./scripts/toolbox.R")


##cargar all data
all_df<-read.csv("./data/all_data.csv")

gor<-all_df%>%
  filter(Site=="Gorbea")
unique(gor$Pollinator_id)

doñ<-all_df%>%
  filter(Site=="Doñana")
unique_pl<- doñ %>%
  distinct(Pollinator_id) %>%  
  arrange(Pollinator_id)
 

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

# Total de interacciones por año
total_2020 <- sum(web_2020_fixed > 0)
total_2021 <- sum(web_2021_fixed > 0)
total_2022 <- sum(web_2022_fixed > 0)

# Total de interacciones comunes en los tres años
total_common <- sum(common_interactions)

# Total de interacciones únicas por año
total_unique_2020 <- sum(unique_2020)
total_unique_2021 <- sum(unique_2021)
total_unique_2022 <- sum(unique_2022)

# Porcentaje de interacciones comunes respecto al total de cada año
percentage_common_2020 <- (total_common / total_2020) * 100
percentage_common_2021 <- (total_common / total_2021) * 100
percentage_common_2022 <- (total_common / total_2022) * 100

# Porcentaje de interacciones únicas para cada año
percentage_unique_2020 <- (total_unique_2020 / total_2020) * 100
percentage_unique_2021 <- (total_unique_2021 / total_2021) * 100
percentage_unique_2022 <- (total_unique_2022 / total_2022) * 100

# Mostrar los resultados
percentage_common_2020
percentage_common_2021
percentage_common_2022

percentage_unique_2020
percentage_unique_2021
percentage_unique_2022

total_interactions_2020 <- sum(web_2020_fixed)
total_interactions_2021 <- sum(web_2021_fixed)
total_interactions_2022 <- sum(web_2022_fixed)

# Mostrar los resultados
total_interactions_2020
total_interactions_2021
total_interactions_2022



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

# Mostrar las interacciones comunes
common_interactions

# Interacciones únicas para cada año
unique_2020 <- (web_2020_fixed > 0) & !(web_2021_fixed > 0)
unique_2021 <- (web_2021_fixed > 0) & !(web_2020_fixed > 0) 

edit(unique_2020)

unique_2020_numeric <- as.numeric(unique_2020)
unique_2020_matrix <- matrix(unique_2020_numeric, nrow = nrow(unique_2020), 
                             ncol = ncol(unique_2020), 
                             dimnames = dimnames(unique_2020))
edit(unique_2020_matrix)
# También puedes encontrar interacciones compartidas entre dos años, por ejemplo:
common_2020_2021 <- (web_2020_fixed > 0) & (web_2021_fixed > 0)

# Total de interacciones por año
total_2020 <- sum(web_2020_fixed > 0)
total_2021 <- sum(web_2021_fixed > 0)

# Total de interacciones comunes en los tres años
total_common <- sum(common_interactions)

# Total de interacciones únicas por año
total_unique_2020 <- sum(unique_2020_matrix)
total_unique_2021 <- sum(unique_2021)

# Porcentaje de interacciones comunes respecto al total de cada año
percentage_common_2020 <- (total_common / total_2020) * 100
percentage_common_2021 <- (total_common / total_2021) * 100

# Porcentaje de interacciones únicas para cada año
percentage_unique_2020 <- (total_unique_2020 / total_2020) * 100
percentage_unique_2021 <- (total_unique_2021 / total_2021) * 100

# Mostrar los resultados
percentage_common_2020
percentage_common_2021

percentage_unique_2020
percentage_unique_2021

total_interactions_2020 <- sum(web_2020_fixed)
total_interactions_2021 <- sum(web_2021_fixed)

# Mostrar los resultados
total_interactions_2020
total_interactions_2021

gor.20<-all_df%>%
  filter(Site=="Gorbea" & Year =="2020")
gor.20$Planta <- as.factor(gor.20$Planta)
gor.20$Pollinator_id <- as.factor(gor.20$Pollinator_id)

web=matrix(table(gor.20$Planta,gor.20$Pollinator_id),nrow=length(levels(gor.20$Planta)),ncol=length(levels(gor.20$Pollinator_id)))


doñ.20<-all_df%>%
  filter(Site=="Doñana" & Year =="2020")
doñ.20$Planta <- as.factor(doñ.20$Planta)
doñ.20$Pollinator_id <- as.factor(doñ.20$Pollinator_id)

web1=matrix(table(doñ.20$Planta,doñ.20$Pollinator_id),nrow=length(levels(doñ.20$Planta)),ncol=length(levels(doñ.20$Pollinator_id)))


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

##Se crean 3 data.frames para analizar diferentes cosas.
out.site <- data.frame(Site_id = NA, species_poll = NA, species_pl = NA, 
                       functional_comp_poll = NA, functional_comp_plant = NA, 
                       nodf_song = NA)

outsp.site <- data.frame(Site_id = results_df[[1]][[1]], Bosque = results_df[[2]][[1]], Periodo = results_df[[3]][[1]], 
                         Especies = NA, 
                         norm_degree = results_df[[6]][[1]][["spntw"]][["higher level"]][["normalised.degree"]], 
                         weight_closeness= results_df[[6]][[1]][["spntw"]][["higher level"]][["weighted.closeness"]],
                         d=NA)

outsp.pl.site <- data.frame(Site_id = NA, Bosque = NA, Periodo = NA, Especies = NA, 
                            norm_degree = NA, weight_closeness=NA, d=NA)

webs <- list()
for (i in 1:length(sites)) {
  print(i)
  temp <- subset(all_df, Site == sites[i])
  bosque <- unique(temp$Bosque)
  
  for (j in 1:length(bosque)) {
    temp2 <- subset(temp, Bosque == bosque[j])
    periodo <- unique(temp2$Periodo)
    
    for (k in 1:length(periodo)) {
      temp3 <- subset(temp2, Periodo == periodo[k])
      
      web <- table(temp3$Planta, temp3$Pollinator_id)
      spntw <- try(specieslevel(web), TRUE)
      ntw <- networklevel(web)
      if (isTRUE(class(spntw) == "try-error")) { next } 
      
      # Remove empty rows and columns from web
      web3 <- web[, colSums(web != 0) > 0, drop = FALSE] 
      web4 <- web3[rowSums(web3 != 0) > 0, , drop = FALSE]
      
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
      
      out.site[n + 1, ] <- c(as.character(sites[i]), bosque[j], periodo[k], 
                             ntw[[20]], ntw[[21]], ntw[[42]], ntw[[43]], try(combined_NODF, TRUE))
      
      if (!is.null(spntw)) {
        outsp.site[n2 + seq_len(nrow(spntw$`higher level`)), ] <- data.frame(
          Site_id = as.character(sites[i]),
          Bosque = bosque[j],
          Periodo = periodo[k],
          Especies = rownames(spntw$`higher level`),
          norm_degree = spntw$`higher level`[, 2],
          weight_closeness = spntw$`higher level`[, 14],
          d = spntw$`higher level`[, 20]
        )
        
        outsp.pl.site[n3 + seq_len(nrow(spntw$`lower level`)), ] <- data.frame(
          Site_id = as.character(sites[i]),
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

# Verificar los dataframes llenados
print(out.site)
print(outsp.site)
print(outsp.pl.site)

# Opcionalmente, escribir a archivos CSV
write.table(out.site, file = "data/SITE_network_level_metrics.csv", row.names = FALSE, sep = ",")
write.table(outsp.site, file = "data/SITE_species_level_metrics.csv", row.names = FALSE, sep = ",")
write.table(outsp.pl.site, file = "data/SITE_plant_species_level_metrics.csv", row.names = FALSE, sep = ",")


#######PURR########
###################

results <- map(sites, function(site) {
  site_data <- all_df %>%
    filter(Site == site) %>%
    drop_na(Planta, Pollinator_id)
  
  nested_data <- site_data %>%
    group_by(Site,Bosque, Periodo, Year) %>%
    nest()
  
  nested_data <- nested_data %>%
    mutate(metrics = map(data, function(data_subset) {
      web <- table(data_subset$Planta, data_subset$Pollinator_id)
      
      if (length(web) > 0) {
        web <- as.matrix(web)
        
        tryCatch({
          spntw <- specieslevel(web)
          netw_metrics <- networklevel(web)
          
          # Remove empty rows and columns from web
          web3 <- web[, colSums(web != 0) > 0, drop = FALSE]
          web4 <- web3[rowSums(web3 != 0) > 0, , drop = FALSE]
          
          nodf <- nestedness_NODF(web4)
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
          
          # Combine metrics into a list to return
          metrics_list <- list(
            spntw = spntw,
            nodf = nodf,
            max_nodf = max_nodf_value,
            combined_nodf = combined_nodf,
            netw_metrics = netw_metrics
          )
          return(metrics_list)
        }, error = function(e) {
          message("Error in calculating metrics: ", e)
          return(NULL)
        })
      } else {
        return(NULL)
      }
    }))
  
  return(nested_data)
})

# Resultados
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

