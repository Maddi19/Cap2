#######MORISITA########
###################
##preparar consola
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,purrr,iNEXT,wesanderson,ggplot2,bipartite,
               spaa,reshape2,here)

all_df<-read.csv(here("data","useful","all_data.csv"))
sites <- unique(all_df$Site)

#calculate average niche overlap per species 
all_df$Bosque <- as.character(all_df$Bosque)
library(reshape2)
process_site <- function(site) {

  temp <- subset(all_df, Site == site)
  temp2 <- droplevels(temp)
 
  nested_data <- temp2 %>%
    group_by(Site, Bosque, Periodo, Year) %>%
    nest()
  
  nested_data <- nested_data %>%
    mutate(metrics = map(data, function(data_subset) {
      web <- table(data_subset$Pollinator_id, data_subset$Planta)
      
      if (length(web) > 0) {
        web <- as.matrix(web)
        
        tryCatch({
          ni <- spaa::niche.overlap(web, method = "morisita")
          # Remove empty rows and columns from web
          web3 <- web[, colSums(web != 0) > 0, drop = FALSE]
          web4 <- web3[rowSums(web3 != 0) > 0, , drop = FALSE]
          # Convertimos la matriz en un dataframe
          df <- reshape2::melt(as.matrix(ni), varnames = c("row", "col"))
          df$row <- as.character(df$row)
          df$col <- as.character(df$col)
          
          # Agregamos la columna del sitio y devolvemos el dataframe
          df <- df %>% mutate(Site_id = site,
                              Year= Year,
                              Bosque= Bosque,
                              Periodo= Periodo)
          average_niche_overlap <- df %>%
          group_by(col) %>%
          summarise(mean_overlap = mean(value, na.rm = TRUE)) %>%
          ungroup()
       
          return(average_niche_overlap)
          
          
        }, error = function(e) {
          message("Error in calculating metrics: ", e)
          return(NULL)
        })
      } else {
        return(NULL)
      }
    }))
  
  return(nested_data)
}
process_site

out.niche.sinout <- map_df(sites, process_site)


##plantas:
extract_vars.plant <- function(x) {
  maddi.pl <- data.frame(
    Site_id = out.niche.sinout$Site[[x]],
    Year=out.niche.sinout$Year[[x]],
    Bosque =out.niche.sinout$Bosque[[x]],
    Periodo = out.niche.sinout$Periodo[[x]],
    Especies = unique(out.niche.sinout$metrics[[x]]$col),
    morisita = unique(out.niche.sinout$metrics[[x]]$mean_overlap)
  ) 
  return(maddi.pl)
}

unique(out.niche.sinout$metrics[[1]]$mean_overlap)

sp.plant<-map(1:nrow(out.niche.sinout), possibly(extract_vars.plant, NA))


prueba.plant<- map(sp.plant, function(x)sum(is.na(x)))
na_df_list.plant <- map_dbl(sp.plant, \(x)sum(is.na(x)))

contains_one_list.plant <- map_lgl(na_df_list.plant, \(x)any(x == 1))

df_list_filtered.plant <- sp.plant[!contains_one_list.plant]
gorde.plant<- map_df(df_list_filtered.plant,bind_rows)

write.table(gorde.plant, file = "data/plant_species_morisita.csv", row.names = FALSE, sep = ",")


