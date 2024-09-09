##preparar consola
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,ourr,iNEXT,wesandersom,ggplot2,bipartite)
installed.packages("maxnodf")
library(maxnodf)
library(vegan)
library(bipartite)
source("./scripts/toolbox.R")

d<-read.csv("./data/clean/trans_donana_20_clean.csv")
d.21<-read.csv("./data/clean/trans_doñana_21_clean.csv")
d.gorb<-read.csv("./data/clean/trans_gorbea_20_clean.csv")
d.21.gorb<-read.csv("./data/clean/trans_G21_clean.csv")
d.22.gorb<-read.csv("./data/clean/trans_gorbea_22_clean.csv")
glimpse(d)
glimpse(d.21)
glimpse(d.gorb)
glimpse(d.22.gorb)
glimpse(d.21.gorb)

unique(d$Pollinator_id)

d.21$capturado <- str_replace(d.21$capturado, "idem ", "")

d.21[d.21 == " "] <- NA
pollinator_info <- d.21 %>%
  filter(!is.na(Pollinator_id) & Pollinator_id != " ") %>%
  select(Codigo, Pollinator_id, Pollinator_genus, Pollinator_family, Order)
pollinator_info[pollinator_info == ""] <- NA

info_complete <- pollinator_info %>%
  filter(!is.na(Pollinator_id) & !is.na(Pollinator_genus) & !is.na(Pollinator_family) & !is.na(Order)) %>%
  select(Codigo, Pollinator_id, Pollinator_genus, Pollinator_family, Order)

# 3. Actualizar columnas en d.21 usando match
d.21$Pollinator_id <- ifelse(
  is.na(d.21$Pollinator_id),
  info_complete$Pollinator_id[match(d.21$capturado, info_complete$Codigo)],
  d.21$Pollinator_id
)

d.21$Pollinator_genus <- ifelse(
  is.na(d.21$Pollinator_genus),
  info_complete$Pollinator_genus[match(d.21$capturado, info_complete$Codigo)],
  d.21$Pollinator_genus
)

d.21$Pollinator_family <- ifelse(
  is.na(d.21$Pollinator_family),
  info_complete$Pollinator_family[match(d.21$capturado, info_complete$Codigo)],
  d.21$Pollinator_family
)

d.21$Order <- ifelse(
  is.na(d.21$Order),
  info_complete$Order[match(d.21$capturado, info_complete$Codigo)],
  d.21$Order
)

d.21 <- d.21 %>%
  mutate(
    Pollinator_id = case_when(
      (is.na(Pollinator_id) | Pollinator_id == "") & !is.na(Pollinator_genus) & Pollinator_genus != "" ~ paste(trimws(Pollinator_genus), "sp."),  # Si Pollinator_id está vacío y Pollinator_genus no está vacío
      TRUE ~ Pollinator_id  # Mantener el valor original si Pollinator_id no está vacío
    )
  )

d.21 <- d.21 %>%
  mutate(
    Pollinator_genus = if_else(capturado == "AT1-02", "Lasioglossum", Pollinator_genus),
    Pollinator_family = if_else(capturado == "AT1-02", "Halictidae", Pollinator_family),
    Pollinator_id = case_when(
      Codigo == "AT1-02" | capturado == "AT1-02" ~ "Lasioglossum immunitum",
      Codigo == "HT6-16" | capturado == "HT6-16" ~ "Andrena allaudi",
      Codigo == "HT8-02" | capturado == "HT8-02" ~ "Lobonix aeneus",
      Codigo == "PC1-01" | capturado == "PC1-01" ~ "Empis tesellata",
      Codigo == "VET2-01" | capturado == "VET2-01" ~ "Lasioglossum immunitum",
      Codigo == "VET4-01" | capturado == "VET4-01" ~ "Lasioglossum immunitum",
      Codigo == "VEC4-04" | capturado == "VEC4-04" ~ "Lasioglossum immunitum",
      Codigo == "VEC5-06" | capturado == "VEC5-06" ~ "Eucera elongatula",
      Codigo == "VEC5-08" | capturado == "VEC5-08" ~ "Eucera elongatula",
      Codigo == "VET9-01" | capturado == "VET9-01" ~ "Anthrenus verbasci",
      Codigo == "VST2-03" | capturado == "VST2-03" ~ "Lasioglossum immunitum",
      Codigo == "VST4-02" | capturado == "VST4-02" ~ "Lobonix aeneus",
      Codigo == "VST4-04" | capturado == "VST4-04" ~ "Episyrphus balteatus",
      Codigo == "VST5-02" | capturado == "VST5-02" ~ "Eupeodes corollae",
      Codigo == "AT3-09" | capturado == "AT3-09" ~ "Eupeodes corollae",
      Codigo == "PT2-01" | capturado == "PT2-01" ~ "Lobonix aeneus",
      Codigo == "PT2-07" | capturado == "PT2-07" ~ "Rhyncomyia cuprea",
      Codigo == "PT3-01" | capturado == "PT3-01" ~ "Lobonix aeneus",
      Codigo == "PT7-09" | capturado == "PT7-09" ~ "Panurgus perezi",
      Pollinator_id == "Empis " ~ "Empis sp",
      TRUE ~ Pollinator_id  # Mantener el valor original si no coincide
    ),
    Pollinator_sps = if_else(capturado == "AT1-02", "immunitum", Pollinator_sps),
    Order = if_else(str_starts(Pollinator_id, "Andrena") |
                      str_starts(Pollinator_id, "Eucera") |
                      str_starts(Pollinator_id, "Lasioglossum")|
                      str_starts(Pollinator_id, "Panurgus" ),
                    "Hymenoptera", 
                    if_else(str_starts(Pollinator_id, "Empis") |
                              str_starts(Pollinator_id, "Episyrphus") |
                             str_starts(Pollinator_id,  "Eupeodes" )|
                             str_starts(Pollinator_id, "Rhyncomyia"),
                            "Diptera", 
                    if_else(str_starts(Pollinator_id, "Lobonix") |
                              str_starts(Pollinator_id, "Anthrenus"), 
                                    "Coleoptera",        
                            Order)
  )))



unique(d.21$Pollinator_id)
d.21<- d.21 %>%
  filter(!is.na(Pollinator_id))

unique(d.gorb$Pollinator_id)
unique(d.21.gorb$Polinizador_curro)
unique(d.22.gorb$Pollinator_id)


d.22.gorb<-d.22.gorb%>%
  rename(Periodo=Ronda,
         Pollinator_id=Polinizador_curro,
         Transecto = Transecto..1.2.3.,
         Fecha2=Fecha)%>%
  mutate(Year=first(year(Fecha2))) %>% 
  ungroup%>%
  mutate(Site="Gorbea")
d.22.gorb$Bosque<-as.character(d.22.gorb$Bosque)

d.21.gorb<-d.21.gorb%>%
  rename(Pollinator_id=Polinizador_curro)%>%
  mutate(Year=first(year(Fecha2))) %>% 
  ungroup%>%
  mutate(Site="Gorbea")
d.21.gorb$Bosque<-as.character(d.21.gorb$Bosque)

d.gorb<-d.gorb%>%
  mutate(Year=first(year(Fecha2))) %>% 
  ungroup%>%
  mutate(Site="Gorbea")
d.gorb$Bosque<-as.character(d.gorb$Bosque)

d.21<-d.21%>%
  mutate(Year=first(year(Fecha2))) %>% 
  ungroup%>%
  mutate(Site="Doñana")

d<-d%>%
  mutate(Year=first(year(Fecha2))) %>% 
  ungroup%>%
  mutate(Site="Doñana")
  
###group all data and select columns
list_dataframes <- list(d, d.21, d.gorb, d.21.gorb, d.22.gorb)

all_df<-map_df(list_dataframes, ~select(., any_of(c("X", "Site","Year", "Fecha2", "Hora","Bosque",
                                            "Periodo","Transecto","Planta","Pollinator_id", "Order",
                                            "Pollinator_genus","Pollinator_family"))))


all_df$Bosque <- recode(all_df$Bosque,"Pinar Villamanrique Este (Chaparral)"= "Villamanrique Chaparral",
                        "Pinar Villamanrique Sur"="Villamanrique Sur")

unique(all_df$Pollinator_id)
all_df$Pollinator_id <- recode(all_df$Pollinator_id,"Xylocopa  cantabrita"= "Xylocopa cantabrita",
                        "Bombylius  torquatus"="Bombylius torquatus",
                        "Eucera  elongatula"="Eucera elongatula",
                        "Andrena  cinerea"="Andrena cinerea",
                        "Apis melifera"="Apis mellifera",
                        "Andrena  flavipes"="Andrena flavipes",
                        "Andrena  chrysosceles"="Andrena chrysosceles",
                        "Nomada agrestis"="Nomada agrestis",
                        "Empis  sp"="Empis sp",
                        "Osmia  bicornis"="Osmia bicornis",
                        "Calliphora  sp"="Calliphora sp",
                        "Empis  tesellata"="Empis tesellata",
                        "Chasmatopterus  illigeri"="Chasmatopterus illigeri",
                        "Parageron  sp"="Parageron sp",
                        "Merodon  sp"="Merodon sp",
                        "Nomada  sp"="Nomada sp",
                        " Syrphus sp."= "Syrphus sp.",
                        "Eristalis "="Eristalis sp",
                        "Eristalis sp"="Eristalis sp.",
                        "Empis "="Empis sp",
                        "Lasioglossum  prasinum"="Lasioglossum prasinum",
                        "avispa"="Vespidae",
                        "Usia  sp"="Usia sp",
                        "Dasypoda  iberica"="Dasypoda iberica",
                        "Panurgus  calcaratus"="Panurgus calcaratus",
                        "Tropinota  squalida"="Tropinota squalida",
                        "Syrphidae 2"="Syrphidae",
                        "Syrphidae 3"="Syrphidae",
                        "Bombylidae 2"="Bombylidae",
                        "Anthophora  retusa"="Anthophora retusa",
                        "Nomada  agrestis"="Nomada agrestis",
                        "Lasioglossum"="Lasioglossum sp",
                        "Lasioglossum "="Lasioglossum sp",
                        "Osmia  submicans"="Osmia submicans",
                        "Machimus  sp"="Machimus sp",
                        "Panurgus  dentipes"="Panurgus dentipes",
                        "Episyrphus "="Episyrphus sp",
                        "Dasypoda "="Dasypoda sp",
                        "Syrphidae NA"="Syrphidae",
                        "Formicidae NA"="Formicidae",
                        "Lycaenidae NA"="Lycaenidae",
                        "mosca peque\x96a"="Diptera",
                        "Syrphidae 1"="Syrphidae",
                        "Anthophora"="Anthophora sp",
                        "Dilophus  sp"="Dilophus sp",
                        "Eucera"="Eucera sp",
                        "Platycheirus  albimanus"="Platycheirus albimanus",
                        "polilla marron mediana"="Lepidoptera",
                        "Cerambycidae NA"="Cerambycidae",
                        "Pyrgus sp"="Pyrgus sp.",
                        "Bombus sp"="Bombus sp.",
                        "Bombylius sp"="Bombylius sp.",
                        "Eupeodes sp"="Eupeodes sp.",
                        "Eucera "="Eucera sp.")



all_df <- all_df %>%
  filter(Planta != "")

write.csv(all_df, "data/all_data.csv")

all_df<-read.csv("./data/all_data.csv")

all_df <- all_df %>%
  mutate(Order = case_when(
    Pollinator_family %in% c("Apidae", "Megachilidae", "avispa", "abeja", "Andrenidae", "Halictidae", "Avispa", "Melittidae", 
                   "Abeja", "Formicidae", "Anthopora", "Colletidae", "Vespidae", "Argidae", 
                   "Pompiilidae") ~ "Hymenoptera",
    Pollinator_family %in% c("Esfingidae", "Lepidoptera", "Zygaenidae", "Nymphalidae-Satyrinae", "Hesperiidae", 
                   "Lycaenidae", "Pieridae") ~ "Lepidoptera",
    Pollinator_family %in% c("Pyrrhocoridae", "Miridae") ~ "Hemiptera",
    Pollinator_family %in% c("Orthoptera") ~ "Orthoptera",
    Pollinator_family%in% c("Diptera", "Tabanidae", "mosca", "Calliphoridae", "Conopidae", "Asilidae", 
                   "Stratiomyidae", "Tachinidae", "Muscidae", "Bombyliidae", "Bombylidae", 
                   "Empididae", "Bibionidae", "Mosca", "Syrphidae") ~ "Diptera",
    Pollinator_family %in% c("Oedemeridae", "Coleoptera", "Cerambycidae", "Cerambicidae", "Cetonidae", 
                   "Chrysomelidae", "Buprestidae", "Tenebrionidae", "Melyridae", 
                   "Scarabaeidae") ~ "Coleoptera",
    Pollinator_genus %in% c("Cerambycidae","Curculionoidea")~ "Coleoptera",
    Pollinator_genus %in% c("Anthocharis")~ "Lepidoptera",
    Pollinator_id %in% c("Syrphus sp.")~ "Diptera",
    TRUE ~ Order  # Para casos que no coincidan con ninguna familia listada
  ))

unknown_fams <- all_df %>% 
  filter(is.na(Pollinator_family)| Pollinator_family=="")
unique(unknown_fams$Pollinator_id)

all_df <- all_df %>%
  mutate(Order = if_else(str_starts(Pollinator_id, "Andrena") |
                           str_starts(Pollinator_id, "Eucera") |
                           str_starts(Pollinator_id, "Lasioglossum")|
                           str_starts(Pollinator_id, "Panurgus")|
                           str_starts(Pollinator_id, "Dasypoda") |
                           str_starts(Pollinator_id, "Anthophora") |
                           str_starts(Pollinator_id, "Hoplitis") |
                           str_starts(Pollinator_id, "Ceratina")|
                           str_starts(Pollinator_id, "Colletes"),
                         "Hymenoptera", 
                         if_else(str_starts(Pollinator_id, "Empis") |
                                   str_starts(Pollinator_id, "Episyrphus") |
                                   str_starts(Pollinator_id,  "Eupeodes" )|
                                   str_starts(Pollinator_id, "Platycheirus")|
                                   str_starts(Pollinator_id, "Lomatia")|
                                   str_starts(Pollinator_id, "Rhyncomyia")|
                                   str_starts(Pollinator_id, "Eristalis") |
                                   str_starts(Pollinator_id, "Usia")|
                                   str_starts(Pollinator_id, "Bombylius")|
                                   str_starts(Pollinator_id, "Platynochaetus")|
                                   str_starts(Pollinator_id, "Eristalinus") |
                                   str_starts(Pollinator_id, "Sphaerophoria"), 
                                 "Diptera", 
                                 if_else(str_starts(Pollinator_id, "Lobonix") |
                                           str_starts(Pollinator_id, "Anthrenus")|
                                           str_starts(Pollinator_id,"Chasmatopterus")|
                                           str_starts(Pollinator_id,"Exosoma")|
                                           str_starts(Pollinator_id,"Cerocoma")|
                                           str_starts(Pollinator_id,"Oxythyrea") , 
                                         "Coleoptera",        
                                         Order))))

all_df <- all_df %>%
  mutate(Pollinator_family = if_else(
    str_starts(Pollinator_id, "Andrena") | str_starts(Pollinator_id, "Panurgus"), "Andrenidae",
    if_else(
      str_starts(Pollinator_id, "Eristalis") | str_starts(Pollinator_id, "Platycheirus") |
        str_starts(Pollinator_id, "Episyrphus") | str_starts(Pollinator_id, "Eupeodes") |
        str_starts(Pollinator_id, "Platynochaetus") | str_starts(Pollinator_id, "Eristalinus") |
        str_starts(Pollinator_id, "Sphaerophoria") | str_starts(Pollinator_id, "Syrphus"), 
      "Syrphidae",
      if_else(
        str_starts(Pollinator_id, "Lasioglossum"), "Halictidae",
        if_else(
          str_starts(Pollinator_id, "Lomatia") | str_starts(Pollinator_id, "Usia") |
            str_starts(Pollinator_id, "Bombylius"), "Bombyliidae",
          if_else(
            str_starts(Pollinator_id, "Eucera") | str_starts(Pollinator_id, "Ceratina") |
              str_starts(Pollinator_id, "Anthophora"), "Apidae",
            if_else(
              str_starts(Pollinator_id, "Dasypoda"), "Melittidae",
              if_else(
                str_starts(Pollinator_id, "Hoplitis"), "Megachilidae",
                if_else(
                  str_starts(Pollinator_id, "Colletes"), "Colletidae",
                  if_else(
                    str_starts(Pollinator_id, "Anthocharis"), "Pieridae",
                    Pollinator_family
                  ))))))))))


gor<-all_df%>%
  filter(Site=="Gorbea")
unique(gor$Pollinator_id)
#168
doñ<-all_df%>%
  filter(Site=="Doñana")
unique(doñ$Pollinator_id)
#151
unique(all_df$Pollinator_family)
total_visits <- nrow(gor)

# Calcular el número de visitas por cada orden y su porcentaje
visits_by_order <- gor %>%
  group_by(Order) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / total_visits) * 100)

total_visits.d <- nrow(doñ)
visits_by_order.d <- doñ %>%
  group_by(Order) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / total_visits.d) * 100)

apidae <- gor %>%
  group_by(Pollinator_family)%>%
  summarise(Count=n())%>%
  mutate(perc.ap =(Count / total_visits) * 100) 

apidae.d <- doñ %>%
  group_by(Pollinator_family)%>%
  summarise(Count=n())%>%
  mutate(perc.ap =(Count / total_visits.d) * 100) 

apis<- gor %>%
  group_by(Pollinator_id)%>%
  summarise(Count=n())%>%
  mutate(perc.ap =(Count / total_visits) * 100) 
apis.d<- doñ %>%
  group_by(Pollinator_id)%>%
  summarise(Count=n())%>%
  mutate(perc.ap =(Count / total_visits.d) * 100) 

bombus<- gor %>%
  filter(str_starts(Pollinator_id, "Bombus")) %>%  # Aplica str_starts a la columna Pollinator_id
  group_by(Pollinator_id) %>%
  summarise(Count = n()) %>%
  mutate(perc.ap = (Count / total_visits) * 100)%>%
  summarise(sum=sum(perc.ap))

bombus.d <- doñ %>%
  filter(str_starts(Pollinator_id, "Bombus")) %>%  # Aplica str_starts a la columna Pollinator_id
  group_by(Pollinator_id) %>%
  summarise(Count = n()) %>%
  mutate(perc.ap = (Count / total_visits.d) * 100)


##cargar all data
all_df<-read.csv("./data/all_data.csv")
gor$Planta <- as.factor(gor$Planta)
gor$Pollinator_id <- as.factor(gor$Pollinator_id)
web=matrix(table(gor$Planta,gor$Pollinator_id),nrow=length(levels(gor$Planta)),ncol=length(levels(gor$Pollinator_id)))
doñ$Planta <- as.factor(doñ$Planta)
doñ$Pollinator_id <- as.factor(doñ$Pollinator_id)
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

