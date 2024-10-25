####SPECIES LEVEL ANALYSIS

rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,here, dplyr, lme4, carData,effects, easystats, lmertTest,
               performance,see,gridExtra,car, lattice,ggplot2,bipartite,
               glmmTMB)

sitems2<-read.csv(here("data","useful", "sitems2_totvisits.fs.csv"))
morisita<-read.csv(here("data","plant_species_morisita.csv"))

morisita <- morisita %>%
  rename(Planta = Especies)

combined_df <- sitems2 %>%
  left_join(morisita, 
            by = c("Year", "Bosque", "Periodo", "Site_id", "Planta"))

sitems<-read.csv(here("data","useful", "sitems_meanfs.csv"))

all_info <- combined_df %>%
  left_join(sitems,
            by= c("Year", "Bosque", "Periodo", "Site_id"))


all_info <- all_info %>%
  select(-connectance, -robustness.pol, -robustness.pl, -asymmetry, -X.x, -X.y)

write.csv(all_info, "data/useful/all_info.csv")
