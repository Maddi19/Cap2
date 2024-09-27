#######MORISITA########
###################
##preparar consola
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,purrr,iNEXT,wesanderson,ggplot2,bipartite,
               spaa,reshape2,here)

all_df<-read.csv(here("data","all_data.csv"))
sites <- unique(all_df$Site)

#calculate average niche overlap per species 
all_df$Bosque <- as.character(all_df$Bosque)
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
          df <- melt(as.matrix(ni), varnames = c("row", "col"))
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


# Creamos la nueva columna "j"
out.niche.agg.sinout <- out.niche.agg.sinout %>%
  mutate(j = paste(Site_id, col))



#use unaggregated data and join it with network metrics

f2$functional.comp.poll.sinout<-out.site.sinout$functional.comp.poll[match(f2$Site_ID, out.site.sinout$Site_id)]
f2$functional.comp.pl.sinout<-out.site.sinout$functional.comp.pl[match(f2$Site_ID, out.site.sinout$Site_id)]
f2$nodf.song.sinout<-out.site.sinout$nodf.song[match(f2$Site_ID, out.site.sinout$Site_id)]
f2$species.poll.sinout<-out.site.sinout$species.poll[match(f2$Site_ID, out.site.sinout$Site_id)]

#also match with plant species level network data
f2$match<-paste(f2$Site_ID, f2$Plant.sp)
outsp.pl.site.sinout$match<-paste(outsp.pl.site.sinout$Site_id, outsp.pl.site.sinout$species)

f2$norm_degree.sinout<-outsp.pl.site.sinout$norm_degree[match(f2$match, outsp.pl.site.sinout$match)]
f2$weigh_closeness.sinout<-outsp.pl.site.sinout$weigh_closeness[match(f2$match, outsp.pl.site.sinout$match)]

#and with niche overlap and total number of visits
f2$niche.overlap.sinout<-out.niche.agg.sinout$mean.niche.over[match(f2$match, out.niche.agg.sinout$j)]

f2$tot.visits.sinout<-d.vis.sinout$tot.visits[match(f2$match, d.vis.sinout$j)]
f2$tot.visits.sinout[is.na(f2$tot.visits.sinout)]<-0


#calculate number of pollinator species per plant species instead of norm_degree

sites <- unique(d.sinout$Site_ID)


out.sinout <- data.frame(Site_id = NA, plant.sps = NA, poll.sps = NA)


for(i in 1:length(sites)){
  temp <- subset(d.sinout, Site_ID == sites[i])
  
  plants<-unique(temp$Plant_gen_sp)
  for(j in 1:length(plants)){
    temp2 <- subset(temp, Plant_gen_sp == plants[j])
    temp3<-droplevels(temp2)
    
    poll<-nlevels(unique(temp3$Pollinator_gen_sp))
    
    n <- nrow(out.sinout)
    
    out.sinout[n + 1,1] <- as.character(sites[i])
    out.sinout[n + 1,2] <- as.character(plants[j])
    out.sinout[n + 1,3] <- poll
    
  }
}
#out.sinout


out.sinout2<-out.sinout[-1,]
out.sinout2$match<-paste(out.sinout2$Site_id, out.sinout2$plant.sps)

f2$pollspsperplant.sinout<-out.sinout2$poll.sps[match(f2$match, out.sinout2$match)]


#scale all variables used

f2$functional.comp.poll.sinout.sc<-scale(f2$functional.comp.poll.sinout, center = T, scale = T)
f2$functional.comp.pl.sinout.sc<-scale(f2$functional.comp.pl.sinout, center = T, scale = T)
f2$nodf.song.sinout.sc<-scale(f2$nodf.song.sinout, center = T, scale = T)
f2$species.poll.sinout.sc<-scale(f2$species.poll.sinout, center = T, scale = T)
f2$norm_degree.sinout.sc<-scale(f2$norm_degree.sinout, center = T, scale = T)
f2$weigh_closeness.sinout.sc<-scale(f2$weigh_closeness.sinout, center = T, scale = T)
f2$niche.overlap.sinout.sc<-scale(f2$niche.overlap.sinout, center = T, scale = T)
f2$pollspsperplant.sinout.sc<-scale(f2$pollspsperplant.sinout, center = T, scale = T)
f2$tot.visits.sinout.sc<-scale(f2$tot.visits.sinout, center = T, scale = T)



#compare 2 models: one with simple diversity metrics (pollinator richness and number of visits), compared to one where position in a network (centrality) and niche overlap are included with these variables.

m1.sinout<-glmer(tot.fruitset ~  pollspsperplant.sinout.sc + tot.visits.sinout.sc  + (1|Plant.sp:Site_ID) + (1|Site_ID), 
                 family="binomial", data=f2)

summary(m1.sinout)
vif(m1.sinout)  #check for correlations between variables included in model



m2.sinout<-glmer(tot.fruitset ~  pollspsperplant.sinout.sc  + tot.visits.sinout.sc + weigh_closeness.sinout.sc + 
                   niche.overlap.sinout.sc + (1|Plant.sp:Site_ID)+ (1|Site_ID), family="binomial", data=f2)

summary(m2.sinout) 
vif(m2.sinout)

#select best model based on AIC 

AICtab(m1.sinout, m2.sinout) 

r2<-rsquared(c(m2.sinout, m1.sinout)) #check R^2 values for both models 

# extract estimates and st errors to present in table
coefs.s11A <- data.frame(coef(summary(m2.sinout)))
TableS11A<-round(coefs.s11A[,1:3],digits=2)
rownames(TableS11A)<-c("(Intercept)", "Pollinator richness", "Total number of visits", "Centrality", "Plant niche overlap")



##seed set

s3$functional.comp.poll.sinout<-out.site.sinout$functional.comp.poll[match(s3$Site_ID, out.site.sinout$Site_id)]
s3$functional.comp.pl.sinout<-out.site.sinout$functional.comp.pl[match(s3$Site_ID, out.site.sinout$Site_id)]
s3$nodf.song.sinout<-out.site.sinout$nodf.song[match(s3$Site_ID, out.site.sinout$Site_id)]
s3$species.poll.sinout<-out.site.sinout$species.poll[match(s3$Site_ID, out.site.sinout$Site_id)]

#also match with plant species level network data
s3$match<-paste(s3$Site_ID, s3$Plant.sp)
outsp.pl.site.sinout$match<-paste(outsp.pl.site.sinout$Site_id, outsp.pl.site.sinout$species)

s3$norm_degree.sinout<-outsp.pl.site.sinout$norm_degree[match(s3$match, outsp.pl.site.sinout$match)]
s3$weigh_closeness.sinout<-outsp.pl.site.sinout$weigh_closeness[match(s3$match, outsp.pl.site.sinout$match)]
s3$niche.overlap.sinout<-out.niche.agg.sinout$mean.niche.over[match(s3$match, out.niche.agg.sinout$j)]


s3$tot.visits.sinout<-d.vis.sinout$tot.visits[match(s3$match, d.vis.sinout$j)]
s3$tot.visits.sinout[is.na(s3$tot.visits.sinout)]<-0

s3$pollspsperplant.sinout<-out.sinout2$poll.sps[match(s3$match, out.sinout2$match)]


#scale all variables used

s3$norm_degree.sinout.sc<-scale(s3$norm_degree.sinout, center = T, scale = T)
s3$weigh_closeness.sinout.sc<-scale(s3$weigh_closeness.sinout, center = T, scale = T)
s3$niche.overlap.sinout.sc<-scale(s3$niche.overlap.sinout, center = T, scale = T)
s3$pollspsperplant.sinout.sc<-scale(s3$pollspsperplant.sinout, center = T, scale = T)
s3$tot.visits.sinout.sc<-scale(s3$tot.visits.sinout, center = T, scale = T)

s3$mean.seedn.sc<-scale(s3$mean.seedn, center = T, scale = T)
