
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,iNEXT,wesandersom,ggplot2)



####DONANA###
#use data for 2020 and 2021 separately for both sites
d<-read.csv("./data/clean/trans_donana_20_clean.csv")
head(d.tot)

#calculate sampling completeness

#######CALCULATE SAMPLING COMPLETENESS FOR POLLINATORS, PLANTS AND LINKS 

##############################################################
################ OVERALL SAMPLING COMPLETENESS ####################
##############################################################

#POLLINATORS
head(d)
d$full<-rep(1, nrow(d))
d.t<-table(d$full, d$Pollinator_id)
d.t2<-as.data.frame.array(d.t)

d.t3 <- t(d.t2[,1:ncol(d.t2)]) 

d.t4<-as.data.frame(d.t3)
d.t5<-as.list(d.t4)


rar <- iNEXT(d.t5, q=0, datatype="abundance", endpoint = 2000)
poll.d20<-ggiNEXT(rar, color.var="Assemblage", se=FALSE) 
poll.d20


#calculate sampling completeness as observed/estimated species richness
samp_comp_poll<-round(rar$AsyEst$Observed[1]/rar$AsyEst$Estimator[1],2)


#PLANTS
d.pl<-table(d$full, d$Planta)
d.pl2<-as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[,1:ncol(d.pl2)])
d.pl4<-as.data.frame(d.pl3)
d.pl5<-as.list(d.pl4)

rar2 <- iNEXT(d.pl5, q=0, datatype="abundance", endpoint = 2000)
plant.d20<-ggiNEXT(rar2, color.var="Assemblage", se=FALSE) 
plant.d20

#calculate sampling completeness as observed/estimated species richness
samp_comp_pl<-round(rar2$AsyEst$Observed[1]/rar2$AsyEst$Estimator[1],2)


#LINKSss

d$link<-paste(d$Planta, d$Pollinator_id)
d.l<-table(d$full, d$link)
d.l2<-as.data.frame.array(d.l)
d.l3 <- t(d.l2[,1:ncol(d.l2)])
d.l4<-as.data.frame(d.l3)
d.l5<-as.list(d.l4)


rar3 <- iNEXT(d.l5, q=0, datatype="abundance", endpoint = 7100)
rar3
link.d20<-ggiNEXT(rar3, color.var="Assemblage", se=FALSE) 
link.d20

#calculate sampling completeness as observed/estimated species richness
samp_comp_link<-round(rar3$AsyEst$Observed[1]/rar3$AsyEst$Estimator[1],2)


###################################################################
########### SAMPLING COMPLETENESS PER SITE ACROSS ALL TIMES ######
##################################################################

#POLLINATORS

d.t<-table(d$Bosque, d$Pollinator_id)

#pr<-table(d$Pollinator_id, d$Bosque)
d.t2<-as.data.frame.array(d.t)

d.t3 <- t(d.t2[,1:ncol(d.t2)]) 

d.t4<-as.data.frame(d.t3)
d.t5<-as.list(d.t4)


rar <- iNEXT(d.t5, q=0, datatype="abundance", endpoint = 2000)
poll.sites.d20<-ggiNEXT(rar, color.var="Assemblage", se=FALSE) 
poll.sites.d20


#calculate sampling completeness as observed/estimated species richness

est<-rar$AsyEst

samp_comp_poll_site<-round(mean(est$Observed[1]/est$Estimator[1], est$Observed[4]/est$Estimator[4],
                                est$Observed[7]/est$Estimator[7],est$Observed[10]/est$Estimator[10],
                                est$Observed[13]/est$Estimator[13]),2)

#PLANTS
d.pl<-table(d$Bosque, d$Planta)
d.pl2<-as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[,1:ncol(d.pl2)])
d.pl4<-as.data.frame(d.pl3)
d.pl5<-as.list(d.pl4)

rar2 <- iNEXT(d.pl5, q=0, datatype="abundance", endpoint = 200)
plant.sites.d20<-ggiNEXT(rar2, color.var="Assemblage", se=FALSE) 
plant.sites.d20

#calculate sampling completeness as observed/estimated species richness
est2<-rar2$AsyEst

samp_comp_pl_site<-round(mean(est2$Observed[1]/est2$Estimator[1], est2$Observed[4]/est2$Estimator[4],
                              est2$Observed[7]/est2$Estimator[7],est2$Observed[10]/est2$Estimator[10],
                              est2$Observed[13]/est2$Estimator[13]),2)



#LINKS

d$link<-paste(d$Planta, d$Pollinator_id)
d.l<-table(d$Bosque, d$link)
d.l2<-as.data.frame.array(d.l)
d.l3 <- t(d.l2[,1:ncol(d.l2)])
d.l4<-as.data.frame(d.l3)
d.l5<-as.list(d.l4)


rar3 <- iNEXT(d.l5, q=0, datatype="abundance", endpoint = 3000)
link.sites.d20<-ggiNEXT(rar3, color.var="Assemblage", se=FALSE) 
link.sites.d20

#calculate sampling completeness as observed/estimated species richness
est3<-rar3$AsyEst

samp_comp_link_site<-round(mean(est3$Observed[1]/est3$Estimator[1], est3$Observed[4]/est3$Estimator[4],
                                est3$Observed[7]/est3$Estimator[7],est3$Observed[10]/est3$Estimator[10],
                                est3$Observed[13]/est3$Estimator[13]),2)



#################################################################################################################
###################SAMPLING COMPLETENESS PER PERIOD ACROSS ALL SITES############################################
################################################################################################################

#POLLINATORS
d.t<-table(d$Periodo, d$Pollinator_id)
d.t2<-as.data.frame.array(d.t)

d.t3 <- t(d.t2[,1:ncol(d.t2)]) 

d.t4<-as.data.frame(d.t3)
d.t5<-as.list(d.t4)


rar <- iNEXT(d.t5, q=0, datatype="abundance", endpoint = 2000)
poll.period.d20<-ggiNEXT(rar, color.var="Assemblage", se=FALSE) 
poll.period.d20


#calculate sampling completeness as observed/estimated species richness

est_period<-rar$AsyEst

samp_comp_poll_period<-round(mean(est_period$Observed[1]/est_period$Estimator[1], est_period$Observed[4]/est_period$Estimator[4],
                                  est_period$Observed[7]/est_period$Estimator[7],est_period$Observed[10]/est_period$Estimator[10],
                                  est_period$Observed[13]/est_period$Estimator[13], est_period$Observed[16]/est_period$Estimator[16]),2)

#PLANTS
d.pl<-table(d$Periodo, d$Planta)
d.pl2<-as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[,1:ncol(d.pl2)])
d.pl4<-as.data.frame(d.pl3)
d.pl5<-as.list(d.pl4)

rar2 <- iNEXT(d.pl5, q=0, datatype="abundance", endpoint = 2000)
plant.period.d20<-ggiNEXT(rar2, color.var="Assemblage", se=FALSE) 
plant.period.d20

#calculate sampling completeness as observed/estimated species richness
est2_period<-rar2$AsyEst

samp_comp_pl_period<-round(mean(est2_period$Observed[1]/est2_period$Estimator[1], est2_period$Observed[4]/est2_period$Estimator[4],
                                est2_period$Observed[7]/est2_period$Estimator[7],est2_period$Observed[10]/est2_period$Estimator[10],
                                est2_period$Observed[13]/est2_period$Estimator[13], est2_period$Observed[16]/est2_period$Estimator[16]),2)



#LINKS

d$link<-paste(d$Planta, d$Pollinator_id)
d.l<-table(d$Periodo, d$link)
d.l2<-as.data.frame.array(d.l)
d.l3 <- t(d.l2[,1:ncol(d.l2)])
d.l4<-as.data.frame(d.l3)
d.l5<-as.list(d.l4)


rar3 <- iNEXT(d.l5, q=0, datatype="abundance", endpoint = 3000)
link.period.d20<-ggiNEXT(rar3, color.var="Assemblage", se=FALSE) 
link.period.d20

#calculate sampling completeness as observed/estimated species richness
est3_period<-rar3$AsyEst

samp_comp_link_period<-round(mean(est3_period$Observed[1]/est3_period$Estimator[1], est3_period$Observed[4]/est3_period$Estimator[4],
                                  est3_period$Observed[7]/est3_period$Estimator[7],est3_period$Observed[10]/est3_period$Estimator[10],
                                  est3_period$Observed[13]/est3_period$Estimator[13], est3_period$Observed[16]/est3_period$Estimator[16]),2)







#######CALCULATE SAMPLING COMPLETENESS FOR POLLINATORS, PLANTS AND LINKS 

##############################################################
################ OVERALL SAMPLING COMPLETENESS ####################
##############################################################
d.21<-read.csv("./data/clean/trans_doñana_21_clean.csv")
unique(d.21$Periodo)
#POLLINATORS
head(d.21)
d.21$full<-rep(1, nrow(d.21))
d.t.21<-table(d.21$full, d.21$Pollinator_id)
d.t2.21<-as.data.frame.array(d.t.21)

d.t3 <- t(d.t2.21[,1:ncol(d.t2.21)]) 

d.t4<-as.data.frame(d.t3)
d.t5<-as.list(d.t4)


rar <- iNEXT(d.t5, q=0, datatype="abundance", endpoint = 2000)
poll.d21<-ggiNEXT(rar, color.var="Assemblage", se=FALSE) 
poll.d21


#calculate sampling completeness as observed/estimated species richness
samp_comp_poll.21<-round(rar$AsyEst$Observed[1]/rar$AsyEst$Estimator[1],2)


#PLANTS
d.pl<-table(d.21$full, d.21$Planta)
d.pl2<-as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[,1:ncol(d.pl2)])
d.pl4<-as.data.frame(d.pl3)
d.pl5<-as.list(d.pl4)

rar2 <- iNEXT(d.pl5, q=0, datatype="abundance", endpoint = 2000)
plant.d21<-ggiNEXT(rar2, color.var="Assemblage", se=FALSE) 
plant.d21

#calculate sampling completeness as observed/estimated species richness
samp_comp_pl.21<-round(rar2$AsyEst$Observed[1]/rar2$AsyEst$Estimator[1],2)


#LINKS

d.21$link<-paste(d.21$Planta, d.21$Pollinator_id)
d.l<-table(d.21$full, d.21$link)
d.l2<-as.data.frame.array(d.l)
d.l3 <- t(d.l2[,1:ncol(d.l2)])
d.l4<-as.data.frame(d.l3)
d.l5<-as.list(d.l4)


rar3 <- iNEXT(d.l5, q=0, datatype="abundance", endpoint = 7100)
rar3
link.d21<-ggiNEXT(rar3, color.var="Assemblage", se=FALSE) 
link.d21
#calculate sampling completeness as observed/estimated species richness
samp_comp_link.21<-round(rar3$AsyEst$Observed[1]/rar3$AsyEst$Estimator[1],2)


###################################################################
########### SAMPLING COMPLETENESS PER SITE ACROSS ALL TIMES ######
##################################################################

#POLLINATORS

d.t<-table(d.21$Bosque, d.21$Pollinator_id)
d.t2<-as.data.frame.array(d.t)

d.t3 <- t(d.t2[,1:ncol(d.t2)]) 

d.t4<-as.data.frame(d.t3)
d.t5<-as.list(d.t4)


rar <- iNEXT(d.t5, q=0, datatype="abundance", endpoint = 2000)
poll.sites.d21<-ggiNEXT(rar, color.var="Assemblage", se=FALSE) 
poll.sites.d21


#calculate sampling completeness as observed/estimated species richness

est.21<-rar$AsyEst

samp_comp_poll_site.21<-round(mean(est.21$Observed[1]/est.21$Estimator[1], est.21$Observed[4]/est.21$Estimator[4],
                                   est.21$Observed[7]/est.21$Estimator[7],est.21$Observed[10]/est.21$Estimator[10],
                                   est.21$Observed[13]/est.21$Estimator[13]),2)

#PLANTS
d.pl<-table(d.21$Bosque, d.21$Planta)
d.pl2<-as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[,1:ncol(d.pl2)])
d.pl4<-as.data.frame(d.pl3)
d.pl5<-as.list(d.pl4)

rar2 <- iNEXT(d.pl5, q=0, datatype="abundance", endpoint = 200)
plant.sites.d21<-ggiNEXT(rar2, color.var="Assemblage", se=FALSE) 
plant.sites.d21

#calculate sampling completeness as observed/estimated species richness
est2.21<-rar2$AsyEst

samp_comp_pl_site.21<-round(mean(est2.21$Observed[1]/est2.21$Estimator[1], est2.21$Observed[4]/est2.21$Estimator[4],
                                 est2.21$Observed[7]/est2.21$Estimator[7],est2.21$Observed[10]/est2.21$Estimator[10],
                                 est2.21$Observed[13]/est2.21$Estimator[13]),2)



#LINKS

d.21$link<-paste(d.21$Planta, d.21$Pollinator_id)
d.l<-table(d.21$Bosque, d.21$link)
d.l2<-as.data.frame.array(d.l)
d.l3 <- t(d.l2[,1:ncol(d.l2)])
d.l4<-as.data.frame(d.l3)
d.l5<-as.list(d.l4)


rar3 <- iNEXT(d.l5, q=0, datatype="abundance", endpoint = 3000)
link.sites.d21<-ggiNEXT(rar3, color.var="Assemblage", se=FALSE) 
link.sites.d21


#calculate sampling completeness as observed/estimated species richness
est3.21<-rar3$AsyEst

samp_comp_link_site.21<-round(mean(est3.21$Observed[1]/est3.21$Estimator[1], est3.21$Observed[4]/est3.21$Estimator[4],
                                   est3.21$Observed[7]/est3.21$Estimator[7],est3.21$Observed[10]/est3.21$Estimator[10],
                                   est3.21$Observed[13]/est3.21$Estimator[13]),2)




#################################################################################################################
###################SAMPLING COMPLETENESS PER PERIOD ACROSS ALL SITES############################################
################################################################################################################

#POLLINATORS

d.t<-table(d.21$Periodo, d.21$Pollinator_id)
d.t2<-as.data.frame.array(d.t)

d.t3 <- t(d.t2[,1:ncol(d.t2)]) 

d.t4<-as.data.frame(d.t3)
d.t5<-as.list(d.t4)


rar <- iNEXT(d.t5, q=0, datatype="abundance", endpoint = 2000)
poll.period.d21<-ggiNEXT(rar, color.var="Assemblage", se=FALSE) 
poll.period.d21


#calculate sampling completeness as observed/estimated species richness

est_period.21<-rar$AsyEst

samp_comp_poll_period.21<-round(mean(est_period.21$Observed[1]/est_period.21$Estimator[1], est_period.21$Observed[4]/est_period.21$Estimator[4],
                                  est_period.21$Observed[7]/est_period.21$Estimator[7],est_period.21$Observed[10]/est_period.21$Estimator[10],
                                  est_period.21$Observed[13]/est_period.21$Estimator[13], est_period.21$Observed[16]/est_period.21$Estimator[16],
                                  est_period.21$Observed[19]/est_period.21$Estimator[19], est_period.21$Observed[22]/est_period.21$Estimator[22],
                                  est_period.21$Observed[25]/est_period.21$Estimator[25]),2)

#PLANTS
d.pl<-table(d.21$Periodo, d.21$Planta)
d.pl2<-as.data.frame.array(d.pl)
d.pl3 <- t(d.pl2[,1:ncol(d.pl2)])
d.pl4<-as.data.frame(d.pl3)
d.pl5<-as.list(d.pl4)

rar2 <- iNEXT(d.pl5, q=0, datatype="abundance", endpoint = 2000)
plant.period.d21<-ggiNEXT(rar2, color.var="Assemblage", se=FALSE) 
plant.period.d21

#calculate sampling completeness as observed/estimated species richness
est2_period.21<-rar2$AsyEst

samp_comp_pl_period.21<-round(mean(est2_period.21$Observed[1]/est2_period.21$Estimator[1], est2_period.21$Observed[4]/est2_period.21$Estimator[4],
                                   est2_period.21$Observed[7]/est2_period.21$Estimator[7],est2_period.21$Observed[10]/est2_period.21$Estimator[10],
                                   est2_period.21$Observed[13]/est2_period.21$Estimator[13], est2_period.21$Observed[16]/est2_period.21$Estimator[16],
                                   est2_period.21$Observed[19]/est2_period.21$Estimator[19], est2_period.21$Observed[22]/est2_period.21$Estimator[22],
                                   est2_period.21$Observed[25]/est2_period.21$Estimator[25]),2)



#LINKS

d.21$link<-paste(d.21$Planta, d.21$Pollinator_id)
d.l<-table(d.21$Periodo, d.21$link)
d.l2<-as.data.frame.array(d.l)
d.l3 <- t(d.l2[,1:ncol(d.l2)])
d.l4<-as.data.frame(d.l3)
d.l5<-as.list(d.l4)


rar3 <- iNEXT(d.l5, q=0, datatype="abundance", endpoint = 3000)
link.period.d21<-ggiNEXT(rar3, color.var="Assemblage", se=FALSE) 
link.period.d21


#calculate sampling completeness as observed/estimated species richness
est3_period.21<-rar3$AsyEst

samp_comp_link_period.21<-round(mean(est3_period.21$Observed[1]/est3_period.21$Estimator[1], est3_period.21$Observed[4]/est3_period.21$Estimator[4],
                                     est3_period.21$Observed[7]/est3_period.21$Estimator[7],est3_period.21$Observed[10]/est3_period.21$Estimator[10],
                                     est3_period.21$Observed[13]/est3_period.21$Estimator[13], est3_period.21$Observed[16]/est3_period.21$Estimator[16],
                                     est3_period.21$Observed[19]/est3_period.21$Estimator[19], est3_period.21$Observed[22]/est3_period.21$Estimator[22],
                                     est3_period.21$Observed[25]/est3_period.21$Estimator[25]),2)




dat<-data.frame(sampling_type=NA, sampling_completeness_pollinators=NA, sampling_completeness_plants=NA,
                sampling_completeness_links=NA)
dat[1,1]<-"full"
dat[1,2]<-samp_comp_poll
dat[1,3]<-samp_comp_pl
dat[1,4]<-samp_comp_link
dat[2:6,1]<-"per site"
dat[2:6,2]<-c(est$Observed[1]/est$Estimator[1], est$Observed[4]/est$Estimator[4],
              est$Observed[7]/est$Estimator[7],est$Observed[10]/est$Estimator[10],
              est$Observed[13]/est$Estimator[13])
dat[2:6,3]<-c(est2$Observed[1]/est2$Estimator[1], est2$Observed[4]/est2$Estimator[4],
              est2$Observed[7]/est2$Estimator[7],est2$Observed[10]/est2$Estimator[10],
              est2$Observed[13]/est2$Estimator[13])

dat[2:6,4]<-c(est3$Observed[1]/est3$Estimator[1], est3$Observed[4]/est3$Estimator[4],
              est3$Observed[7]/est3$Estimator[7],est3$Observed[10]/est3$Estimator[10],
              est3$Observed[13]/est3$Estimator[13])

dat[2:6,4]<-c(est3$Observed[1]/est3$Estimator[1], est3$Observed[4]/est3$Estimator[4],
              est3$Observed[7]/est3$Estimator[7],est3$Observed[10]/est3$Estimator[10],
              est3$Observed[13]/est3$Estimator[13])

dat[7:12,1]<-"per period"

dat[7:12,2]<-c(est_period$Observed[1]/est_period$Estimator[1], est_period$Observed[4]/est_period$Estimator[4],
               est_period$Observed[7]/est_period$Estimator[7],est_period$Observed[10]/est_period$Estimator[10],
               est_period$Observed[13]/est_period$Estimator[13], est_period$Observed[16]/est_period$Estimator[16])

dat[7:12,3]<-c(est2_period$Observed[1]/est2_period$Estimator[1], est2_period$Observed[4]/est2_period$Estimator[4],
               est2_period$Observed[7]/est2_period$Estimator[7],est2_period$Observed[10]/est2_period$Estimator[10],
               est2_period$Observed[13]/est2_period$Estimator[13], est2_period$Observed[16]/est2_period$Estimator[16])

dat[7:12,4]<-c(est3_period$Observed[1]/est3_period$Estimator[1], est3_period$Observed[4]/est3_period$Estimator[4],
               est3_period$Observed[7]/est3_period$Estimator[7],est3_period$Observed[10]/est3_period$Estimator[10],
               est3_period$Observed[13]/est3_period$Estimator[13], est3_period$Observed[16]/est3_period$Estimator[16])

dat[13,1]<-"full"
dat[13,2]<-samp_comp_poll.21
dat[13,3]<-samp_comp_pl.21
dat[13,4]<-samp_comp_link.21
dat[14:18,1]<-"per site"
dat[14:18,2]<-c(est.21$Observed[1]/est.21$Estimator[1], est.21$Observed[4]/est.21$Estimator[4],
                est.21$Observed[7]/est.21$Estimator[7],est.21$Observed[10]/est.21$Estimator[10],
                est.21$Observed[13]/est.21$Estimator[13])
dat[14:18,3]<-c(est2.21$Observed[1]/est2.21$Estimator[1], est2.21$Observed[4]/est2.21$Estimator[4],
                est2.21$Observed[7]/est2.21$Estimator[7],est2.21$Observed[10]/est2.21$Estimator[10],
                est2.21$Observed[13]/est2.21$Estimator[13])

dat[14:18,4]<-c(est3.21$Observed[1]/est3.21$Estimator[1], est3.21$Observed[4]/est3.21$Estimator[4],
                est3.21$Observed[7]/est3.21$Estimator[7],est3.21$Observed[10]/est3.21$Estimator[10],
                est3.21$Observed[13]/est3.21$Estimator[13])

dat[19:27,1]<-"per period"

dat[19:27,2]<-c(est_period.21$Observed[1]/est_period.21$Estimator[1], est_period.21$Observed[4]/est_period.21$Estimator[4],
                est_period.21$Observed[7]/est_period.21$Estimator[7],est_period.21$Observed[10]/est_period.21$Estimator[10],
                est_period.21$Observed[13]/est_period.21$Estimator[13], est_period.21$Observed[16]/est_period.21$Estimator[16],
                est_period.21$Observed[19]/est_period.21$Estimator[19], est_period.21$Observed[22]/est_period.21$Estimator[22],
                est_period.21$Observed[25]/est_period.21$Estimator[25])

dat[19:27,3]<-c(est2_period.21$Observed[1]/est2_period.21$Estimator[1], est2_period.21$Observed[4]/est2_period.21$Estimator[4],
                est2_period.21$Observed[7]/est2_period.21$Estimator[7],est2_period.21$Observed[10]/est2_period.21$Estimator[10],
                est2_period.21$Observed[13]/est2_period.21$Estimator[13], est2_period.21$Observed[16]/est2_period.21$Estimator[16],
                est2_period.21$Observed[19]/est2_period.21$Estimator[19], est2_period.21$Observed[22]/est2_period.21$Estimator[22],
                est2_period.21$Observed[25]/est2_period.21$Estimator[25])

dat[19:27,4]<-c(est3_period.21$Observed[1]/est3_period.21$Estimator[1], est3_period.21$Observed[4]/est3_period.21$Estimator[4],
                est3_period.21$Observed[7]/est3_period.21$Estimator[7],est3_period.21$Observed[10]/est3_period.21$Estimator[10],
                est3_period.21$Observed[13]/est3_period.21$Estimator[13], est3_period.21$Observed[16]/est3_period.21$Estimator[16],
                est3_period.21$Observed[19]/est3_period.21$Estimator[19], est3_period.21$Observed[22]/est3_period.21$Estimator[22],
                est3_period.21$Observed[25]/est3_period.21$Estimator[25])


dat$site<-rep("Donana", nrow(dat))
dat[13:27, 6]<-"2021"
dat[1:12, 6]<-"2020"
dat



#####GORBEA
########################### GORBEA DATA #########################

#######CALCULATE SAMPLING COMPLETENESS FOR POLLINATORS, PLANTS AND LINKS 
d.gorb<-read.csv("./data/clean/trans_gorbea_20_clean.csv")
unique(d.gorb$Periodo)
##############################################################
################ OVERALL SAMPLING COMPLETENESS ####################
##############################################################

#POLLINATORS
head(d.gorb)
d.gorb$full<-rep(1, nrow(d.gorb))
d.gorb.t<-table(d.gorb$full, d.gorb$Pollinator_id)
d.gorb.t2<-as.data.frame.array(d.gorb.t)

d.gorb.t3 <- t(d.gorb.t2[,1:ncol(d.gorb.t2)]) 

d.gorb.t4<-as.data.frame(d.gorb.t3)
d.gorb.t5<-as.list(d.gorb.t4)


rar.gorb <- iNEXT(d.gorb.t5, q=0, datatype="abundance", endpoint = 3000)
poll.gorb.20<-ggiNEXT(rar.gorb, color.var="Assemblage", se=FALSE) 
poll.gorb.20


#calculate sampling completeness as observed/estimated species richness
samp_comp_poll_gorb<-round(rar.gorb$AsyEst$Observed[1]/rar.gorb$AsyEst$Estimator[1],2)


#PLANTS
d.pl.gorb<-table(d.gorb$full, d.gorb$Planta)
d.pl.gorb2<-as.data.frame.array(d.pl.gorb)
d.pl.gorb3 <- t(d.pl.gorb2[,1:ncol(d.pl.gorb2)])
d.pl.gorb4<-as.data.frame(d.pl.gorb3)
d.pl.gorb5<-as.list(d.pl.gorb4)

rar.gorb2 <- iNEXT(d.pl.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.gorb.20<-ggiNEXT(rar.gorb2, color.var="Assemblage", se=FALSE) 
plant.gorb.20

#calculate sampling completeness as observed/estimated species richness
samp_comp_pl_gorb<-round(rar.gorb2$AsyEst$Observed[1]/rar.gorb2$AsyEst$Estimator[1],2)


#LINKS

d.gorb$link<-paste(d.gorb$Planta, d.gorb$Pollinator_id)
d.l.gorb<-table(d.gorb$full, d.gorb$link)
d.l.gorb2<-as.data.frame.array(d.l.gorb)
d.l.gorb3 <- t(d.l.gorb2[,1:ncol(d.l.gorb2)])
d.l.gorb4<-as.data.frame(d.l.gorb3)
d.l.gorb5<-as.list(d.l.gorb4)


rar.gorb3 <- iNEXT(d.l.gorb5, q=0, datatype="abundance", endpoint = 3000)
link.gorb.20<-ggiNEXT(rar.gorb3, color.var="Assemblage", se=FALSE) 
link.gorb.20


#calculate sampling completeness as observed/estimated species richness
samp_comp_link_gorb<-round(rar.gorb3$AsyEst$Observed[1]/rar.gorb3$AsyEst$Estimator[1],2)


###################################################################
########### SAMPLING COMPLETENESS PER SITE ACROSS ALL TIMES ######
##################################################################

#POLLINATORS

d.gorb.t<-table(d.gorb$Bosque, d.gorb$Pollinator_id)
d.gorb.t2<-as.data.frame.array(d.gorb.t)

d.gorb.t3 <- t(d.gorb.t2[,1:ncol(d.gorb.t2)]) 

d.gorb.t4<-as.data.frame(d.gorb.t3)
d.gorb.t5<-as.list(d.gorb.t4)


rar.gorb <- iNEXT(d.gorb.t5, q=0, datatype="abundance", endpoint = 3000)
poll.sites.gorb20<-ggiNEXT(rar.gorb, color.var="Assemblage", se=FALSE) 
poll.sites.gorb20


#calculate sampling completeness as observed/estimated species richness

est.gorb<-rar.gorb$AsyEst

samp_comp_poll_site_gorb<-round(mean(est.gorb$Observed[1]/est.gorb$Estimator[1], est.gorb$Observed[4]/est.gorb$Estimator[4],
                                     est.gorb$Observed[7]/est.gorb$Estimator[7],est.gorb$Observed[10]/est.gorb$Estimator[10],
                                     est.gorb$Observed[13]/est.gorb$Estimator[13]),2)

#PLANTS
d.pl.gorb<-table(d.gorb$Bosque, d.gorb$Planta)
d.pl.gorb2<-as.data.frame.array(d.pl.gorb)
d.pl.gorb3 <- t(d.pl.gorb2[,1:ncol(d.pl.gorb2)])
d.pl.gorb4<-as.data.frame(d.pl.gorb3)
d.pl.gorb5<-as.list(d.pl.gorb4)

rar.gorb2 <- iNEXT(d.pl.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.sites.gorb20<-ggiNEXT(rar.gorb2, color.var="Assemblage", se=FALSE) 
plant.sites.gorb20

#calculate sampling completeness as observed/estimated species richness
est.gorb2<-rar.gorb2$AsyEst

samp_comp_pl_site_gorb<-round(mean(est.gorb2$Observed[1]/est.gorb2$Estimator[1], est.gorb2$Observed[4]/est.gorb2$Estimator[4],
                                   est.gorb2$Observed[7]/est.gorb2$Estimator[7],est.gorb2$Observed[10]/est.gorb2$Estimator[10],
                                   est.gorb2$Observed[13]/est.gorb2$Estimator[13]),2)
#LINKS

d.gorb$link<-paste(d.gorb$Planta, d.gorb$Pollinator_id)
d.l.gorb<-table(d.gorb$Bosque, d.gorb$link)
d.l.gorb2<-as.data.frame.array(d.l.gorb)
d.l.gorb3 <- t(d.l.gorb2[,1:ncol(d.l.gorb2)])
d.l.gorb4<-as.data.frame(d.l.gorb3)
d.l.gorb5<-as.list(d.l.gorb4)


rar.gorb3 <- iNEXT(d.l.gorb5, q=0, datatype="abundance", endpoint = 3000)
link.sites.gorb20<-ggiNEXT(rar.gorb3, color.var="Assemblage", se=FALSE) 
link.sites.gorb20

#calculate sampling completeness as observed/estimated species richness

#calculate sampling completeness as observed/estimated species richness
est.gorb3<-rar.gorb3$AsyEst

samp_comp_link_site_gorb<-round(mean(est.gorb3$Observed[1]/est.gorb3$Estimator[1], est.gorb3$Observed[4]/est.gorb3$Estimator[4],
                                     est.gorb3$Observed[7]/est.gorb3$Estimator[7],est.gorb3$Observed[10]/est.gorb3$Estimator[10],
                                     est.gorb3$Observed[13]/est.gorb3$Estimator[13]),2)




#################################################################################################################
###################SAMPLING COMPLETENESS PER PERIOD ACROSS ALL SITES############################################
################################################################################################################

#POLLINATORS
d.t.gorb<-table(d.gorb$Periodo, d.gorb$Pollinator_id)
d.t.gorb2<-as.data.frame.array(d.t.gorb)

d.t.gorb3 <- t(d.t.gorb2[,1:ncol(d.t.gorb2)]) 

d.t.gorb4<-as.data.frame(d.t.gorb3)
d.t.gorb5<-as.list(d.t.gorb4)


rar.gorb <- iNEXT(d.t.gorb5, q=0, datatype="abundance", endpoint = 3000)
poll.period.gorb20<-ggiNEXT(rar.gorb, color.var="Assemblage", se=FALSE) 
poll.period.gorb20


#calculate sampling completeness as observed/estimated species richness

est_period.gorb<-rar.gorb$AsyEst

samp_comp_poll_period_gorb<-round(mean(est_period.gorb$Observed[1]/est_period.gorb$Estimator[1], est_period.gorb$Observed[4]/est_period.gorb$Estimator[4],
                                       est_period.gorb$Observed[7]/est_period.gorb$Estimator[7],est_period.gorb$Observed[10]/est_period.gorb$Estimator[10],
                                       est_period.gorb$Observed[13]/est_period.gorb$Estimator[13], est_period.gorb$Observed[16]/est_period.gorb$Estimator[16],
                                       est_period.gorb$Observed[19]/est_period.gorb$Estimator[19], est_period.gorb$Observed[22]/est_period.gorb$Estimator[22]),2)

#PLANTS
d.pl.gorb<-table(d.gorb$Periodo, d.gorb$Planta)
d.pl.gorb2<-as.data.frame.array(d.pl.gorb)
d.pl.gorb3 <- t(d.pl.gorb2[,1:ncol(d.pl.gorb2)])
d.pl.gorb4<-as.data.frame(d.pl.gorb3)
d.pl.gorb5<-as.list(d.pl.gorb4)

rar.gorb2 <- iNEXT(d.pl.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.period.gorb20<-ggiNEXT(rar.gorb2, color.var="Assemblage", se=FALSE) 
plant.period.gorb20

#calculate sampling completeness as observed/estimated species richness
est2_period.gorb<-rar.gorb2$AsyEst

samp_comp_pl_period_gorb<-round(mean(est2_period.gorb$Observed[1]/est2_period.gorb$Estimator[1], est2_period.gorb$Observed[4]/est2_period.gorb$Estimator[4],
                                     est2_period.gorb$Observed[7]/est2_period.gorb$Estimator[7],est2_period.gorb$Observed[10]/est2_period.gorb$Estimator[10],
                                     est2_period.gorb$Observed[13]/est2_period.gorb$Estimator[13], est2_period.gorb$Observed[16]/est2_period.gorb$Estimator[16],
                                     est2_period.gorb$Observed[19]/est2_period.gorb$Estimator[19], est2_period.gorb$Observed[22]/est2_period.gorb$Estimator[22]),2)



#LINKS

d.gorb$link<-paste(d.gorb$Planta, d.gorb$Pollinator_id)
d.l.gorb<-table(d.gorb$Periodo, d.gorb$link)
d.l.gorb2<-as.data.frame.array(d.l.gorb)
d.l.gorb3 <- t(d.l.gorb2[,1:ncol(d.l.gorb2)])
d.l.gorb4<-as.data.frame(d.l.gorb3)
d.l.gorb5<-as.list(d.l.gorb4)


rar.gorb3 <- iNEXT(d.l.gorb5, q=0, datatype="abundance", endpoint = 3000)
link.period.gorb20<-ggiNEXT(rar.gorb3, color.var="Assemblage", se=FALSE) 
link.period.gorb20


#calculate sampling completeness as observed/estimated species richness
est3_period_gorb<-rar.gorb3$AsyEst

samp_comp_link_period_gorb<-round(mean(est3_period_gorb$Observed[1]/est3_period_gorb$Estimator[1], est3_period_gorb$Observed[4]/est3_period_gorb$Estimator[4],
                                       est3_period_gorb$Observed[7]/est3_period_gorb$Estimator[7],est3_period_gorb$Observed[10]/est3_period_gorb$Estimator[10],
                                       est3_period_gorb$Observed[13]/est3_period_gorb$Estimator[13], est3_period_gorb$Observed[16]/est3_period_gorb$Estimator[16],
                                       est3_period_gorb$Observed[19]/est3_period_gorb$Estimator[19], est3_period_gorb$Observed[22]/est3_period_gorb$Estimator[22]),2)


#####GORBEA 2021
d.21.gorb<-read.csv("./data/clean/trans_G21_clean.csv")

########################### GORBEA DATA #########################

#######CALCULATE SAMPLING COMPLETENESS FOR POLLINATORS, PLANTS AND LINKS 

##############################################################
################ OVERALL SAMPLING COMPLETENESS ####################
##############################################################

#POLLINATORS
head(d.21.gorb)
d.21.gorb$full<-rep(1, nrow(d.21.gorb))
d.21.gorb.t<-table(d.21.gorb$full, d.21.gorb$Polinizador_curro)
d.21.gorb.t2<-as.data.frame.array(d.21.gorb.t)

d.21.gorb.t3 <- t(d.21.gorb.t2[,1:ncol(d.21.gorb.t2)]) 

d.21.gorb.t4<-as.data.frame(d.21.gorb.t3)
d.21.gorb.t5<-as.list(d.21.gorb.t4)


rar.21.gorb <- iNEXT(d.21.gorb.t5, q=0, datatype="abundance", endpoint = 3000)
poll.gorb21<-ggiNEXT(rar.21.gorb, color.var="Assemblage", se=FALSE) 
poll.gorb21


#calculate sampling completeness as observed/estimated species richness
samp_comp_poll_gorb.21<-round(rar.21.gorb$AsyEst$Observed[1]/rar.21.gorb$AsyEst$Estimator[1],2)


#PLANTS
d.pl.21.gorb<-table(d.21.gorb$full, d.21.gorb$Planta)
d.pl.21.gorb2<-as.data.frame.array(d.pl.21.gorb)
d.pl.21.gorb3 <- t(d.pl.21.gorb2[,1:ncol(d.pl.21.gorb2)])
d.pl.21.gorb4<-as.data.frame(d.pl.21.gorb3)
d.pl.21.gorb5<-as.list(d.pl.21.gorb4)

rar.21.gorb2 <- iNEXT(d.pl.21.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.gorb21<-ggiNEXT(rar.21.gorb2, color.var="Assemblage", se=FALSE) 
plant.gorb21

#calculate sampling completeness as observed/estimated species richness
samp_comp_pl_gorb.21<-round(rar.21.gorb2$AsyEst$Observed[1]/rar.21.gorb2$AsyEst$Estimator[1],2)


#LINKS

d.21.gorb$link<-paste(d.21.gorb$Planta, d.21.gorb$Polinizador_curro)
d.l.21.gorb<-table(d.21.gorb$full, d.21.gorb$link)
d.l.21.gorb2<-as.data.frame.array(d.l.21.gorb)
d.l.21.gorb3 <- t(d.l.21.gorb2[,1:ncol(d.l.21.gorb2)])
d.l.21.gorb4<-as.data.frame(d.l.21.gorb3)
d.l.21.gorb5<-as.list(d.l.21.gorb4)


rar.21.gorb3 <- iNEXT(d.l.21.gorb5, q=0, datatype="abundance", endpoint = 3000)
link.gorb21<-ggiNEXT(rar.21.gorb3, color.var="Assemblage", se=FALSE) 
link.gorb21

#calculate sampling completeness as observed/estimated species richness
samp_comp_link_gorb.21<-round(rar.21.gorb3$AsyEst$Observed[1]/rar.21.gorb3$AsyEst$Estimator[1],2)


###################################################################
########### SAMPLING COMPLETENESS PER SITE ACROSS ALL TIMES ######
##################################################################

#POLLINATORS

d.21.gorb.t<-table(d.21.gorb$Bosque, d.21.gorb$Polinizador_curro)
d.21.gorb.t2<-as.data.frame.array(d.21.gorb.t)

d.21.gorb.t3 <- t(d.21.gorb.t2[,1:ncol(d.21.gorb.t2)]) 

d.21.gorb.t4<-as.data.frame(d.21.gorb.t3)
d.21.gorb.t5<-as.list(d.21.gorb.t4)


rar.21.gorb <- iNEXT(d.21.gorb.t5, q=0, datatype="abundance", endpoint = 3000)
poll.sites.gorb21<-ggiNEXT(rar.21.gorb, color.var="Assemblage", se=FALSE) 
poll.sites.gorb21


#calculate sampling completeness as observed/estimated species richness

est.21.gorb<-rar.21.gorb$AsyEst

samp_comp_poll_site_gorb<-round(mean(est.21.gorb$Observed[1]/est.21.gorb$Estimator[1], est.21.gorb$Observed[4]/est.21.gorb$Estimator[4],
                                     est.21.gorb$Observed[7]/est.21.gorb$Estimator[7],est.21.gorb$Observed[10]/est.21.gorb$Estimator[10],
                                     est.21.gorb$Observed[13]/est.21.gorb$Estimator[13]),2)

#PLANTS
d.pl.21.gorb<-table(d.21.gorb$Bosque, d.21.gorb$Planta)
d.pl.21.gorb2<-as.data.frame.array(d.pl.21.gorb)
d.pl.21.gorb3 <- t(d.pl.21.gorb2[,1:ncol(d.pl.21.gorb2)])
d.pl.21.gorb4<-as.data.frame(d.pl.21.gorb3)
d.pl.21.gorb5<-as.list(d.pl.21.gorb4)

rar.21.gorb2 <- iNEXT(d.pl.21.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.sites.gorb21<-ggiNEXT(rar.21.gorb2, color.var="Assemblage", se=FALSE) 
plant.sites.gorb21

#calculate sampling completeness as observed/estimated species richness
est.21.gorb2<-rar.21.gorb2$AsyEst

samp_comp_pl_site_gorb<-round(mean(est.21.gorb2$Observed[1]/est.21.gorb2$Estimator[1], est.21.gorb2$Observed[4]/est.21.gorb2$Estimator[4],
                                   est.21.gorb2$Observed[7]/est.21.gorb2$Estimator[7],est.21.gorb2$Observed[10]/est.21.gorb2$Estimator[10],
                                   est.21.gorb2$Observed[13]/est.21.gorb2$Estimator[13]),2)



#LINKS

d.21.gorb$link<-paste(d.21.gorb$Planta, d.21.gorb$Polinizador_curro)
d.l.21.gorb<-table(d.21.gorb$Bosque, d.21.gorb$link)
d.l.21.gorb2<-as.data.frame.array(d.l.21.gorb)
d.l.21.gorb3 <- t(d.l.21.gorb2[,1:ncol(d.l.21.gorb2)])
d.l.21.gorb4<-as.data.frame(d.l.21.gorb3)
d.l.21.gorb5<-as.list(d.l.21.gorb4)


rar.21.gorb3 <- iNEXT(d.l.21.gorb5, q=0, datatype="abundance", endpoint = 3000)
link.sites.gorb21<-ggiNEXT(rar.21.gorb3, color.var="Assemblage", se=FALSE) 
link.sites.gorb21

#calculate sampling completeness as observed/estimated species richness

#calculate sampling completeness as observed/estimated species richness
est.21.gorb3<-rar.21.gorb3$AsyEst

samp_comp_link_site_gorb<-round(mean(est.21.gorb3$Observed[1]/est.21.gorb3$Estimator[1], est.21.gorb3$Observed[4]/est.21.gorb3$Estimator[4],
                                     est.21.gorb3$Observed[7]/est.21.gorb3$Estimator[7],est.21.gorb3$Observed[10]/est.21.gorb3$Estimator[10],
                                     est.21.gorb3$Observed[13]/est.21.gorb3$Estimator[13]),2)




#################################################################################################################
###################SAMPLING COMPLETENESS PER PERIOD ACROSS ALL SITES############################################
################################################################################################################

#POLLINATORS

d.t.21.gorb<-table(d.21.gorb$Periodo, d.21.gorb$Polinizador_curro)
d.t.21.gorb2<-as.data.frame.array(d.t.21.gorb)

d.t.21.gorb3 <- t(d.t.21.gorb2[,1:ncol(d.t.21.gorb2)]) 

d.t.21.gorb4<-as.data.frame(d.t.21.gorb3)
d.t.21.gorb5<-as.list(d.t.21.gorb4)


rar.21.gorb <- iNEXT(d.t.21.gorb5, q=0, datatype="abundance", endpoint = 3000)
poll.period.gorb21<-ggiNEXT(rar.21.gorb, color.var="Assemblage", se=FALSE) 
poll.period.gorb21


#calculate sampling completeness as observed/estimated species richness

est_period.21.gorb<-rar.21.gorb$AsyEst

samp_comp_poll_period_gorb<-round(mean(est_period.21.gorb$Observed[1]/est_period.21.gorb$Estimator[1], est_period.21.gorb$Observed[4]/est_period.21.gorb$Estimator[4],
                                       est_period.21.gorb$Observed[7]/est_period.21.gorb$Estimator[7],est_period.21.gorb$Observed[10]/est_period.21.gorb$Estimator[10],
                                       est_period.21.gorb$Observed[13]/est_period.21.gorb$Estimator[13], est_period.21.gorb$Observed[16]/est_period.21.gorb$Estimator[16],
                                       est_period.21.gorb$Observed[19]/est_period.21.gorb$Estimator[19]),2)

#PLANTS
d.pl.21.gorb<-table(d.21.gorb$Periodo, d.21.gorb$Planta)
d.pl.21.gorb2<-as.data.frame.array(d.pl.21.gorb)
d.pl.21.gorb3 <- t(d.pl.21.gorb2[,1:ncol(d.pl.21.gorb2)])
d.pl.21.gorb4<-as.data.frame(d.pl.21.gorb3)
d.pl.21.gorb5<-as.list(d.pl.21.gorb4)

rar.21.gorb2 <- iNEXT(d.pl.21.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.period.gorb21<-ggiNEXT(rar.21.gorb2, color.var="Assemblage", se=FALSE) 
plant.period.gorb21

#calculate sampling completeness as observed/estimated species richness
est2_period.21.gorb<-rar.21.gorb2$AsyEst

samp_comp_pl_period_gorb<-round(mean(est2_period.21.gorb$Observed[1]/est2_period.21.gorb$Estimator[1], est2_period.21.gorb$Observed[4]/est2_period.21.gorb$Estimator[4],
                                     est2_period.21.gorb$Observed[7]/est2_period.21.gorb$Estimator[7],est2_period.21.gorb$Observed[10]/est2_period.21.gorb$Estimator[10],
                                     est2_period.21.gorb$Observed[13]/est2_period.21.gorb$Estimator[13], est2_period.21.gorb$Observed[16]/est2_period.21.gorb$Estimator[16],
                                     est2_period.21.gorb$Observed[19]/est2_period.21.gorb$Estimator[19]),2)



#LINKS

d.21.gorb$link<-paste(d.21.gorb$Planta, d.21.gorb$Polinizador_curro)
d.l.21.gorb<-table(d.21.gorb$Periodo, d.21.gorb$link)
d.l.21.gorb2<-as.data.frame.array(d.l.21.gorb)
d.l.21.gorb3 <- t(d.l.21.gorb2[,1:ncol(d.l.21.gorb2)])
d.l.21.gorb4<-as.data.frame(d.l.21.gorb3)
d.l.21.gorb5<-as.list(d.l.21.gorb4)


rar.21.gorb3 <- iNEXT(d.l.21.gorb5, q=0, datatype="abundance", endpoint = 3000)
link.period.gorb21<-ggiNEXT(rar.21.gorb3, color.var="Assemblage", se=FALSE) 
link.period.gorb21


#calculate sampling completeness as observed/estimated species richness
est3_period_21_gorb<-rar.21.gorb3$AsyEst

samp_comp_link_period_21_gorb<-round(mean(est3_period_21_gorb$Observed[1]/est3_period_21_gorb$Estimator[1], est3_period_21_gorb$Observed[4]/est3_period_21_gorb$Estimator[4],
                                          est3_period_21_gorb$Observed[7]/est3_period_21_gorb$Estimator[7],est3_period_21_gorb$Observed[10]/est3_period_21_gorb$Estimator[10],
                                          est3_period_21_gorb$Observed[13]/est3_period_21_gorb$Estimator[13], est3_period_21_gorb$Observed[16]/est3_period_21_gorb$Estimator[16],
                                          est3_period_21_gorb$Observed[19]/est3_period_21_gorb$Estimator[19]),2)


#####GORBEA 2022
d.22.gorb<-read.csv("./data/clean/trans_gorbea_22_clean.csv")

########################### GORBEA DATA #########################

#######CALCULATE SAMPLING COMPLETENESS FOR POLLINATORS, PLANTS AND LINKS 

##############################################################
################ OVERALL SAMPLING COMPLETENESS ####################
##############################################################

#POLLINATORS
head(d.22.gorb)
d.22.gorb$full<-rep(1, nrow(d.22.gorb))
d.22.gorb.t<-table(d.22.gorb$full, d.22.gorb$Polinizador_curro)
d.22.gorb.t2<-as.data.frame.array(d.22.gorb.t)

d.22.gorb.t3 <- t(d.22.gorb.t2[,1:ncol(d.22.gorb.t2)]) 

d.22.gorb.t4<-as.data.frame(d.22.gorb.t3)
d.22.gorb.t5<-as.list(d.22.gorb.t4)


rar.22.gorb <- iNEXT(d.22.gorb.t5, q=0, datatype="abundance", endpoint = 3000)
poll.gorb22<-ggiNEXT(rar.22.gorb, color.var="Assemblage", se=FALSE) 
poll.gorb22


#calculate sampling completeness as observed/estimated species richness
samp_comp_poll_gorb.22<-round(rar.22.gorb$AsyEst$Observed[1]/rar.22.gorb$AsyEst$Estimator[1],2)


#PLANTS
d.pl.22.gorb<-table(d.22.gorb$full, d.22.gorb$Planta)
d.pl.22.gorb2<-as.data.frame.array(d.pl.22.gorb)
d.pl.22.gorb3 <- t(d.pl.22.gorb2[,1:ncol(d.pl.22.gorb2)])
d.pl.22.gorb4<-as.data.frame(d.pl.22.gorb3)
d.pl.22.gorb5<-as.list(d.pl.22.gorb4)

rar.22.gorb2 <- iNEXT(d.pl.22.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.gorb22<-ggiNEXT(rar.22.gorb2, color.var="Assemblage", se=FALSE) 
plant.gorb22

#calculate sampling completeness as observed/estimated species richness
samp_comp_pl_gorb.22<-round(rar.22.gorb2$AsyEst$Observed[1]/rar.22.gorb2$AsyEst$Estimator[1],2)


#LINKS

d.22.gorb$link<-paste(d.22.gorb$Planta, d.22.gorb$Polinizador_curro)
d.l.22.gorb<-table(d.22.gorb$full, d.22.gorb$link)
d.l.22.gorb2<-as.data.frame.array(d.l.22.gorb)
d.l.22.gorb3 <- t(d.l.22.gorb2[,1:ncol(d.l.22.gorb2)])
d.l.22.gorb4<-as.data.frame(d.l.22.gorb3)
d.l.22.gorb5<-as.list(d.l.22.gorb4)


rar.22.gorb3 <- iNEXT(d.l.22.gorb5, q=0, datatype="abundance", endpoint = 3000)
link.gorb22<-ggiNEXT(rar.22.gorb3, color.var="Assemblage", se=FALSE) 
link.gorb22

#calculate sampling completeness as observed/estimated species richness
samp_comp_link_gorb.22<-round(rar.22.gorb3$AsyEst$Observed[1]/rar.22.gorb3$AsyEst$Estimator[1],2)


###################################################################
########### SAMPLING COMPLETENESS PER SITE ACROSS ALL TIMES ######
##################################################################

#POLLINATORS

d.22.gorb.t<-table(d.22.gorb$Bosque, d.22.gorb$Polinizador_curro)
d.22.gorb.t2<-as.data.frame.array(d.22.gorb.t)

d.22.gorb.t3 <- t(d.22.gorb.t2[,1:ncol(d.22.gorb.t2)]) 

d.22.gorb.t4<-as.data.frame(d.22.gorb.t3)
d.22.gorb.t5<-as.list(d.22.gorb.t4)


rar.22.gorb <- iNEXT(d.22.gorb.t5, q=0, datatype="abundance", endpoint = 3000)
poll.sites.gorb22<-ggiNEXT(rar.22.gorb, color.var="Assemblage", se=FALSE) 
poll.sites.gorb22


#calculate sampling completeness as observed/estimated species richness

est.22.gorb<-rar.22.gorb$AsyEst

samp_comp_poll_site_gorb22<-round(mean(est.22.gorb$Observed[1]/est.22.gorb$Estimator[1], est.22.gorb$Observed[4]/est.22.gorb$Estimator[4],
                                     est.22.gorb$Observed[7]/est.22.gorb$Estimator[7],est.22.gorb$Observed[10]/est.22.gorb$Estimator[10],
                                     est.22.gorb$Observed[13]/est.22.gorb$Estimator[13]),2)

#PLANTS
d.pl.22.gorb<-table(d.22.gorb$Bosque, d.22.gorb$Planta)
d.pl.22.gorb2<-as.data.frame.array(d.pl.22.gorb)
d.pl.22.gorb3 <- t(d.pl.22.gorb2[,1:ncol(d.pl.22.gorb2)])
d.pl.22.gorb4<-as.data.frame(d.pl.22.gorb3)
d.pl.22.gorb5<-as.list(d.pl.22.gorb4)

rar.22.gorb2 <- iNEXT(d.pl.22.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.sites.gorb22<-ggiNEXT(rar.22.gorb2, color.var="Assemblage", se=FALSE) 
plant.sites.gorb22

#calculate sampling completeness as observed/estimated species richness
est.22.gorb2<-rar.22.gorb2$AsyEst

samp_comp_pl_site_gorb22<-round(mean(est.22.gorb2$Observed[1]/est.22.gorb2$Estimator[1], est.22.gorb2$Observed[4]/est.22.gorb2$Estimator[4],
                                   est.22.gorb2$Observed[7]/est.22.gorb2$Estimator[7],est.22.gorb2$Observed[10]/est.22.gorb2$Estimator[10],
                                   est.22.gorb2$Observed[13]/est.22.gorb2$Estimator[13]),2)



#LINKS

d.22.gorb$link<-paste(d.22.gorb$Planta, d.22.gorb$Polinizador_curro)
d.l.22.gorb<-table(d.22.gorb$Bosque, d.22.gorb$link)
d.l.22.gorb2<-as.data.frame.array(d.l.22.gorb)
d.l.22.gorb3 <- t(d.l.22.gorb2[,1:ncol(d.l.22.gorb2)])
d.l.22.gorb4<-as.data.frame(d.l.22.gorb3)
d.l.22.gorb5<-as.list(d.l.22.gorb4)


rar.22.gorb3 <- iNEXT(d.l.22.gorb5, q=0, datatype="abundance", endpoint = 3000)
link.sites.gorb22<-ggiNEXT(rar.22.gorb3, color.var="Assemblage", se=FALSE) 
link.sites.gorb22

#calculate sampling completeness as observed/estimated species richness

#calculate sampling completeness as observed/estimated species richness
est.22.gorb3<-rar.22.gorb3$AsyEst

samp_comp_link_site_gorb22<-round(mean(est.22.gorb3$Observed[1]/est.22.gorb3$Estimator[1], est.22.gorb3$Observed[4]/est.22.gorb3$Estimator[4],
                                     est.22.gorb3$Observed[7]/est.22.gorb3$Estimator[7],est.22.gorb3$Observed[10]/est.22.gorb3$Estimator[10],
                                     est.22.gorb3$Observed[13]/est.22.gorb3$Estimator[13]),2)




#################################################################################################################
###################SAMPLING COMPLETENESS PER PERIOD ACROSS ALL SITES############################################
################################################################################################################

#POLLINATORS

d.t.22.gorb<-table(d.22.gorb$Ronda, d.22.gorb$Polinizador_curro)
d.t.22.gorb2<-as.data.frame.array(d.t.22.gorb)

d.t.22.gorb3 <- t(d.t.22.gorb2[,1:ncol(d.t.22.gorb2)]) 

d.t.22.gorb4<-as.data.frame(d.t.22.gorb3)
d.t.22.gorb5<-as.list(d.t.22.gorb4)


rar.22.gorb <- iNEXT(d.t.22.gorb5, q=0, datatype="abundance", endpoint = 3000)
poll.period.gorb22<-ggiNEXT(rar.22.gorb, color.var="Assemblage", se=FALSE) 
poll.period.gorb22


#calculate sampling completeness as observed/estimated species richness

est_period.22.gorb<-rar.22.gorb$AsyEst

samp_comp_poll_period_gorb22<-round(mean(est_period.22.gorb$Observed[1]/est_period.22.gorb$Estimator[1], est_period.22.gorb$Observed[4]/est_period.22.gorb$Estimator[4],
                                       est_period.22.gorb$Observed[7]/est_period.22.gorb$Estimator[7],est_period.22.gorb$Observed[10]/est_period.22.gorb$Estimator[10],
                                       est_period.22.gorb$Observed[13]/est_period.22.gorb$Estimator[13], est_period.22.gorb$Observed[16]/est_period.22.gorb$Estimator[16],
                                       est_period.22.gorb$Observed[19]/est_period.22.gorb$Estimator[19]),2)

#PLANTS
d.pl.22.gorb<-table(d.22.gorb$Ronda, d.22.gorb$Planta)
d.pl.22.gorb2<-as.data.frame.array(d.pl.22.gorb)
d.pl.22.gorb3 <- t(d.pl.22.gorb2[,1:ncol(d.pl.22.gorb2)])
d.pl.22.gorb4<-as.data.frame(d.pl.22.gorb3)
d.pl.22.gorb5<-as.list(d.pl.22.gorb4)

rar.22.gorb2 <- iNEXT(d.pl.22.gorb5, q=0, datatype="abundance", endpoint = 3000)
plant.period.gorb22<-ggiNEXT(rar.22.gorb2, color.var="Assemblage", se=FALSE) 
plant.period.gorb22

#calculate sampling completeness as observed/estimated species richness
est2_period.22.gorb<-rar.22.gorb2$AsyEst

samp_comp_pl_period_gorb22<-round(mean(est2_period.22.gorb$Observed[1]/est2_period.22.gorb$Estimator[1], est2_period.22.gorb$Observed[4]/est2_period.22.gorb$Estimator[4],
                                     est2_period.22.gorb$Observed[7]/est2_period.22.gorb$Estimator[7],est2_period.22.gorb$Observed[10]/est2_period.22.gorb$Estimator[10],
                                     est2_period.22.gorb$Observed[13]/est2_period.22.gorb$Estimator[13], est2_period.22.gorb$Observed[16]/est2_period.22.gorb$Estimator[16],
                                     est2_period.22.gorb$Observed[19]/est2_period.22.gorb$Estimator[19]),2)



#LINKS

d.22.gorb$link<-paste(d.22.gorb$Planta, d.22.gorb$Polinizador_curro)
d.l.22.gorb<-table(d.22.gorb$Ronda, d.22.gorb$link)
d.l.22.gorb2<-as.data.frame.array(d.l.22.gorb)
d.l.22.gorb3 <- t(d.l.22.gorb2[,1:ncol(d.l.22.gorb2)])
d.l.22.gorb4<-as.data.frame(d.l.22.gorb3)
d.l.22.gorb5<-as.list(d.l.22.gorb4)


rar.22.gorb3 <- iNEXT(d.l.22.gorb5, q=0, datatype="abundance", endpoint = 3000)
link.period.gorb22<-ggiNEXT(rar.22.gorb3, color.var="Assemblage", se=FALSE) 
link.period.gorb22


#calculate sampling completeness as observed/estimated species richness
est3_period_22_gorb<-rar.22.gorb3$AsyEst

samp_comp_link_period_22_gorb<-round(mean(est3_period_22_gorb$Observed[1]/est3_period_22_gorb$Estimator[1], est3_period_22_gorb$Observed[4]/est3_period_22_gorb$Estimator[4],
                                          est3_period_22_gorb$Observed[7]/est3_period_22_gorb$Estimator[7],est3_period_22_gorb$Observed[10]/est3_period_22_gorb$Estimator[10],
                                          est3_period_22_gorb$Observed[13]/est3_period_22_gorb$Estimator[13], est3_period_22_gorb$Observed[16]/est3_period_22_gorb$Estimator[16],
                                          est3_period_22_gorb$Observed[19]/est3_period_22_gorb$Estimator[19]),2)







dat.gorb<-data.frame(sampling_type=NA, sampling_completeness_pollinators=NA, sampling_completeness_plants=NA,
                     sampling_completeness_links=NA)
dat.gorb[1,1]<-"full"
dat.gorb[1,2]<-samp_comp_poll_gorb
dat.gorb[1,3]<-samp_comp_pl_gorb
dat.gorb[1,4]<-samp_comp_link_gorb
dat.gorb[2:6,1]<-"per site"
dat.gorb[2:6,2]<-c(est.gorb$Observed[1]/est.gorb$Estimator[1], est.gorb$Observed[4]/est.gorb$Estimator[4],
                   est.gorb$Observed[7]/est.gorb$Estimator[7],est.gorb$Observed[10]/est.gorb$Estimator[10],
                   est.gorb$Observed[13]/est.gorb$Estimator[13])
dat.gorb[2:6,3]<-c(est.gorb2$Observed[1]/est.gorb2$Estimator[1], est.gorb2$Observed[4]/est.gorb2$Estimator[4],
                   est.gorb2$Observed[7]/est.gorb2$Estimator[7],est.gorb2$Observed[10]/est.gorb2$Estimator[10],
                   est.gorb2$Observed[13]/est.gorb2$Estimator[13])

dat.gorb[2:6,4]<-c(est.gorb3$Observed[1]/est.gorb3$Estimator[1], est.gorb3$Observed[4]/est.gorb3$Estimator[4],
                   est.gorb3$Observed[7]/est.gorb3$Estimator[7],est.gorb3$Observed[10]/est.gorb3$Estimator[10],
                   est.gorb3$Observed[13]/est.gorb3$Estimator[13])

dat.gorb[7:14,1]<-"per period"

dat.gorb[7:14,2]<-c(est_period.gorb$Observed[1]/est_period.gorb$Estimator[1], est_period.gorb$Observed[4]/est_period.gorb$Estimator[4],
                    est_period.gorb$Observed[7]/est_period.gorb$Estimator[7],est_period.gorb$Observed[10]/est_period.gorb$Estimator[10],
                    est_period.gorb$Observed[13]/est_period.gorb$Estimator[13], est_period.gorb$Observed[16]/est_period.gorb$Estimator[16],
                    est_period.gorb$Observed[19]/est_period.gorb$Estimator[19], est_period.gorb$Observed[22]/est_period.gorb$Estimator[22])

dat.gorb[7:14,3]<-c(est2_period.gorb$Observed[1]/est2_period.gorb$Estimator[1], est2_period.gorb$Observed[4]/est2_period.gorb$Estimator[4],
                    est2_period.gorb$Observed[7]/est2_period.gorb$Estimator[7],est2_period.gorb$Observed[10]/est2_period.gorb$Estimator[10],
                    est2_period.gorb$Observed[13]/est2_period.gorb$Estimator[13], est2_period.gorb$Observed[16]/est2_period.gorb$Estimator[16],
                    est2_period.gorb$Observed[19]/est2_period.gorb$Estimator[19], est2_period.gorb$Observed[22]/est2_period.gorb$Estimator[22])


dat.gorb[7:14,4]<-c(est3_period_gorb$Observed[1]/est3_period_gorb$Estimator[1], est3_period_gorb$Observed[4]/est3_period_gorb$Estimator[4],
                    est3_period_gorb$Observed[7]/est3_period_gorb$Estimator[7],est3_period_gorb$Observed[10]/est3_period_gorb$Estimator[10],
                    est3_period_gorb$Observed[13]/est3_period_gorb$Estimator[13], est3_period_gorb$Observed[16]/est3_period_gorb$Estimator[16],
                    est3_period_gorb$Observed[19]/est3_period_gorb$Estimator[19], est3_period_gorb$Observed[22]/est3_period_gorb$Estimator[22])




dat.gorb[15,1]<-"full"
dat.gorb[15,2]<-samp_comp_poll_gorb.21
dat.gorb[15,3]<-samp_comp_pl_gorb.21
dat.gorb[15,4]<-samp_comp_link_gorb.21
dat.gorb[16:20,1]<-"per site"
dat.gorb[16:20,2]<-c(est.21.gorb$Observed[1]/est.21.gorb$Estimator[1], est.21.gorb$Observed[4]/est.21.gorb$Estimator[4],
                     est.21.gorb$Observed[7]/est.21.gorb$Estimator[7],est.21.gorb$Observed[10]/est.21.gorb$Estimator[10],
                     est.21.gorb$Observed[13]/est.21.gorb$Estimator[13])
dat.gorb[16:20,3]<-c(est.21.gorb2$Observed[1]/est.21.gorb2$Estimator[1], est.21.gorb2$Observed[4]/est.21.gorb2$Estimator[4],
                     est.21.gorb2$Observed[7]/est.21.gorb2$Estimator[7],est.21.gorb2$Observed[10]/est.21.gorb2$Estimator[10],
                     est.21.gorb2$Observed[13]/est.21.gorb2$Estimator[13])

dat.gorb[16:20,4]<-c(est.21.gorb3$Observed[1]/est.21.gorb3$Estimator[1], est.21.gorb3$Observed[4]/est.21.gorb3$Estimator[4],
                     est.21.gorb3$Observed[7]/est.21.gorb3$Estimator[7],est.21.gorb3$Observed[10]/est.21.gorb3$Estimator[10],
                     est.21.gorb3$Observed[13]/est.21.gorb3$Estimator[13])

dat.gorb[21:27,1]<-"per period"

dat.gorb[21:27,2]<-c(est_period.21.gorb$Observed[1]/est_period.21.gorb$Estimator[1], est_period.21.gorb$Observed[4]/est_period.21.gorb$Estimator[4],
                     est_period.21.gorb$Observed[7]/est_period.21.gorb$Estimator[7],est_period.21.gorb$Observed[10]/est_period.21.gorb$Estimator[10],
                     est_period.21.gorb$Observed[13]/est_period.21.gorb$Estimator[13], est_period.21.gorb$Observed[16]/est_period.21.gorb$Estimator[16],
                     est_period.21.gorb$Observed[19]/est_period.21.gorb$Estimator[19])

dat.gorb[21:27,3]<-c(est2_period.21.gorb$Observed[1]/est2_period.21.gorb$Estimator[1], est2_period.21.gorb$Observed[4]/est2_period.21.gorb$Estimator[4],
                     est2_period.21.gorb$Observed[7]/est2_period.21.gorb$Estimator[7],est2_period.21.gorb$Observed[10]/est2_period.21.gorb$Estimator[10],
                     est2_period.21.gorb$Observed[13]/est2_period.21.gorb$Estimator[13], est2_period.21.gorb$Observed[16]/est2_period.21.gorb$Estimator[16],
                     est2_period.21.gorb$Observed[19]/est2_period.21.gorb$Estimator[19])

dat.gorb[21:27,4]<-c(est3_period_21_gorb$Observed[1]/est3_period_21_gorb$Estimator[1], est3_period_21_gorb$Observed[4]/est3_period_21_gorb$Estimator[4],
                     est3_period_21_gorb$Observed[7]/est3_period_21_gorb$Estimator[7],est3_period_21_gorb$Observed[10]/est3_period_21_gorb$Estimator[10],
                     est3_period_21_gorb$Observed[13]/est3_period_21_gorb$Estimator[13], est3_period_21_gorb$Observed[16]/est3_period_21_gorb$Estimator[16],
                     est3_period_21_gorb$Observed[19]/est3_period_21_gorb$Estimator[19])


dat.gorb[28,1]<-"full"
dat.gorb[28,2]<-samp_comp_poll_gorb.22
dat.gorb[28,3]<-samp_comp_pl_gorb.22
dat.gorb[28,4]<-samp_comp_link_gorb.22
dat.gorb[29:33,1]<-"per site"
dat.gorb[29:33,2]<-c(est.22.gorb$Observed[1]/est.22.gorb$Estimator[1], est.22.gorb$Observed[4]/est.22.gorb$Estimator[4],
                     est.22.gorb$Observed[7]/est.22.gorb$Estimator[7],est.22.gorb$Observed[10]/est.22.gorb$Estimator[10],
                     est.22.gorb$Observed[13]/est.22.gorb$Estimator[13])
dat.gorb[29:33,3]<-c(est.22.gorb2$Observed[1]/est.22.gorb2$Estimator[1], est.22.gorb2$Observed[4]/est.22.gorb2$Estimator[4],
                     est.22.gorb2$Observed[7]/est.22.gorb2$Estimator[7],est.22.gorb2$Observed[10]/est.22.gorb2$Estimator[10],
                     est.22.gorb2$Observed[13]/est.22.gorb2$Estimator[13])

dat.gorb[29:33,4]<-c(est.22.gorb3$Observed[1]/est.22.gorb3$Estimator[1], est.22.gorb3$Observed[4]/est.22.gorb3$Estimator[4],
                     est.22.gorb3$Observed[7]/est.22.gorb3$Estimator[7],est.22.gorb3$Observed[10]/est.22.gorb3$Estimator[10],
                     est.22.gorb3$Observed[13]/est.22.gorb3$Estimator[13])

dat.gorb[34:40,1]<-"per period"

dat.gorb[34:40,2]<-c(est_period.22.gorb$Observed[1]/est_period.22.gorb$Estimator[1], est_period.22.gorb$Observed[4]/est_period.22.gorb$Estimator[4],
                     est_period.22.gorb$Observed[7]/est_period.22.gorb$Estimator[7],est_period.22.gorb$Observed[10]/est_period.22.gorb$Estimator[10],
                     est_period.22.gorb$Observed[13]/est_period.22.gorb$Estimator[13], est_period.22.gorb$Observed[16]/est_period.22.gorb$Estimator[16],
                     est_period.22.gorb$Observed[19]/est_period.22.gorb$Estimator[19])


dat.gorb[34:40,3]<-c(est2_period.22.gorb$Observed[1]/est2_period.22.gorb$Estimator[1], est2_period.22.gorb$Observed[4]/est2_period.22.gorb$Estimator[4],
                     est2_period.22.gorb$Observed[7]/est2_period.22.gorb$Estimator[7],est2_period.22.gorb$Observed[10]/est2_period.22.gorb$Estimator[10],
                     est2_period.22.gorb$Observed[13]/est2_period.22.gorb$Estimator[13], est2_period.22.gorb$Observed[16]/est2_period.22.gorb$Estimator[16],
                     est2_period.22.gorb$Observed[19]/est2_period.22.gorb$Estimator[19])

dat.gorb[34:40,4]<-c(est3_period_22_gorb$Observed[1]/est3_period_22_gorb$Estimator[1], est3_period_22_gorb$Observed[4]/est3_period_22_gorb$Estimator[4],
                     est3_period_22_gorb$Observed[7]/est3_period_22_gorb$Estimator[7],est3_period_22_gorb$Observed[10]/est3_period_22_gorb$Estimator[10],
                     est3_period_22_gorb$Observed[13]/est3_period_22_gorb$Estimator[13], est3_period_22_gorb$Observed[16]/est3_period_22_gorb$Estimator[16],
                     est3_period_22_gorb$Observed[19]/est3_period_22_gorb$Estimator[19])


dat.gorb$site<-rep("Gorbea", nrow(dat.gorb))
dat.gorb[28:40, 6]<-"2022"
dat.gorb[15:27, 6]<-"2021"
dat.gorb[1:14, 6]<-"2020"




dat.total<-rbind(dat, dat.gorb)

dat.total$site<-as.factor(dat.total$site)
levels(dat.total$site) <- c("Doñana", "Gorbea")

head(dat.total)
colnames(dat.total)[2]<-"Pollinators"
colnames(dat.total)[3]<-"Plants"
colnames(dat.total)[4]<-"Links"

dat.total2 <- cbind(dat.total[c(1,5:6)], stack(dat.total[2:4]))

install.packages("wesanderson")
library(wesanderson)

Fig_complete<-ggplot(dat.total2, aes(x=sampling_type, y=values, fill=factor(V6)))+
  geom_boxplot() + facet_wrap(~site*ind) + scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1")) +
  ylab("Sampling completeness") + labs(fill="Year")+
  xlab("Network aggregation") + theme_bw() + ylim(0.2, 1)

ggsave("Figs/sampling.png",Fig_complete, width = 9, height = 4)

#create table with mean and SD values
library(tidyr)
library(dplyr)
dat.total3<- dat.total %>%
  group_by (sampling_type, V6, site) %>%
  summarize (mean_pol=mean(Pollinators), mean_pl=mean(Plants),
             mean_links=mean(Links), sd_poll=sd(Pollinators),
             sd_pl=sd(Plants), sd_links=sd(Links)) 

dat.total3<-as.data.frame(cbind(dat.total3[,1:3],round(dat.total3[,4:9],2)))
paste('a','b', sep="±")

dat.total4<-cbind(dat.total3[,1:3], paste(dat.total3[,4], dat.total3[,7], sep="±"),
                  paste(dat.total3[,5], dat.total3[,8], sep="±"),
                  paste(dat.total3[,6], dat.total3[,9], sep="±"))

colnames(dat.total4)<-c("Aggregation level", "Year", "Study area", "Pollinators",
                        "Plants", "Links")

save(Fig_complete,dat.total3, dat.total4, file = "RData/Sampl_compl_ECOLOGY.RData")


glimpse(d.22.gorb)
glimpse(d.21.gorb)

d.22.gorb<-d.22.gorb%>%
  rename(Periodo=Ronda,
         Pollinator_id=Polinizador_curro)





