

source("./scripts/sampling_completness.R")

##doñana 2020 
poll.d20
plant.d20
link.d20

poll.sites.d20
plant.sites.d20
link.sites.d20

poll.period.d20
plant.period.d20
link.period.d20

##doñana 2021
poll.d21
plant.d21
link.d21

poll.sites.d21
plant.sites.d21
link.sites.d21

poll.period.d21
plant.period.d21
link.period.d21

##gorbea 2020
poll.gorb.20
plant.gorb.20
link.gorb.20

poll.sites.gorb20
plant.sites.gorb20
link.sites.gorb20

poll.period.gorb20
plant.period.gorb20
link.period.gorb20

##gorbea 2021
poll.gorb21
plant.gorb21
link.gorb21

poll.sites.gorb21
plant.sites.gorb21
link.sites.gorb21

poll.period.gorb21
plant.period.gorb21
link.period.gorb21

## gorbea 2022
poll.gorb22
plant.gorb22
link.gorb22

poll.sites.gorb22
plant.sites.gorb22
link.sites.gorb22

poll.period.gorb22
plant.period.gorb22
link.period.gorb22

library(cowplot)
title <- ggdraw() + 
  draw_label(
    "Sampling completeness for pollinator and plant species and plant-pollinator links in Gorbea",
    fontface = 'bold', x = 0, hjust = 0)+
  theme(plot.margin = margin(0, 0, 0, 7))

formato<-theme_set(theme_bw()+theme(legend.position = element_blank(), 
                                    axis.title=element_blank(),
                                    axis.text = element_blank()))

fig<- plot_grid(poll.gorb.20+formato, poll.gorb21+formato, poll.gorb22+formato,
                labels=c("2020","2021","2022"),
                nrow=1,ncol=3,hjust=-4.5, vjust=0.45,label_size=12)+
  draw_label("Pollinator sp. richness", x=0, y=0.5, vjust= 0.1, angle=90, size=10)

fig2<- plot_grid(plant.gorb.20+ formato, plant.gorb21+formato, plant.gorb22+formato,
                 nrow=1, ncol=3)+
  draw_label("Plant sp. richness", x=0, y=0.5, vjust= 0.1, angle=90, size=10)


fig3<- plot_grid(link.gorb.20+ formato, link.gorb21+formato, link.gorb22+formato,
                 nrow=1, ncol=3)+
  draw_label("Link richness", x=0, y=0.5, vjust= 0.1, angle=90, size=10)

fig.t<-plot_grid(fig,fig2,fig3, ncol=1)

fig.t<-plot_grid(title,fig.t,ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.15, 1))+ #perhaps reduce this for a bit more space
  draw_label("number of observations", x=0.5, y= 0, vjust=0.4, angle= 0)+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 
  
ggsave("Figs/sampl.comp/samplfull.png",fig.t, width = 7, height = 6)


fig.site<- plot_grid(poll.sites.gorb20+formato, poll.sites.gorb21+formato, poll.sites.gorb22+formato,
                labels=c("2020","2021","2022"),
                nrow=1,ncol=3,hjust=-4.5, vjust=0.45,label_size=12)+
  draw_label("Pollinator sp. richness", x=0, y=0.5, vjust= 0.1, angle=90, size=10)

fig2.site<- plot_grid(plant.sites.gorb20+formato, plant.sites.gorb21+formato, plant.sites.gorb22+formato,
                      nrow=1, ncol=3)+
  draw_label("Plant sp. richness", x=0, y=0.5, vjust= 0.1, angle=90, size=10)


fig3.site<- plot_grid(link.sites.gorb20+formato, link.sites.gorb21+formato, link.sites.gorb22+formato,
                 nrow=1, ncol=3)+
  draw_label("Link richness", x=0, y=0.5, vjust= 0.1, angle=90, size=10)

legend<-cowplot::get_plot_component(poll.sites.gorb20, 'guide-box')
legend.s<- ggdraw(legend)
fig.t.site<-plot_grid(fig.site,fig2.site, fig3.site,ncol=1)

title.s<- ggdraw() + 
  draw_label(
    "Sampling completeness for pollinator and plant species and plant-pollinator links in different sites of Gorbea",
    fontface = 'bold', x = 0, hjust = 0)+
  theme(plot.margin = margin(0, 0, 0, 7))



fig.t.site<-plot_grid(title.s,fig.t.site,ncol = 1,
                 # rel_heights values control vertical title margins
                 rel_heights = c(0.15, 1), legend="bottom")+ #perhaps reduce this for a bit more space
  draw_label("number of observations", x=0.5, y= 0, vjust=0.4, angle= 0)+
  draw_plot(legend, x = .85, width = .2)+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 

ggsave("Figs/sampl.comp/sampl.site.png",fig.t.site, width = 7, height = 6)


fig.period<- plot_grid(poll.period.gorb20+formato, poll.period.gorb21+formato, poll.period.gorb22+formato,
                     labels=c("2020","2021","2022"),
                     nrow=1,ncol=3,hjust=-4.5, vjust=0.45,label_size=12)+
  draw_label("Pollinator sp. richness", x=0, y=0.5, vjust= 0.1, angle=90)


fig2.period<- plot_grid(plant.period.gorb20+formato, plant.period.gorb21+formato, plant.period.gorb22+formato,
                      nrow=1, ncol=3)+
  draw_label("Plant sp. richness", x=0, y=0.5, vjust= 0.1, angle=90)

fig3.period<- plot_grid(link.period.gorb20+formato, link.period.gorb21+formato, link.period.gorb22+formato,
                        nrow=1, ncol=3)+
  draw_label("Link richness", x=0, y=0.5, vjust= 0.1, angle=90)

legend<-cowplot::get_plot_component(poll.period.gorb20, 'guide-box')
legend.s<- ggdraw(legend)
fig.t.p<-plot_grid(fig.period,fig2.period, fig3.period,ncol=1)

title.p<- ggdraw() + 
  draw_label(
    "Sampling completeness for pollinator and plant species and plant-pollinator links through time in Gorbea",
    fontface = 'bold', x = 0, hjust = 0)+
  theme(plot.margin = margin(0, 0, 0, 7))


fig.t.period<-plot_grid(title.p,fig.t.p,ncol = 1,
                      # rel_heights values control vertical title margins
                      rel_heights = c(0.15, 1))+ #perhaps reduce this for a bit more space
  draw_label("number of observations", x=0.5, y= 0, vjust=0.4, angle= 0)+
  draw_plot(legend, x = .85, width = .2)+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 

