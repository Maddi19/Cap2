

source("./scripts/sampling_completness.R")
load("./scripts/plots_full.gorbea.RData")

##doñana 
poll.d20
plant.d20
link.d20
poll.d21
plant.d21
link.d21

poll.sites.d20
plant.sites.d20
link.sites.d20
poll.sites.d21
plant.sites.d21
link.sites.d21

poll.period.d20
plant.period.d20
link.period.d20
poll.period.d21
plant.period.d21
link.period.d21




##gorbea 
poll.gorb20
plant.gorb.20
link.gorb20
poll.gorb21
plant.gorb21
link.gorb21
poll.gorb22
plant.gorb22
link.gorb22

poll.sites.gorb20
plant.sites.gorb20
link.sites.gorb20
poll.sites.gorb21
plant.sites.gorb21
link.sites.gorb21
poll.sites.gorb22
plant.sites.gorb22
link.sites.gorb22

poll.period.gorb20
plant.period.gorb20
link.period.gorb20
poll.period.gorb21
plant.period.gorb21
link.period.gorb21
poll.period.gorb22
plant.period.gorb22
link.period.gorb22






library(cowplot)
title <- ggdraw() +
  draw_label(
    "Gorbea",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))


fig <- plot_grid(
  poll.gorb20, 
  poll.gorb21, 
  poll.gorb22,
  labels = c("2020", "2021", "2022"),
  nrow = 1,
  ncol = 3,
  hjust = -4.5,
  vjust = 0.45,
  label_size = 12,
  rel_heights = c(0.6),  
  rel_widths = c(1.5, 1.5, 1.5)  
) + 
  draw_label(
    "Pollinator sp. richness",
    x = 0,
    y = 0.5,
    vjust = 0.1,
    angle = 90,
    size = 10
  )


fig2 <- plot_grid(
  plant.gorb.20 ,
  plant.gorb21 ,
  plant.gorb22 ,
  nrow = 1,
  ncol = 3
) +
  draw_label(
    "Plant sp. richness",
    x = 0,
    y = 0.5,
    vjust = 0.1,
    angle = 90,
    size = 10
  )


fig3 <- plot_grid(
  link.gorb20 ,
  link.gorb21 ,
  link.gorb22 ,
  nrow = 1,
  ncol = 3
) +
  draw_label(
    "Link richness",
    x = 0,
    y = 0.5,
    vjust = 0.1,
    angle = 90,
    size = 10
  )

fig.t <- plot_grid(fig, fig2, fig3, ncol = 1)

fig.t <- plot_grid(title, fig.t, ncol = 1, # rel_heights values control vertical title margins
                   rel_heights = c(0.15, 1)) + #perhaps reduce this for a bit more space
  draw_label(
    "number of observations",
    x = 0.5,
    y = 0,
    vjust = 0.4,
    angle = 0
  ) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

save(fig.t, file = "full_sampling.RData")

ggsave("Figs/sampl.comp/samplfull.png",
       fig.t,
       width = 7,
       height = 6)



fig.site <- plot_grid(
  poll.sites.gorb20 ,
  poll.sites.gorb21 ,
  poll.sites.gorb22 ,
  labels = c("2020", "2021", "2022"),
  nrow = 1,
  ncol = 3,
  hjust = -4.5,
  vjust = 0.45,
  label_size = 12
) +
  draw_label(
    "Pollinator sp. richness",
    x = 0,
    y = 0.5,
    vjust = 0.1,
    angle = 90,
    size = 10
  )

fig2.site <- plot_grid(
  plant.sites.gorb20 ,
  plant.sites.gorb21 ,
  plant.sites.gorb22 ,
  nrow = 1,
  ncol = 3
) +
  draw_label(
    "Plant sp. richness",
    x = 0,
    y = 0.5,
    vjust = 0.1,
    angle = 90,
    size = 10
  )


fig3.site <- plot_grid(
  link.sites.gorb20 ,
  link.sites.gorb21 ,
  link.sites.gorb22 ,
  nrow = 1,
  ncol = 3
) +
  draw_label(
    "Link richness",
    x = 0,
    y = 0.5,
    vjust = 0.1,
    angle = 90,
    size = 10
  )

legend <- cowplot::get_plot_component(poll.sites.gorb20, 'guide-box')
legend.s <- ggdraw(legend)
fig.t.site <- plot_grid(fig.site, fig2.site, fig3.site, ncol = 1)

title.s <- ggdraw() +
  draw_label(
    "Sampling completeness for pollinator and plant species and plant-pollinator links in different sites of Gorbea",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))



fig.t.site <- plot_grid(
  title.s,
  fig.t.site,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.15, 1),
  legend = "bottom"
) + #perhaps reduce this for a bit more space
  draw_label(
    "number of observations",
    x = 0.5,
    y = 0,
    vjust = 0.4,
    angle = 0
  ) +
  draw_plot(legend, x = .85, width = .2) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

ggsave("Figs/sampl.comp/sampl.site.png",
       fig.t.site,
       width = 7,
       height = 6)


fig.period <- plot_grid(
  poll.period.gorb20,
  poll.period.gorb21 ,
  poll.period.gorb22,
  labels = c("2020", "2021", "2022"),
  nrow = 1,
  ncol = 3,
  hjust = -4.5,
  vjust = 0.45,
  label_size = 12
) +
  draw_label(
    "Pollinator sp. richness",
    x = 0,
    y = 0.5,
    vjust = 0.1,
    angle = 90,
    size = 8
  )


fig2.period <- plot_grid(
  plant.period.gorb20,
  plant.period.gorb21,
  plant.period.gorb22,
  nrow = 1,
  ncol = 3
) +
  draw_label(
    "Plant sp. richness",
    x = 0,
    y = 0.5,
    vjust = 0.1,
    angle = 90,
    size = 8
  )

fig3.period <- plot_grid(
  link.period.gorb20 ,
  link.period.gorb21,
  link.period.gorb22 ,
  nrow = 1,
  ncol = 3
) +
  draw_label(
    "Link richness",
    x = 0,
    y = 0.5,
    vjust = 0.1,
    angle = 90,
    size = 8
  )


legend.period.g 
legend <- cowplot::get_plot_component(poll.period.gorb20, 'guide-box')
legend.s <- ggdraw(legend.period.g )
legend.d <- ggdraw(legend.period.d21 )

fig.t.p <- plot_grid(fig.period, fig2.period, fig3.period, ncol = 1, rel_heights = c(1.4,1.4,1.4))

title.p <- ggdraw() +
  draw_label(
    "Sampling completeness for pollinator and plant species and plant-pollinator links through time in Gorbea",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))



fig.t.period <- plot_grid(title.p,
                          fig.t.p,
                          ncol = 1,
                          rel_heights = c(0.15, 1)) +
  draw_label(
    "number of observations",
    x = 0.5,
    y = 0,
    vjust = 0.4,
    angle = 0
  )

# Paso 6: Combinar todo en una columna, con la leyenda al final
final_plot <- plot_grid(fig.t.period, legend.s, ncol = 1, rel_heights = c(1, 0.2))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))



ggsave("Figs/sampl.comp/sampl.period.png",
       final_plot,
       width = 7,
       height = 6)


##purrr
#doñana
load("plots_doñana.RData")
library(cowplot)
create_fig <- function(plots, title) {
  plot_grid(
    plotlist = plots,  # Combina los gráficos sin etiquetas
    nrow = 1,
    ncol = 2
  ) +
    draw_label(
      title,  # Este es el título que aparecerá a la izquierda
      x = 0,
      y = 0.5,
      vjust = 0.1,
      angle = 90,
      size = 8
    )
}

# Crear los gráficos de riqueza de especies
fig.poll.d <- create_fig(list(poll.d20, poll.d21), "Pollinator sp. richness")
fig.plant.d <- create_fig(list(plant.d20, plant.d21), "Plant sp. richness")
fig.link.d <- create_fig(list(link.d20, link.d21), "Link richness")



fig.poll.d.site <- create_fig(list(poll.sites.d20, poll.sites.d21), "Pollinator sp. richness")
fig.plant.d.site <- create_fig(list(plant.sites.d20, plant.sites.d21), "Plant sp. richness")
fig.link.d.site <- create_fig(list(link.sites.d20, link.sites.d21), "Link richness")


fig.poll.d.period <- create_fig(list(poll.period.d20, poll.period.d21), "Pollinator sp. richness")
fig.plant.d.period <- create_fig(list(plant.period.d20, plant.period.d21), "Plant sp. richness")
fig.link.d.period <- create_fig(list(link.period.d20, link.period.d21), "Link richness")


# Crear los títulos de los años
year_titles <- ggdraw() +
  draw_label("2020", x = 0.25, y = 7.7) + 
  draw_label("2021", x = 0.75, y = 7.7) + 
  theme(plot.margin = margin(0, 0, 0, 0))

fig.t.d <- plot_grid(fig.poll.d.site, fig.plant.d.site, fig.link.d.site, ncol = 1)
fig.per.d <- plot_grid(fig.poll.d.period, fig.plant.d.period, fig.link.d.period, ncol = 1, rel_heights = c(1.5,1.5,1.5))

# Título de la figura final
title.p <- ggdraw() +
  draw_label(
    "Sampling completeness",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

# Combinar todo en un solo gráfico con título del eje X
final_fig <- plot_grid(
  title.p, 
  fig.per.d, 
  year_titles,
  ncol = 1, 
  rel_heights = c(0.15, 1)
) + 
  draw_label("number of observations", x = 0.5, y = 0.05, vjust = 0.4, angle = 0) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))



# Guardar el gráfico final
ggsave("Figs/sampl.comp/samplfull.png", fig.t, width = 7, height = 6)
ggsave("Figs/sampl.comp/samplfulldon.png", final_fig, width = 7, height = 6)

ggsave("Figs/sampl.comp/sampl.sites.don.png", final_fig, width = 7, height = 6)



final_plot.d <- plot_grid(final_fig, legend.d, ncol = 1, rel_heights = c(1, 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

ggsave("Figs/sampl.comp/sampl.period.don.png", final_plot.d, width = 7, height = 6)



save(final_fig,
     file = "scripts/plots_full_doñ.RData")






##gorbea
create_fig <- function(plots, title) {
  plot_grid(
    plotlist = plots,  # Combina los gráficos sin etiquetas
    nrow = 1,
    ncol = 3
  ) +
    draw_label(
      title,  # Este es el título que aparecerá a la izquierda
      x = 0,
      y = 0.5,
      vjust = 0.1,
      angle = 90,
      size = 10
    )
}

# Crear los gráficos de riqueza de especies
fig.poll <- create_fig(list(poll.gorb20, poll.gorb21, poll.gorb22), "Pollinator sp. richness")
fig.plant <- create_fig(list(plant.gorb20, plant.gorb21, plant.gorb22), "Plant sp. richness")
fig.link <- create_fig(list(link.gorb20, link.gorb21, link.gorb22), "Link richness")

# Crear los títulos de los años
year_titles <- ggdraw() +
  draw_label("2020", x = 0.25, y = 7.7) + 
  draw_label("2021", x = 0.6, y = 7.7) + 
  draw_label("2022", x = 0.9, y = 7.7) +
  theme(plot.margin = margin(0, 0, 0, 0))

fig.t.p <- plot_grid(fig.poll, fig.plant, fig.link, ncol = 1)
# Título de la figura final
title.p <- ggdraw() +
  draw_label(
    "Sampling completeness",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

# Combinar todo en un solo gráfico con título del eje X
final_fig <- plot_grid(
  title.p, 
  fig.t.p, 
  year_titles,
  ncol = 1, 
  rel_heights = c(0.15, 1)
) + 
  draw_label("number of observations", x = 0.5, y = 0, vjust = 0.4, angle = 0) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Guardar el gráfico final
ggsave("Figs/sampl.comp/samplfull.png", fig.t, width = 7, height = 6)

save(final_fig,
     file = "scripts/plots_gorb.RData")
