# ------------------------------------------------------------------------------
#
# Artículo sobre mortalidad infantil en Antioquia
# Juan Pablo Calle Quintero
# jpcalleq@unal.edu.co
#
# ------------------------------------------------------------------------------

# Este gion contiene las funciones auxiliares definidas para el análisis

# Un gráfico en ocasiones útil
blankPlot <- ggplot() + geom_blank(aes(1, 1)) +
  theme(
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )
