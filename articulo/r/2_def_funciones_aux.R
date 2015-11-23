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

sixnum <- function(x, na.rm = TRUE) {
  # Calcula estadísticos de resumen de un vector de datos
  five <- fivenum(x, na.rm = na.rm)
  six <- c(five[1:2], mean(x, na.rm = na.rm), five[3:5])
  names(six) <- c('Min', 'Q1', 'Mean', 'Median', 'Q3', 'Max')
  return(six)
}

calc_fecha_muerte <- function(anio_nac, mes_nac, mes_mte) {
  # Calcula el año de fallecioento de los niños menores de un año
  
  anio_mte <- anio_nac
  
  for (i in seq_along(anio_nac)) {
    if (anyNA(c(anio_nac[i], mes_nac[i], mes_mte[i]))) {
      anio_mte[i] <- NA
    } else if (mes_nac[i] + mes_mte[i] > 12) {
      anio_mte[i] <- anio_nac[i] + 1
    } else {
      anio_mte[i] <- anio_nac[i]
    }
  }
  
  return(anio_mte)
}
