library(rworldmap)

newmap <- getMap(resolution = "low")
plot(newmap)
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)
plot(newmap, xlim = c(-75, -70), ylim = c(-5, 12), asp = 1)
points(datos_orig[, lon], datos_orig[, lat], col = "red", cex = .6)

library(ggmap)
pos_col <- c(-80, -5, -65, 15)
map <- get_map(location = 'Colombia', zoom = 5)
map <- get_map(location = pos_col, zoom = 5)
ggmap(map) + geom_point(aes(x = lon, y = lat, colour = alt_gps),
                        data = datos_orig, alpha = 0.5)
