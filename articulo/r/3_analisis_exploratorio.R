# ------------------------------------------------------------------------------
#
# Artículo sobre mortalidad infantil en el Chocó y la Guajira
# Juan Pablo Calle Quintero
# jpcalleq@unal.edu.co
#
# Este guion contiene el código del análisis exploratorio
# 
# ------------------------------------------------------------------------------

## Cargar librerias necesarias y funciones auxiliares --------------------------

source(file = '1_cargar_librerias.R')
source(file = '2_def_funciones_aux.R')


## Lectura de datos ------------------------------------------------------------

datos_orig <- fread(input =  '../datos/datos.csv', header = TRUE,
                    encoding = 'UTF-8')
depart <- fread(input = '../datos/departamentos.csv', header = TRUE,
                encoding = 'UTF-8')
gps_data <- fread(input = '../datos/gps_data.csv', header = TRUE,
                  encoding = 'UTF-8')

# solo estudiaremos los nacimientos en un periodo de 5 años 2005-2009
periodo_estudio <- as.character(2005:2009)
datos_orig <- datos_orig[anio_nac_hijo %in% periodo_estudio]
datos_orig[, nro_hijos := .N, by = id_madre]

setkey(datos_orig, id_madre)
setkey(depart, id_dep)
setkey(gps_data, cluster)

datos_orig[departamento == 27L, nom_depart := 'Chocó']
datos_orig[departamento == 44L, nom_depart := 'La Guajira']
datos_orig[!(departamento %in% c(27L, 44L)), nom_depart := 'Otros']
datos_orig[, .N, by = .(departamento, nom_depart)]
gps_data[fuente_gps == 'MIS', c('lat', 'lon', 'alt_gps', 'alt_dem') := .(NA)]

datos_orig <- merge(datos_orig,
                    gps_data[, .(cluster, lat, lon, alt_gps, alt_dem)],
                    by = 'cluster', suffixes = c('.orig', '.gps'))

tables() # ver resumen base de datos orginal

# Creamos dos bases de datos para comparar:
# datos_1m: solo contiene los hijos de las madres que han perdido por lo menos
#           un hijo menor de un año. Estan tanto los hijos vivos como los
#           fallecidos.
# datos_0m: contienen los datos de los hijos de las madres que no han perdido
#           ningún hijo.
# Note que si alguna madre perdió un hijo antes del año 2005, y no ha perdido
# hijos posteriormente, quedará dentro de la base datos_0m

si_ha_perdido_hijos12 <- unique(datos_orig[edad_muerte_meses < 12, id_madre])

datos_1m <- datos_orig[id_madre %in% si_ha_perdido_hijos12]
datos_0m <- datos_orig[!(id_madre %in% si_ha_perdido_hijos12)]

tables()

# Número de niños <= a un año fallecidos por departamento

tot_nacim <- datos_orig[, .(tot_nac = .N), by = departamento]
setkey(tot_nacim, departamento)
fallecidos_dep <- datos_1m[edad_muerte_meses < 12,
                           .(tot_fall = .N),
                           by = departamento]
setkey(fallecidos_dep, departamento)
tbl_x_dep <- fallecidos_dep[depart][tot_nacim][,
                                               .(dep, tot_fall, tot_nac)
                                               ][,
                                                 prop := 100 * tot_fall/tot_nac
                                                 ][order(-tot_fall)]

xtable(tbl_x_dep)
write.csv(tbl_x_dep, file = '../datos/tabla_departamentos.csv',
          row.names = FALSE, fileEncoding = 'UTF-8')


### Listas de variables con código original de la base de datos ----------------
# Importante para poder buscar las definiciones en el diccionario
# Para ver definiciones ir a: COBR61FL_BIRTH_RECODE.MAP
#                             COPR61FL_HOUSEHOLD_MEMBER_RECODE.MAP
variables_usadas_hogar <-
  list(
    id_hogar = 'hhid',
    padre_vivo = 'hv113',
    nro_habitaciones_hogar = 'hv216'
  )

variables_usadas_nacimientos <- 
  list(
    id_madre = 'caseid',
    peso_muestra = 'v005',
    edad_madre = 'v012',
    departamento = 'v023',
    region = 'v024',
    tipo_lugar_residencia = 'v025',
    anios_resid = 'v104',
    max_nivel_edu = 'v106',
    fuente_agua = 'v113',
    tipo_banio = 'v116',
    energia = 'v119',
    radio = 'v120',
    television = 'v121',
    nevera = 'v122',
    moto = 'v124',
    carro = 'v125',
    etnia = 'v131',
    nro_personas_hogar = 'v136',
    nro_ninios_hogar5 = 'v137',
    nro_mujeres_eleg_hogar = 'v138',
    rel_cabeza_fam = 'v150',
    sexo_cabeza_fam = 'v151',
    edad_cabeza_familia = 'v152',
    tipo_combustible_cocina = 'v161',
    hijo_nro = 'bidx',
    twin_code = 'b0',
    mes_nac_hijo = 'b1',
    anio_nac_hijo = 'b2',
    siglo_nac_hijo = 'b3',
    sexo_hijo = 'b4',
    hijo_vivo_muerto = 'b5',
    edad_muerte_meses = 'b7',
    edad_hijo_anios = 'b8',
    tpo_entre_nac_meses = 'b11',
    nro_hijos_muertos = 'v206',
    nro_hijas_muertas = 'v207',
    nro_hijos5 = 'v208',
    nro_hijos1 = 'v209',
    edad_madre_1hijo = 'v212',
    ind_embarazos_fallidos = 'v228',
    meses_embarazo_fallido = 'v233',
    tpo_amamanta_meses = 'm5',
    hijo_deseado = 'm10',
    nacimiento_cesarea = 'm17',
    peso_al_nacer_kg = 'm19',
    quien_tomo_peso = 'm19a',
    edad_actual_hijo_meses = 'hw1',
    peso_actual_hijo_kg = 'hw2',
    talla_actual_hijo_cm = 'hw3',
    perc_talla_edad = 'hw4',
    perc_peso_edad = 'hw7',
    perc_peso_talla = 'hw10',
    estado_civil_madre = 'v501',
    si_ha_estado_casada = 'v502',
    edad_primera_rel_sexual = 'v531'
  )


## Tablas de contingencia por variable de interés ------------------------------

# número de observaciones en la base de datos
(nro_obs_1m <- datos_1m[, .N])
(nro_obs_0m <- datos_0m[, .N])

# las madres que han perdido hijos menores de 1 año en el periodo de estudio
(nro_madres_1m <- datos_1m[, .N, by = id_madre][, .N])
# las madres que NO han perdido hijos en el periodo de estudio
(nro_madres_0m <- datos_0m[, .N, by = id_madre][, .N])
(nro_madres_1m / (nro_madres_0m + nro_madres_1m))

# Número de niños menores de un año que han fallecido por año
datos_1m[edad_muerte_meses < 12, .N, by = anio_nac_hijo][order(anio_nac_hijo)]

datos_1m[,
         anio_mte := calc_fecha_muerte(anio_nac_hijo, mes_nac_hijo,
                                         edad_muerte_meses)]

# cuantos menores de un año murieron por año
muertes <- datos_1m[edad_muerte_meses < 12 & anio_mte %in% periodo_estudio,
                    .N, by = anio_mte]

# cuantos niños nacieron por año
nacimientos <- datos_orig[, .N, by = anio_nac_hijo]

# Tasa mortalidad infantil por año
1000 * (muertes[, N] / nacimientos[, N])

# Distribución de fallecimientos por departamento
pdf('../img/densidades_edad_fallecimiento.pdf', width = 7, height = 5)
ggplot(data = datos_orig[hijo_vivo_muerto == 0]) + theme_bw(12) +
  geom_density(aes(x = edad_muerte_meses, linetype = nom_depart)) +
  scale_linetype_manual(values = c('solid', 'longdash', 'dotted')) +
  labs(x = 'edada fallecimiento [meses]', y = 'densidad') +
  theme(
    legend.title = element_blank()
  )
dev.off()

# # Método de Brass?
# nro_hijos_nacidos_vivos <- 
#   datos_orig[, .N, by = anio_nac_hijo]
# 
# nro_total_mujeres <- 
#   datos_orig[, .(N = length(unique(id_madre))), by = anio_nac_hijo]
# 
# P_i <- nro_hijos_nacidos_vivos[, N] / nro_total_mujeres[, N]
# 
# nro_hijos_fallecidos <- 
#   datos_1m[edad_muerte_meses < 12, .N, by = anio_nac_hijo]
# 
# D_i <- nro_hijos_fallecidos[, N] / nro_hijos_nacidos_vivos[, N]
# 
# tasa_mort_inf <- D_i * 1000

# Número de niños fallecidos por departamento
datos_1m[edad_muerte_meses < 12, .N, by = nom_depart]
datos_orig[, .N, by = nom_depart]

# hijo_deseado 1: Sí, 2: Sí pero después, 3: No
datos_1m[, table(nom_depart, hijo_deseado, useNA = 'ifany')]
datos_0m[, table(nom_depart, hijo_deseado, useNA = 'ifany')]
datos_1m[, table(nom_depart, hijo_deseado, useNA = 'ifany') / .N]
datos_0m[, table(nom_depart, hijo_deseado, useNA = 'ifany') / .N]

prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, hijo_deseado, useNA = 'ifany')],
           margin = 1)
prop.table(datos_orig[, table(nom_depart, hijo_deseado, useNA = 'ifany')],
           margin = 1)

# padre_vivo: 0: No, 1: Sí, 8: No sabe (solo aplica para los niños < de 17 años)
datos_1m[, table(nom_depart, padre_vivo, useNA = 'ifany')]
datos_0m[, table(nom_depart, padre_vivo, useNA = 'ifany')]
datos_1m[, table(nom_depart, padre_vivo, useNA = 'ifany') / .N]
datos_0m[, table(nom_depart, padre_vivo, useNA = 'ifany') / .N]

prop.table(datos_1m[, table(nom_depart, padre_vivo, useNA = 'ifany')],
           margin = 1)
prop.table(datos_0m[, table(nom_depart, padre_vivo, useNA = 'ifany')],
           margin = 1)

# edad_muerte_meses: edad
datos_1m[edad_muerte_meses < 12,
         table(nom_depart, edad_muerte_meses, useNA = 'ifany')]
datos_1m[edad_muerte_meses < 12,
         table(nom_depart, edad_muerte_meses, useNA = 'ifany') / .N]

prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, edad_muerte_meses, useNA = 'ifany')],
           margin = 1)

# estado_civil_madre:
# 0: solt, 1: casada, 2: unión libre, 3: viuda, 4: divorciada, 5: no viven junt
datos_1m[, table(nom_depart, estado_civil_madre) / .N]
datos_0m[, table(nom_depart, estado_civil_madre) / .N]

prop.table(datos_1m[, table(nom_depart, estado_civil_madre)],
           margin = 1)
prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, estado_civil_madre)],
           margin = 1)
prop.table(datos_0m[, table(nom_depart, estado_civil_madre)],
           margin = 1)

datos_1m[edad_muerte_meses < 12, table(estado_civil_madre) / .N]
datos_0m[, table(estado_civil_madre) / .N]

# total_hijos_muertos: número de hijos fallecidos.
datos_1m[, table(nom_depart, total_hijos_muertos, useNA = 'ifany') / .N]
datos_0m[, table(nom_depart, total_hijos_muertos, useNA = 'ifany') / .N]

# edad_madre_1hijo: edad en años (entre 10 y 49)
datos_1m[, table(nom_depart, edad_madre_1hijo, useNA = 'ifany')]
datos_0m[, table(nom_depart, edad_madre_1hijo, useNA = 'ifany')]
datos_1m[, table(nom_depart, edad_madre_1hijo, useNA = 'ifany') / .N]
datos_0m[, table(nom_depart, edad_madre_1hijo, useNA = 'ifany') / .N]

datos_1m[edad_muerte_meses < 12,
         table(nom_depart, edad_madre_1hijo, useNA = 'ifany')]

datos_1m[, sixnum(edad_madre_1hijo), by = nom_depart]
datos_0m[, sixnum(edad_madre_1hijo), by = nom_depart]

# edad_madre: edad en años (entre 13 y 49)
datos_1m[, table(nom_depart, edad_madre, useNA = 'ifany')]
datos_0m[, table(nom_depart, edad_madre, useNA = 'ifany')]
datos_1m[, table(nom_depart, edad_madre, useNA = 'ifany') / .N]
datos_0m[, table(nom_depart, edad_madre, useNA = 'ifany') / .N]

datos_1m[, sixnum(edad_madre), by = nom_depart]
datos_0m[, sixnum(edad_madre), by = nom_depart]

# edad_primera_rel_sexual: edad en años, NA
datos_1m[, table(nom_depart, edad_primera_rel_sexual, useNA = 'ifany')]
datos_0m[, table(nom_depart, edad_primera_rel_sexual, useNA = 'ifany')]
datos_1m[, table(nom_depart, edad_primera_rel_sexual, useNA = 'ifany') / .N]
datos_0m[, table(nom_depart, edad_primera_rel_sexual, useNA = 'ifany') / .N]

datos_1m[, sixnum(edad_primera_rel_sexual), by = nom_depart]
datos_0m[, sixnum(edad_primera_rel_sexual), by = nom_depart]

# tpo_entre_nac_meses: tiempo entre nacimientos meses, NA para  primer hijo
datos_1m[, table(nom_depart, tpo_entre_nac_meses, useNA = 'ifany')]
datos_0m[, table(nom_depart, tpo_entre_nac_meses, useNA = 'ifany')]
datos_1m[, table(nom_depart, tpo_entre_nac_meses, useNA = 'ifany') / .N]
datos_0m[, table(nom_depart, tpo_entre_nac_meses, useNA = 'ifany') / .N]

datos_1m[edad_muerte_meses < 12, sixnum(tpo_entre_nac_meses), by = nom_depart]
datos_1m[, sixnum(tpo_entre_nac_meses), by = nom_depart]
datos_0m[, sixnum(tpo_entre_nac_meses), by = nom_depart]

ggplot(data = datos_orig) + theme_bw(10) +
  geom_density(aes(x = tpo_entre_nac_meses, colour = nom_depart))

# tipo_lugar_residencia: 1: urbano, 2: rural
datos_1m[, table(nom_depart, tipo_lugar_residencia, useNA = 'ifany')]
datos_0m[, table(nom_depart, tipo_lugar_residencia, useNA = 'ifany')]
datos_1m[, table(nom_depart, tipo_lugar_residencia, useNA = 'ifany') / .N]
datos_0m[, table(nom_depart, tipo_lugar_residencia, useNA = 'ifany') / .N]

prop.table(datos_1m[,
                    table(nom_depart, tipo_lugar_residencia, useNA = 'ifany')],
           margin = 1)
prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, tipo_lugar_residencia, useNA = 'ifany')],
           margin = 1)
prop.table(datos_0m[,
                    table(nom_depart, tipo_lugar_residencia, useNA = 'ifany')],
           margin = 1)

# max_nivel_edu (madre): 0: sin edu, 1: Primaria, 2: secundaria, 3: superior
datos_1m[, table(nom_depart, max_nivel_edu, useNA = 'ifany')]
datos_0m[, table(nom_depart, max_nivel_edu, useNA = 'ifany')]
datos_1m[, table(nom_depart, max_nivel_edu, useNA = 'ifany') / .N]
datos_0m[, table(nom_depart, max_nivel_edu, useNA = 'ifany') / .N]

prop.table(datos_1m[, table(nom_depart, max_nivel_edu, useNA = 'ifany')],
           margin = 1)
prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, max_nivel_edu, useNA = 'ifany')],
           margin = 1)
prop.table(datos_0m[, table(nom_depart, max_nivel_edu, useNA = 'ifany')],
           margin = 1)

# fuente_agua:
# 11: acueducto emp urbana, 12: acueducto sist rural, 13: grifo público
# 21: fuente pública con bomba, 22: fuente pública sin bomba, 42: rio,
# 51: lluvia, 71: embotellada, 96: otra.
datos_1m[, table(nom_depart, fuente_agua, useNA = 'ifany')]
datos_0m[, table(nom_depart, fuente_agua, useNA = 'ifany')]
datos_1m[, table(nom_depart, fuente_agua, useNA = 'ifany') / .N]
datos_0m[, table(nom_depart, fuente_agua, useNA = 'ifany') / .N]

prop.table(datos_1m[, table(nom_depart, fuente_agua, useNA = 'ifany')],
           margin = 1)
prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, fuente_agua, useNA = 'ifany')],
           margin = 1)
prop.table(datos_0m[, table(nom_depart, fuente_agua, useNA = 'ifany')],
           margin = 1)

datos_agua <- datos_orig[, .N, by = .(nom_depart, fuente_agua)]
datos_agua[, .(prop = N / sum(N))]
nom_fuente_agua <- function(x) {
  nom_agua <- vector(length = length(x))
  for (i in seq_along(x)) {
    if (x[i] == 11 | x[i] == 12) {
      nom_agua[i] <- 'Acueducto'
    } else if (x[i] == 13 | x[i] == 21 | x[i] == 22) {
      nom_agua[i] <- 'Fuente pública'
    } else if (x[i] == 42) {
      nom_agua[i] <- 'Rio'
    } else if (x[i] == 51) {
      nom_agua[i] <- 'Lluvia'
    } else {
      nom_agua[i] <- 'Otra'
    }
  }
  return(nom_agua)
}

datos_agua[, fuente_agua := nom_fuente_agua(fuente_agua)][,]

ggplot(data = datos_orig) + theme_bw(10) +
  geom_bar(aes(x = fuente_agua, group = nom_depart))

# tipo_banio:
# 11: inodoro con alcant, 12: inodoro poso séptico, 13: inodoro al patio,
# 21: letrina, 22: letrina al rio, 31: no tiene baño
datos_1m[edad_muerte_meses < 12,
         table(tipo_banio, useNA = 'ifany')]
datos_1m[edad_muerte_meses < 12,
         table(tipo_banio, useNA = 'ifany') / .N]
datos_0m[edad_hijo_anios < 1,
         table(tipo_banio, useNA = 'ifany')]
datos_0m[edad_hijo_anios < 1,
         table(tipo_banio, useNA = 'ifany') / .N]

prop.table(datos_1m[, table(nom_depart, tipo_banio, useNA = 'ifany')],
           margin = 1)
prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, tipo_banio, useNA = 'ifany')],
           margin = 1)
prop.table(datos_0m[, table(nom_depart, tipo_banio, useNA = 'ifany')],
           margin = 1)

# energía: 0: No, 1: Sí
datos_1m[edad_muerte_meses < 12,
         table(energia, useNA = 'ifany')]
datos_1m[edad_muerte_meses < 12,
         table(energia, useNA = 'ifany') / .N]
datos_0m[edad_hijo_anios < 1,
         table(energia, useNA = 'ifany')]
datos_0m[edad_hijo_anios < 1,
         table(energia, useNA = 'ifany') / .N]

prop.table(datos_1m[, table(nom_depart, energia, useNA = 'ifany')],
           margin = 1)
prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, energia, useNA = 'ifany')],
           margin = 1)
prop.table(datos_0m[, table(nom_depart, energia, useNA = 'ifany')],
           margin = 1)

# nro_personas_hogar: número entero
datos_1m[edad_muerte_meses < 12, sixnum(nro_personas_hogar)]
datos_0m[, sixnum(nro_personas_hogar)]

# hijo_nro: posición del hijo dentro de la familia
datos_1m[edad_muerte_meses < 12,
         table(hijo_nro, useNA = 'ifany')]
datos_1m[edad_muerte_meses < 12,
         table(hijo_nro, useNA = 'ifany') / .N]
datos_0m[edad_hijo_anios < 1,
         table(hijo_nro, useNA = 'ifany')]
datos_0m[edad_hijo_anios < 1,
         table(hijo_nro, useNA = 'ifany') / .N]

prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, hijo_nro, useNA = 'ifany')],
           margin = 1)

# sexo_cabeza_fam: 1: hombre, 2: mujer
datos_1m[edad_muerte_meses < 12,
         table(sexo_cabeza_fam, useNA = 'ifany')]
datos_1m[edad_muerte_meses < 12,
         table(sexo_cabeza_fam, useNA = 'ifany') / .N]
datos_0m[edad_hijo_anios < 1,
         table(sexo_cabeza_fam, useNA = 'ifany')]
datos_0m[edad_hijo_anios < 1,
         table(sexo_cabeza_fam, useNA = 'ifany') / .N]

prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, sexo_cabeza_fam, useNA = 'ifany')],
           margin = 1)
prop.table(datos_0m[, table(nom_depart, sexo_cabeza_fam, useNA = 'ifany')],
           margin = 1)

# edad_cabeza_familia: número
datos_1m[edad_muerte_meses <= 12,
         table(edad_cabeza_familia, useNA = 'ifany')]
datos_1m[edad_muerte_meses <= 12,
         table(edad_cabeza_familia, useNA = 'ifany') / .N]
datos_0m[edad_hijo_anios <= 1,
         table(edad_cabeza_familia, useNA = 'ifany')]
datos_0m[edad_hijo_anios <= 1,
         table(edad_cabeza_familia, useNA = 'ifany') / .N]

datos_1m[edad_muerte_meses < 12, sixnum(edad_cabeza_familia)]
datos_0m[edad_hijo_anios < 1, sixnum(edad_cabeza_familia)]

# sexo_hijo: 1: hombre, 2: mujer
datos_1m[edad_muerte_meses < 12,
         table(sexo_hijo, useNA = 'ifany')]
datos_1m[edad_muerte_meses < 12,
         table(sexo_hijo, useNA = 'ifany') / .N]
datos_0m[edad_hijo_anios < 1,
         table(sexo_hijo, useNA = 'ifany')]
datos_0m[edad_hijo_anios < 1,
         table(sexo_hijo, useNA = 'ifany') / .N]

prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, sexo_hijo, useNA = 'ifany')],
           margin = 1)
prop.table(datos_0m[, table(nom_depart, sexo_hijo, useNA = 'ifany')],
           margin = 1)

# ind_embarazos_fallidos: 0: No, 1: Sí
datos_1m[edad_muerte_meses < 12,
         table(ind_embarazos_fallidos, useNA = 'ifany')]
datos_1m[edad_muerte_meses < 12,
         table(ind_embarazos_fallidos, useNA = 'ifany') / .N]
datos_0m[edad_hijo_anios < 1,
         table(ind_embarazos_fallidos, useNA = 'ifany')]
datos_0m[edad_hijo_anios < 1,
         table(ind_embarazos_fallidos, useNA = 'ifany') / .N]

prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, ind_embarazos_fallidos, useNA = 'ifany')],
           margin = 1)
prop.table(datos_0m[,
                    table(nom_depart, ind_embarazos_fallidos, useNA = 'ifany')],
           margin = 1)

# meses_embarazo_fallido: meses en que perdión el bebé
table(datos$meses_embarazo_fallido, useNA = 'ifany')

# tpo_amamanta_meses
datos_1m[edad_muerte_meses < 12, sixnum(tpo_amamanta_meses)]
datos_0m[edad_hijo_anios < 1, sixnum(tpo_amamanta_meses)]

# nacimiento_cesarea: 0: No, 1: Sí, NA
datos_1m[edad_muerte_meses < 12,
         table(nacimiento_cesarea, useNA = 'ifany')]
datos_1m[edad_muerte_meses < 12,
         table(nacimiento_cesarea, useNA = 'ifany') / .N]
datos_0m[edad_hijo_anios < 1,
         table(nacimiento_cesarea, useNA = 'ifany')]
datos_0m[edad_hijo_anios < 1,
         table(nacimiento_cesarea, useNA = 'ifany') / .N]

prop.table(datos_1m[edad_muerte_meses < 12,
                    table(nom_depart, nacimiento_cesarea, useNA = 'ifany')],
           margin = 1)
prop.table(datos_0m[, table(nom_depart, nacimiento_cesarea, useNA = 'ifany')],
           margin = 1)

# peso_al_nacer_kg: solo de los niños nacidos entre hace 3 y 5 años
datos_1m[edad_muerte_meses < 12, sixnum(peso_al_nacer_kg), by = nom_depart]
datos_0m[edad_hijo_anios < 1, sixnum(peso_al_nacer_kg), by = nom_depart]

pdf('../img/box-plot_peso_al_nacer.pdf', width = 7, height = 5)
ggplot(data = datos_orig) + theme_bw(12) +
  geom_boxplot(aes(x = nom_depart, y = peso_al_nacer_kg,
                   linetype = factor(hijo_vivo_muerto)),
               outlier.size = 1, position = position_dodge(width = 0.9)) +
  labs(x = 'Departamento', y = 'Peso al nacer [kg]') +
  scale_linetype_manual(breaks = c(0, 1),
                        labels = c('Fallecidos', 'Sobrevivieron'),
                        values = c('longdash', 'solid')) +
  theme(
    legend.title = element_blank()
  )
dev.off()

# resumen número de madres por número de hijos (2157 madres entrevistadas)
hijos_x_nmadres <- 
  datos_orig[, .N, by = id_madre][, .(nmadres = .N), by = N][order(N)]

ggplot(data = hijos_x_nmadres) + theme_bw() +
  geom_point(aes(x = nmadres, y = N)) +
  geom_segment(aes(x = 1, xend = nmadres, y = N, yend = N),
               size = 0.5, colour = 'gray') +
  scale_y_reverse()

# resumen edad primera relación sexual
datos_orig[, nro_hijos := .N, by = id_madre]
datos_orig[,
           .(edad_min = min(edad_primera_rel_sexual, na.rm = TRUE),
             edad_prom = mean(edad_primera_rel_sexual, na.rm = TRUE),
             edad_max = max(edad_primera_rel_sexual, na.rm = TRUE),
             nro_madres = length(unique(id_madre))),
           by = .(nro_hijos)][order(nro_hijos)]

# la misma tabla anterior pero solo para las madres que han perdido hijos
datos_orig[edad_muerte_meses < 12,
           .(edad_min = min(edad_primera_rel_sexual, na.rm = TRUE),
             edad_prom = mean(edad_primera_rel_sexual, na.rm = TRUE),
             edad_max = max(edad_primera_rel_sexual, na.rm = TRUE),
             nro_madres = length(unique(id_madre))),
           by = .(nro_hijos)][order(nro_hijos)]

# proporción de mujeres que tuvieron su primer hijo a los 15 años o menos
datos[edad_madre_1hijo < 15, .N, by = id_madre][, .N] / nro_madres
# proporción de mujeres que tuvieron su primera relación sexual <= 15 años
datos[edad_primera_rel_sexual < 15, .N, by = id_madre][, .N] / nro_madres
# proporción de mujeres que tuvieron su primera relación sexual < 15 años y
# que tuvieron su primer hijo < 15 años
datos_orig[edad_primera_rel_sexual < 15 & edad_madre_1hijo < 15,
           .N,
           by = id_madre][, .N] /
  datos_orig[edad_primera_rel_sexual < 15, .N, by = id_madre][, .N]

## Gráficos bivariados ---------------------------------------------------------

gg_base <- ggplot(data = datos_orig) + theme_bw(12) # gráfico base

gg_base +
  geom_jitter(aes(x = edad_primera_rel_sexual, y = total_hijos_muertos))

gg_base +
  geom_jitter(aes(x = hijo_deseado, y = padre_vivo))

gg_base +
  geom_histogram(aes(x = anio_nac_hijo), colour = 'black', fill = 'white')

gg_base +
  geom_histogram(aes(x = mes_nac_hijo))

# esta
g1 <- ggplot(data = datos_1m) +
  aes(x = hijo_nro, y = hijo_vivo_muerto, linetype = nom_depart) +
  geom_point(alpha = 0.2) +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE) +
  theme_bw() +
  labs(title = 'Curva de probabilidad para número de hijo',
       x = 'orden nacimiento del hijo',
       y = 'Pr(sobrevivir)')

ggplot(data = datos_1m) +
  aes(x = edad_madre_1hijo, y = hijo_vivo_muerto, linetype = nom_depart) +
  geom_point(alpha = 0.2) +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE) +
  theme_bw() +
  labs(title = 'Curva de probabilidad para número de hijo',
       x = 'edad madre primer hijo',
       y = 'Pr(sobrevivir)')

ggplot(data = datos_1m) +
  aes(x = edad_primera_rel_sexual, y = hijo_vivo_muerto,
      linetype = nom_depart) +
  geom_point(alpha = 0.2) +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE) +
  theme_bw() +
  labs(title = 'Curva de probabilidad para número de hijo',
       x = 'edad primera relación sexual',
       y = 'Pr(sobrevivir)')

# esta
g1 <- ggplot(data = datos_orig) +
  aes(x = peso_al_nacer_kg, y = hijo_vivo_muerto) +
  geom_point(alpha = 0.1) +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE) +
  theme_bw() +
  labs(title = 'Curva de probabilidad para peso al nacer',
       x = 'peso al nacer [kg]',
       y = 'Pr(sobrevivir)')

ggplot(data = datos_orig[nom_depart == 'La Guajira']) +
  aes(x = peso_al_nacer_kg, y = hijo_vivo_muerto) +
  geom_point(alpha = 0.1) +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE) +
  theme_bw() +
  labs(title = 'Curva de probabilidad para peso al nacer',
       x = 'peso al nacer [kg]',
       y = 'Pr(sobrevivir)')

ggplot(data = datos_1m) +
  aes(x = peso_al_nacer_kg, y = hijo_vivo_muerto, linetype = nom_depart) +
  geom_point(alpha = 0.1) +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE) +
  theme_bw() +
  labs(title = 'Curva de probabilidad para peso al nacer',
       x = 'peso al nacer [kg]',
       y = 'Pr(sobrevivir)')

pdf(file = '../img/graf_prob_hijo_muerto.pdf',
    width = 7, height = 4)

grid.arrange(g1, g2, ncol = 2, nrow = 1)

dev.off()
