# ------------------------------------------------------------------------------
#
# Artículo sobre mortalidad infantil en Antioquia
# Juan Pablo Calle Quintero
# jpcalleq@unal.edu.co
#
# ------------------------------------------------------------------------------

## Este guion contiene el código del análisis exploratorio ---------------------

source(file = '1_cargar_librerias.R')
source(file = '2_def_funciones_aux.R')

## Lectura de datos ------------------------------------------------------------

datos <- read.csv(file = '../datos/datos_ant.csv', header = TRUE)
datos <- data.table(datos) # para poder utilizar las funciones de data.table
str(datos)

# Listas de variables con código original de la base de datos
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
    departamento = 'v023', # no es de interés, solo estamos trabajando con Ant
    region = 'v024', # no es de interés, solo Ant, Central
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

# primero algunos datos para tener presente
nro_obs <- datos[, .N] # número de observaciones
no_vive <- datos[hijo_vivo_muerto == 0, .N] # número de hijos fallecidos
tasa_mort <-
  datos[, .(tasa_mort = 1 - sum(hijo_vivo_muerto) / .N), by = .(anio_nac_hijo)]

table(datos$anio_nac_hijo[datos$edad_muerte_meses <= 12],
      datos$edad_muerte_meses[datos$edad_muerte_meses <= 12])

# cuantos menores de un año murieron por año
muertes <- table(datos$anio_nac_hijo[datos$edad_muerte_meses <= 12],
                 datos$edad_muerte_meses[datos$edad_muerte_meses <= 12])
# cuantos niños nacieron por año
nacimientos <- table(datos$anio_nac_hijo, datos$mes_nac_hijo)

1000 * (apply(muertes, 1, sum)[as.character(1995:2008)] / 
          apply(nacimientos, 1, sum)[as.character(1995:2008)])

# hijo_deseado 1: Sí, 2: Sí pero después, 3: No
table(datos$hijo_deseado, useNA = 'ifany')
# padre_vivo: 0: No, 1: Sí, 8: No sabe (solo aplica para los niños < de 17 años)
table(datos$padre_vivo, useNA = 'ifany')
# edad_hijo_anios: edad, NA (los que ya están muertos)
table(datos$edad_hijo_anios, useNA = 'ifany')
# edad_muerte_meses: edad, NA (los que están vivos)
table(datos$edad_muerte_meses, useNA = 'ifany')
# mes_nac_hijo: 1-12 mes del año
table(datos$mes_nac_hijo, useNA = 'ifany')
# anio_nac_hijo: 1960-2010
table(datos$anio_nac_hijo, useNA = 'ifany')
# hijo_vivo_muerto: 0: muerto, 1: vivo
table(datos$hijo_vivo_muerto, useNA = 'ifany')
# estado_civil_madre:
# 0: solt, 1: casada, 2: unión libre, 3: viuda, 4: divorciada, 5: no viven junt
table(datos$estado_civil_madre, useNA = 'ifany')
# total_hijos_muertos: número de hijos fallecidos.
table(datos$total_hijos_muertos, useNA = 'ifany')
# edad_madre_1hijo: edad en años (entre 10 y 49)
table(datos$edad_madre_1hijo, useNA = 'ifany')
# edad_madre: edad en años (entre 13 y 49)
table(datos$edad_madre, useNA = 'ifany')
# edad_primera_rel_sexual: edad en años, NA
table(datos$edad_primera_rel_sexual, useNA = 'ifany')
# tpo_entre_nac_meses: tiempo entre nacimientos meses, NA para  primer hijo
table(datos$tpo_entre_nac_meses, useNA = 'ifany')
# tipo_lugar_residencia: 1: urbano, 2: rural
table(datos$tipo_lugar_residencia, useNA = 'ifany')
# max_nivel_edu (del hijo): 0: sin edu, 1: Primaria, 2: secundaria, 3: superior
table(datos$max_nivel_edu, useNA = 'ifany')
# fuente_agua:
# 11: acueducto emp urbana, 12: acueducto sist rural, 13: grifo público
# 21: fuente pública con bomba, 22: fuente pública sin bomba, 42: rio,
# 51: lluvia, 71: embotellada, 96: otra.
table(datos$fuente_agua, useNA = 'ifany')
# tipo_banio:
# 11: inodoro con alcant, 12: inodoro poso séptico, 13: inodoro al patio,
# 21: letrina, 22: letrina al rio, 31: no tiene baño
table(datos$tipo_banio, useNA = 'ifany')
# energía: 0: No, 1: Sí
table(datos$energia, useNA = 'ifany')
# nro_personas_hogar: número entreo
table(datos$nro_personas_hogar, useNA = 'ifany')
# hijo_nro: posición del hijo dentro de la familia
table(datos$hijo_nro, useNA = 'ifany')
# sexo_cabeza_fam: 1: hombre, 2: mujer
table(datos$sexo_cabeza_fam, useNA = 'ifany')
# edad_cabeza_familia: número
table(datos$edad_cabeza_familia, useNA = 'ifany')
# sexo_hijo: 1: hombre, 2: mujer
table(datos$sexo_hijo, useNA = 'ifany')
# edad_muerte_meses: edad fallecimiento, NA (todavía viven)
table(datos$edad_muerte_meses, useNA = 'ifany')
# hijo_vivo_muerto: si el hijo vive o ya falleció. 0: muerto, 1: vivo
table(datos$hijo_vivo_muerto, useNA = 'ifany')
# ind_embarazos_fallidos: 0: No, 1: Sí
table(datos$ind_embarazos_fallidos, useNA = 'ifany')
# meses_embarazo_fallido: meses en que perdión el bebé
table(datos$meses_embarazo_fallido, useNA = 'ifany')
# tpo_amamanta_meses
table(datos$tpo_amamanta_meses, useNA = 'ifany')
# nacimiento_cesarea: 0: No, 1: Sí, NA
table(datos$nacimiento_cesarea, useNA = 'ifany')
# peso_al_nacer_kg: solo de los niños nacidos entre hace 3 y 5 años
table(datos$peso_al_nacer_kg, useNA = 'ifany')
# edad_actual_hijo_meses: solo de los niños nacidos entre hace 3 y 5 años
table(datos$edad_actual_hijo_meses, useNA = 'ifany')
# peso_actual_hijo_kg: solo de los niños nacidos entre hace 3 y 5 años
table(datos$peso_actual_hijo_kg, useNA = 'ifany')
# talla_actual_hijo_cm: solo de los niños nacidos entre hace 3 y 5 años
table(datos$talla_actual_hijo_cm, useNA = 'ifany')
# perc_talla_edad: solo de los niños nacidos entre hace 3 y 5 años
table(datos$perc_talla_edad, useNA = 'ifany')
# perc_peso_edad: solo de los niños nacidos entre hace 3 y 5 años
table(datos$perc_peso_edad, useNA = 'ifany')
# perc_peso_talla: solo de los niños nacidos entre hace 3 y 5 años
table(datos$perc_peso_talla, useNA = 'ifany')


## Gráficos bivariados ---------------------------------------------------------

gg_base <- ggplot(data = datos) + theme_bw(10) # gráfico base

gg_base +
  geom_point(aes(x = edad_primera_rel_sexual, y = total_hijos_muertos))

gg_base +
  geom_jitter(aes(x = edad_primera_rel_sexual, y = total_hijos_muertos))

gg_base +
  geom_jitter(aes(x = hijo_deseado, y = padre_vivo))

gg_base +
  geom_histogram(aes(x = anio_nac_hijo), colour = 'black', fill = 'white')

gg_base +
  geom_histogram(aes(x = mes_nac_hijo))
