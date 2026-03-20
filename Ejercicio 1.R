# ---------------------------------------------------------
# 2. CARGAR PAQUETES
# ---------------------------------------------------------
# Esto sí se hace cada vez que abras el proyecto.
# Sirve para activar las librerías que vamos a usar.

library(sf)               # Para trabajar con datos espaciales
library(dplyr)            # Para manipulación de tablas
library(lubridate)        # Para manejo de fechas
library(leaflet)          # Para mapas interactivos
library(leaflet.extras2)  # Complementos para mapas interactivos
library(shiny)            # Para crear aplicaciones interactivas
library(ggplot2)          # Para gráficos
library(gganimate)        # Para animaciones
library(readr)            # Para leer archivos CSV
library(gifski)
library(yyjsonr)
# ---------------------------------------------------------
# 3. CARGAR LA BASE DE DATOS DE SISMOS
# ---------------------------------------------------------
# Se carga el archivo CSV descargado desde USGS.

sismos <- read.csv("G:/Mi unidad/GF0629-SIG_II/TP1_Patrick_Alcazar_C00160/query.csv")

# ---------------------------------------------------------
# 4. REVISAR LOS DATOS
# ---------------------------------------------------------
# head() muestra las primeras filas.
# names() muestra los nombres de las columnas.
# Esto sirve para confirmar que sí cargó bien.

head(sismos)
names(sismos)

# ---------------------------------------------------------
# 5. CONVERTIR LA TABLA DE SISMOS A CAPA ESPACIAL
# ---------------------------------------------------------
# Aquí le decimos a R cuáles columnas son longitud y latitud.
# USGS normalmente usa:
# longitude = longitud
# latitude  = latitud
# crs = 4326 significa coordenadas geográficas WGS84

sismos_sf <- st_as_sf(
  sismos,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# ---------------------------------------------------------
# 6. REVISAR QUE AHORA SEA UN OBJETO ESPACIAL
# ---------------------------------------------------------
# class() indica qué tipo de objeto es.
# plot() dibuja los puntos rápidamente para una revisión básica.

class(sismos_sf)
plot(st_geometry(sismos_sf))

# ---------------------------------------------------------
# 7. CARGAR LA CAPA DE PROVINCIAS
# ---------------------------------------------------------
# Se carga el shapefile de provincias de Costa Rica.
# IMPORTANTE: el shapefile debe tener juntos los archivos
# .shp, .dbf, .shx y demás.

provincias <- st_read("G:/Mi unidad/GF0629-SIG_II/TP1_Patrick_Alcazar_C00160/limiteprovincial_5k/limiteprovincial_5k.shp)

# ---------------------------------------------------------
# 8. VERIFICAR Y AJUSTAR EL SISTEMA DE COORDENADAS
# ---------------------------------------------------------
# Transformamos provincias al mismo sistema de coordenadas
# de los sismos para que ambos coincidan correctamente.

provincias <- st_transform(provincias, 4326)

# ---------------------------------------------------------
# 9. CREAR EL CONTORNO DE COSTA RICA
# ---------------------------------------------------------
# Se unen todas las provincias en un solo polígono
# para obtener el contorno nacional.

costa_rica <- st_union(provincias)

# ---------------------------------------------------------
# 10. FILTRAR LOS SISMOS QUE ESTÁN DENTRO DE COSTA RICA
# ---------------------------------------------------------
# Se conservan únicamente los sismos ubicados dentro
# del territorio nacional.

sismos_cr <- st_filter(sismos_sf, costa_rica)

# ---------------------------------------------------------
# 11. VISUALIZAR RÁPIDAMENTE EL RESULTADO DEL FILTRO
# ---------------------------------------------------------
# Esto permite comprobar que ya no haya sismos fuera de CR.

plot(st_geometry(costa_rica), col = "lightgray")
plot(st_geometry(sismos_cr), col = "red", add = TRUE)

# ---------------------------------------------------------
# 12. HACER LA UNIÓN ESPACIAL
# ---------------------------------------------------------
# Ahora sí se asigna a cada sismo su provincia.
# OJO: se usa sismos_cr, no sismos_sf.

sismos_prov <- st_join(sismos_cr, provincias)

# ---------------------------------------------------------
# 13. REVISAR EL RESULTADO DE LA UNIÓN
# ---------------------------------------------------------
# Aquí vemos las primeras filas del nuevo objeto.
# names() deja ver qué columnas nuevas se agregaron.

head(sismos_prov)
names(sismos_prov)

# ---------------------------------------------------------
# 14. CREAR UN MAPA INTERACTIVO BÁSICO
# ---------------------------------------------------------
# Este mapa sirve para comprobar que todo está funcionando.
# Se dibuja el contorno de Costa Rica y los sismos filtrados.

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = costa_rica,
    fill = FALSE,
    color = "black",
    weight = 1
  ) %>%
  addCircleMarkers(
    data = sismos_cr,
    radius = 2,          # Tamaño del punto
    color = "red",       # Color del punto
    stroke = FALSE,      # Sin borde
    fillOpacity = 0.7    # Transparencia
  )

# ---------------------------------------------------------
# 15. PREPARAR LA FECHA DE LOS SISMOS
# ---------------------------------------------------------
# La columna 'time' del USGS viene como texto con fecha y hora.
# Aquí la convertimos a formato fecha-hora (POSIXct),
# que R puede usar para análisis temporal.

sismos_prov$time <- ymd_hms(sismos_prov$time)

# Revisar que la conversión fue exitosa
head(sismos_prov$time)


# ---------------------------------------------------------
# 16. CREAR VARIABLES TEMPORALES
# ---------------------------------------------------------
# Se crean nuevas columnas para facilitar análisis y filtros.

sismos_prov$anio <- year(sismos_prov$time)        # Extrae el año
sismos_prov$mes <- month(sismos_prov$time)        # Extrae el mes
sismos_prov$dia <- day(sismos_prov$time)          # Extrae el día
sismos_prov$fecha <- as.Date(sismos_prov$time)    # Solo fecha (sin hora)

# Revisar que las columnas se crearon correctamente
head(sismos_prov)


# ---------------------------------------------------------
# 17. ORDENAR LOS SISMOS POR TIEMPO
# ---------------------------------------------------------
# Se ordenan los registros desde el más antiguo hasta el más reciente.
# Esto es clave para animaciones acumulativas.

sismos_prov <- sismos_prov %>%
  arrange(time)

# Verificar orden
head(sismos_prov$time)
tail(sismos_prov$time)


# ---------------------------------------------------------
# 18. CREAR UNA TABLA SIN GEOMETRÍA (OPCIONAL)
# ---------------------------------------------------------
# Esto sirve para revisar los datos como tabla normal
# sin la parte espacial.

sismos_tabla <- sismos_prov %>%
  st_drop_geometry()

# Revisar tabla
head(sismos_tabla)

head(sismos_prov$time)

# ---------------------------------------------------------
# 19. EXTRAER COORDENADAS DE LOS SISMOS
# ---------------------------------------------------------
# Como los sismos están en formato espacial (sf),
# aquí extraemos sus coordenadas X y Y para poder
# usarlas en gráficos y animaciones.

coordenadas <- st_coordinates(sismos_prov)

# Agregar las coordenadas como nuevas columnas
sismos_prov$x <- coordenadas[, 1]   # Longitud
sismos_prov$y <- coordenadas[, 2]   # Latitud

# Revisar que sí se agregaron
head(sismos_prov)


# ---------------------------------------------------------
# 20. CREAR UN ÍNDICE TEMPORAL PARA LA ANIMACIÓN
# ---------------------------------------------------------
# Esta variable numérica sirve para controlar el orden
# en que van apareciendo los sismos en la animación.

sismos_prov$orden <- 1:nrow(sismos_prov)

# Revisar las primeras filas
head(sismos_prov$orden)


# ---------------------------------------------------------
# 21. REVISAR RANGO TEMPORAL DE LOS DATOS
# ---------------------------------------------------------
# Esto permite confirmar desde qué fecha hasta qué fecha
# llegan los registros que vamos a animar.

min(sismos_prov$time)
max(sismos_prov$time)

# ---------------------------------------------------------
# 22. CREAR ANIMACIÓN TEMPORAL ACUMULATIVA
# ---------------------------------------------------------
# Esta versión mejora la visibilidad de los sismos:
# - puntos más grandes
# - colores más contrastantes
# - borde oscuro para que resalten sobre el fondo

# Definir encuadre manual centrado
xlim <- c(-86.5, -82.5)
ylim <- c(8.0, 11.5)

animacion_sismos <- ggplot() +
  
  # Provincias
  geom_sf(
    data = provincias,
    fill = "gray90",
    color = "black",
    linewidth = 0.3
  ) +
  
  # Sismos
  geom_sf(
    data = sismos_prov,
    aes(size = mag, fill = mag),
    shape = 21,          # círculo con borde y relleno
    color = "black",     # borde negro
    stroke = 0.25,
    alpha = 0.8
  ) +
  
  # Encuadre fijo y centrado
  coord_sf(
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  ) +
  
  # Escala de color más visible
  scale_fill_gradient(
    name = "Magnitud",
    low = "orange",
    high = "darkred"
  ) +
  
  # Escala de tamaño más fuerte
  scale_size_continuous(
    name = "Magnitud",
    range = c(2, 14)
  ) +
  
  # Títulos
  labs(
    title = "Evolución temporal de sismos en Costa Rica",
    subtitle = "Fecha: {frame_time}",
    x = "Longitud",
    y = "Latitud",
    caption = paste(
      "Fuente: USGS y SNIT | Datum: WGS84 (EPSG:4326) |",
      "Periodo:", format(min(sismos_prov$time), "%Y"), "-", format(max(sismos_prov$time), "%Y"), "|",
      "Autor: Patrick Josué Alcázar Ortiz"
    )
  ) +
  
  # Tema visual
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0, color = "gray30"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 12, 12, 12)
  ) +
  
  # Animación temporal
  transition_time(time) +
  
  # Mantener visibles los sismos anteriores
  shadow_mark(
    past = TRUE,
    future = FALSE,
    alpha = 0.9,
    size = 2
  ) +
  
  # Mantener el encuadre fijo
  view_static()
# ---------------------------------------------------------
# 23. EXPORTAR LA ANIMACIÓN COMO GIF
# ---------------------------------------------------------
# Se exporta con resolución más alta para mejorar lectura.

animate(
  animacion_sismos,
  nframes = 300,
  fps = 10,
  width = 1400,
  height = 850,
  res = 150,
  renderer = gifski_renderer("sismos_cr_animacion.gif")
)
# ---------------------------------------------------------
# 24. CREAR TEXTO EMERGENTE (POPUP) PARA CADA SISMO
# ---------------------------------------------------------
# Esta columna guarda el texto que aparecerá al hacer clic
# sobre cada punto en el mapa.
# La columna correcta del nombre de provincia es PROVINCIA.

sismos_prov$popup <- paste0(
  "<b>Fecha:</b> ", sismos_prov$fecha, "<br>",
  "<b>Hora:</b> ", format(sismos_prov$time, "%H:%M:%S"), "<br>",
  "<b>Magnitud:</b> ", sismos_prov$mag, "<br>",
  "<b>Provincia:</b> ", sismos_prov$PROVINCIA
)

# Revisar que sí se creó correctamente
head(sismos_prov$popup)


# ---------------------------------------------------------
# 25. CREAR COLUMNA DE TIEMPO PARA EL SLIDER
# ---------------------------------------------------------
# La función addTimeslider() necesita que exista una
# columna llamada 'times' dentro de la capa de sismos.

sismos_prov$times <- sismos_prov$time

# Revisar que la columna se creó correctamente
head(sismos_prov$times)
class(sismos_prov$times)


# ---------------------------------------------------------
# 26. CREAR MAPA BASE INTERACTIVO
# ---------------------------------------------------------
# Se crea un mapa con Leaflet que servirá como base
# para agregar el control temporal.

mapa_tiempo <- leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = provincias,
    fill = FALSE,
    color = "black",
    weight = 1
  ) %>%
  addCircleMarkers(
    data = sismos_prov,
    radius = 3,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~popup
  )

# Mostrar el mapa base
mapa_tiempo


# ---------------------------------------------------------
# 27. AGREGAR SLIDER TEMPORAL AL MAPA
# ---------------------------------------------------------
# Se agrega el control temporal al mapa.

mapa_tiempo <- mapa_tiempo %>%
  addTimeslider(
    data = sismos_prov,
    options = timesliderOptions(
      position = "bottomright"
    )
  )

# Mostrar mapa final con slider
mapa_tiempo


# ---------------------------------------------------------
# 28. REVISAR LOS NOMBRES ÚNICOS DE PROVINCIA
# ---------------------------------------------------------
# Esto permite ver exactamente cómo están escritos los
# nombres de las provincias dentro de la tabla.

unique(sismos_prov$PROVINCIA)


# ---------------------------------------------------------
# 29. FILTRAR SISMOS DE UNA PROVINCIA ESPECÍFICA
# ---------------------------------------------------------
# Aquí se seleccionan únicamente los sismos que pertenecen
# a una provincia dada. En este ejemplo: San José.

sismos_sj <- sismos_prov %>%
  filter(PROVINCIA == "San José")

# Revisar cuántos registros quedaron
nrow(sismos_sj)

# Ver algunas filas
head(st_drop_geometry(sismos_sj))


# ---------------------------------------------------------
# 30. CREAR MAPA FILTRADO POR PROVINCIA
# ---------------------------------------------------------
# Este mapa muestra únicamente los sismos de la provincia
# seleccionada en el filtro manual.

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = provincias,
    fill = FALSE,
    color = "black",
    weight = 1
  ) %>%
  addCircleMarkers(
    data = sismos_sj,
    radius = 3,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~popup
  )

# ---------------------------------------------------------
# 31. REVISAR RANGO DE MAGNITUD
# ---------------------------------------------------------
# Esto permite ver valores mínimos y máximos de magnitud.

summary(sismos_prov$mag)

# ---------------------------------------------------------
# 32. FILTRAR SISMOS POR MAGNITUD
# ---------------------------------------------------------
# Aquí se seleccionan únicamente los sismos con magnitud
# mayor o igual a 5.

sismos_mag <- sismos_prov %>%
  filter(mag >= 5)

# Ver cuántos quedaron
nrow(sismos_mag)

# Revisar datos
head(st_drop_geometry(sismos_mag))


# ---------------------------------------------------------
# 33. MAPA FILTRADO POR MAGNITUD
# ---------------------------------------------------------
# Este mapa muestra únicamente los sismos con magnitud >= 5.

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = provincias,
    fill = FALSE,
    color = "black",
    weight = 1
  ) %>%
  addCircleMarkers(
    data = sismos_mag,
    radius = ~mag,        # tamaño proporcional a magnitud
    color = "orange",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~popup
  )

# ---------------------------------------------------------
# ---------------------------------------------------------
# 34. COMBINAR FILTRO POR PROVINCIA Y MAGNITUD
# ---------------------------------------------------------
# Aquí se seleccionan únicamente los sismos de una provincia
# específica y además con magnitud mayor o igual a 5.

sismos_combo <- sismos_prov %>%
  filter(PROVINCIA == "San José", mag >= 5)

# Revisar cuántos registros quedaron
nrow(sismos_combo)

#Ver algunas filas
head(st_drop_geometry(sismos_combo))


# ---------------------------------------------------------
# 35. CREAR MAPA FILTRADO POR PROVINCIA Y MAGNITUD
# ---------------------------------------------------------
# Este mapa muestra únicamente los sismos que cumplen
# simultáneamente con la provincia seleccionada y la
# magnitud mínima definida.

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = provincias,
    fill = FALSE,
    color = "black",
    weight = 1
  ) %>%
  addCircleMarkers(
    data = sismos_combo,
    radius = ~mag,
    color = "purple",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~popup
  )