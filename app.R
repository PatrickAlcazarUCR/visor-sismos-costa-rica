# ---------------------------------------------------------
# APP SHINY - VISOR DE SISMOS EN COSTA RICA
# ---------------------------------------------------------

# ---------------------------------------------------------
# 1. CARGAR PAQUETES
# ---------------------------------------------------------

library(shiny)
library(sf)
library(dplyr)
library(lubridate)
library(leaflet)


# ---------------------------------------------------------
# 2. CARGAR DATOS
# ---------------------------------------------------------

sismos <- read.csv(
  "G:/Mi unidad/GF0629-SIG_II/TP1_Patrick_Alcazar_C00160/query.csv"
)

provincias <- st_read(
  "G:/Mi unidad/GF0629-SIG_II/TP1_Patrick_Alcazar_C00160/limiteprovincial_5k/limiteprovincial_5k.shp",
  quiet = TRUE
)


# ---------------------------------------------------------
# 3. PREPARACIÓN ESPACIAL
# ---------------------------------------------------------

provincias <- st_transform(provincias, 4326)

sismos_sf <- st_as_sf(
  sismos,
  coords = c("longitude", "latitude"),
  crs = 4326
)

costa_rica <- st_union(provincias)

sismos_cr <- st_filter(sismos_sf, costa_rica)

sismos_prov <- st_join(sismos_cr, provincias)


# ---------------------------------------------------------
# 4. PREPARAR VARIABLES
# ---------------------------------------------------------

sismos_prov$time <- ymd_hms(sismos_prov$time)
sismos_prov$fecha <- as.Date(sismos_prov$time)

sismos_prov$popup <- paste0(
  "<b>Fecha:</b> ", sismos_prov$fecha, "<br>",
  "<b>Hora:</b> ", format(sismos_prov$time, "%H:%M:%S"), "<br>",
  "<b>Magnitud:</b> ", sismos_prov$mag, "<br>",
  "<b>Provincia:</b> ", sismos_prov$PROVINCIA
)

lista_provincias <- sort(unique(sismos_prov$PROVINCIA))
lista_provincias <- lista_provincias[!is.na(lista_provincias)]

fecha_min <- min(sismos_prov$fecha, na.rm = TRUE)
fecha_max <- max(sismos_prov$fecha, na.rm = TRUE)


# ---------------------------------------------------------
# 5. UI
# ---------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Visor de Sismos en Costa Rica"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "provincia",
        "Provincia:",
        choices = c("Todas", lista_provincias)
      ),
      
      sliderInput(
        "magnitud",
        "Magnitud mínima:",
        min = floor(min(sismos_prov$mag, na.rm = TRUE)),
        max = ceiling(max(sismos_prov$mag, na.rm = TRUE)),
        value = 5,
        step = 0.1
      ),
      
      sliderInput(
        "fecha_fin",
        "Mostrar hasta:",
        min = fecha_min,
        max = fecha_max,
        value = fecha_max,
        timeFormat = "%Y-%m-%d",
        step = 30
      ),
      
      checkboxInput("grilla", "Mostrar grilla", FALSE)
    ),
    
    mainPanel(
      
      # Norte
      tags$div(
        style = "text-align:right; font-size:20px; font-weight:bold;",
        "N ↑"
      ),
      
      leafletOutput("mapa", height = 700),
      
      br(),
      textOutput("resumen"),
      br(),
      textOutput("coords")
    )
  )
)


# ---------------------------------------------------------
# 6. SERVER
# ---------------------------------------------------------

server <- function(input, output, session) {
  
  
  # ---------------------------------------------
  # FILTROS
  # ---------------------------------------------
  
  datos_filtrados <- reactive({
    
    datos <- sismos_prov
    
    if (input$provincia != "Todas") {
      datos <- datos %>%
        filter(PROVINCIA == input$provincia)
    }
    
    datos <- datos %>%
      filter(mag >= input$magnitud)
    
    datos <- datos %>%
      filter(fecha <= as.Date(input$fecha_fin))
    
    datos
  })
  
  
  # ---------------------------------------------
  # MAPA BASE
  # ---------------------------------------------
  
  output$mapa <- renderLeaflet({
    
    mapa <- leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = provincias,
        fill = FALSE,
        color = "black",
        weight = 1
      )
    
    if (input$grilla) {
      mapa <- mapa %>%
        addGraticule(interval = 1)
    }
    
    mapa
  })
  
  
  # ---------------------------------------------
  # ACTUALIZAR MAPA (CLUSTER)
  # ---------------------------------------------
  
  observe({
    
    leafletProxy("mapa") %>%
      clearMarkers() %>%
      clearMarkerClusters()
    
    # grilla
    if (input$grilla) {
      leafletProxy("mapa") %>%
        addGraticule(interval = 1)
    }
    
    leafletProxy("mapa") %>%
      addCircleMarkers(
        data = datos_filtrados(),
        radius = ~pmax(3, mag),
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~popup,
        
        clusterOptions = markerClusterOptions(
          disableClusteringAtZoom = 9,
          spiderfyOnMaxZoom = TRUE,
          zoomToBoundsOnClick = TRUE
        )
      )
  })
  
  
  # ---------------------------------------------
  # RESUMEN
  # ---------------------------------------------
  
  output$resumen <- renderText({
    paste("Sismos:", nrow(datos_filtrados()))
  })
  
  
  # ---------------------------------------------
  # COORDENADAS
  # ---------------------------------------------
  
  output$coords <- renderText({
    
    if (is.null(input$mapa_click)) {
      return("Haga clic en el mapa")
    }
    
    paste(
      "Lat:", round(input$mapa_click$lat, 5),
      "| Lon:", round(input$mapa_click$lng, 5)
    )
  })
}


# ---------------------------------------------------------
# 7. EJECUTAR APP
# ---------------------------------------------------------

shinyApp(ui = ui, server = server)