# ---------------------------------------------------------
# APP SHINY - VISOR DE SISMOS EN COSTA RICA
# ---------------------------------------------------------

library(shiny)
library(sf)
library(dplyr)
library(lubridate)
library(leaflet)

# ---------------------------------------------------------
# 1. CARGAR DATOS
# ---------------------------------------------------------

sismos <- read.csv(
  "G:/Mi unidad/GF0629-SIG_II/TP1_Patrick_Alcazar_C00160/query.csv"
)

provincias <- st_read(
  "G:/Mi unidad/GF0629-SIG_II/TP1_Patrick_Alcazar_C00160/limiteprovincial_5k/limiteprovincial_5k.shp",
  quiet = TRUE
)

# ---------------------------------------------------------
# 2. PREPARACIÓN ESPACIAL
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
# 3. PREPARAR VARIABLES
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

anio_inicio <- format(fecha_min, "%Y")

# ---------------------------------------------------------
# 4. PALETA DE COLOR
# ---------------------------------------------------------

pal_mag <- colorNumeric(
  palette = c("orange", "red", "darkred"),
  domain = sismos_prov$mag,
  na.color = "gray"
)

# ---------------------------------------------------------
# 5. UI
# ---------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Visor de Sismos en Costa Rica"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # -----------------------------------------
      # CONTROLES SOLO PARA LA PESTAÑA MAPA
      # -----------------------------------------
      
      conditionalPanel(
        condition = "input.tabs == 'Mapa'",
        
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
        
        checkboxInput("grilla", "Mostrar grilla", FALSE),
        
        br(),
        
        tags$div(
          style = "font-size: 12px; color: gray30;",
          HTML(
            paste0(
              "<b>Fuente:</b> USGS y SNIT<br>",
              "<b>Datum / SRC:</b> WGS84 (EPSG:4326)<br>",
              "<b>Autor:</b> Patrick Josué Alcázar Ortiz"
            )
          )
        )
      ),
      
      # -----------------------------------------
      # TEXTO SOLO PARA LA PESTAÑA TIMELAPSE
      # -----------------------------------------
      
      conditionalPanel(
        condition = "input.tabs == 'Timelapse'",
        
        tags$div(
          style = "
            font-size: 13px;
            color: gray30;
            line-height: 1.6;
          ",
          HTML(
            paste0(
              "<b>Timelapse de sismos</b><br>",
              "Visualización temporal acumulativa de la sismicidad en Costa Rica.<br><br>",
              "<b>Fuente:</b> USGS y SNIT<br>",
              "<b>Datum / SRC:</b> WGS84 (EPSG:4326)<br>",
              "<b>Periodo:</b> ", anio_inicio, " - 2026<br>",
              "<b>Autor:</b> Patrick Josué Alcázar Ortiz"
            )
          )
        )
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        id = "tabs",
        
        # -------------------------------------------------
        # PESTAÑA 1: MAPA
        # -------------------------------------------------
        
        tabPanel(
          "Mapa",
          
          br(),
          
          tags$div(
            style = "text-align:right; font-size:20px; font-weight:bold;",
            "N ↑"
          ),
          
          leafletOutput("mapa", height = 700),
          
          br(),
          textOutput("resumen"),
          textOutput("fecha_texto"),
          br(),
          textOutput("coords")
        ),
        
        # -------------------------------------------------
        # PESTAÑA 2: TIMELAPSE
        # -------------------------------------------------
        
        tabPanel(
          "Timelapse",
          
          br(),
          
          tags$div(
            style = "
              width: 100%;
              padding: 25px;
              background-color: #f4f4f4;
              border-radius: 12px;
            ",
            
            tags$h2(
              paste0(
                "Timelapse de sismos a lo largo de la historia de Costa Rica (",
                anio_inicio, " - 2026)"
              ),
              style = "
                text-align: center;
                font-weight: bold;
                margin-bottom: 25px;
                font-size: 30px;
              "
            ),
            
            tags$div(
              style = "display: flex; justify-content: center;",
              
              tags$video(
                src = "sismos_cr_animacion.mp4",
                controls = TRUE,
                autoplay = FALSE,
                style = "
                  width: 100%;
                  max-width: 1400px;
                  height: auto;
                  display: block;
                  border-radius: 10px;
                  background-color: black;
                  box-shadow: 0 4px 12px rgba(0,0,0,0.25);
                "
              )
            )
          )
        )
      )
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
      setView(
        lng = -84.0,
        lat = 9.9,
        zoom = 7
      ) %>%
      addTiles() %>%
      addPolygons(
        data = provincias,
        fill = FALSE,
        color = "black",
        weight = 1
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal_mag,
        values = sismos_prov$mag,
        title = "Magnitud",
        opacity = 1
      )
    
    if (input$grilla) {
      mapa <- mapa %>%
        addGraticule(interval = 1)
    }
    
    mapa
  })
  
  # ---------------------------------------------
  # ACTUALIZAR MAPA
  # ---------------------------------------------
  
  observe({
    
    leafletProxy("mapa") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearControls()
    
    if (input$grilla) {
      leafletProxy("mapa") %>%
        addGraticule(interval = 1)
    }
    
    leafletProxy("mapa") %>%
      addCircleMarkers(
        data = datos_filtrados(),
        radius = ~pmax(4, mag * 1.2),
        color = ~pal_mag(mag),
        fillColor = ~pal_mag(mag),
        stroke = TRUE,
        weight = 0.5,
        opacity = 1,
        fillOpacity = 0.8,
        popup = ~popup,
        clusterOptions = markerClusterOptions(
          disableClusteringAtZoom = 9,
          spiderfyOnMaxZoom = TRUE,
          zoomToBoundsOnClick = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal_mag,
        values = sismos_prov$mag,
        title = "Magnitud",
        opacity = 1
      )
  })
  
  # ---------------------------------------------
  # TEXTOS
  # ---------------------------------------------
  
  output$resumen <- renderText({
    paste("Sismos:", nrow(datos_filtrados()))
  })
  
  output$fecha_texto <- renderText({
    paste("Mostrando sismos hasta:", as.character(input$fecha_fin))
  })
  
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