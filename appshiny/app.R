library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(readr)
library(htmltools)

# 1. LLEGIR DADES UN COP (fora del server) -------------------------------------

# Municipis amb meteo (ja ho tenies)
meteo <- read_csv(
  "Dades_Finals/meteo_municipis_2005_2024.csv",
  col_types = cols(
    DATA_LECTURA    = col_date(),
    `Codi municipi` = col_integer(),
    Municipi        = col_character(),
    CODI_ESTACIO    = col_character(),
    NOM_ESTACIO     = col_character(),
    distancia_km    = col_double(),
    t_max           = col_double()
  )
)

formes <- st_read("Dades_Finals/FormesMunicipis.geojson", quiet = TRUE) |>
  mutate(
    CODIMUNI_num = as.integer(CODIMUNI)  # 080018 -> 80018
  )

# Perímetres d'incendis (fitxer que m’has passat)
incendis <- st_read("Dades_Finals/incendis_complet.geojson", quiet = TRUE)
# incendis té: DATA_INCEN (dd/mm/aaaa), MUNICIPI, ANY, HECTAREES

# Metadades d’estacions XEMA
library(readr)

meta_estacions <- read_csv(
  "Dades_Finals/Metadades_estacions_meteorològiques_automàtiques_20251130.csv",
  locale = locale(decimal_mark = ","),   # <- AIXÒ ÉS LA CLAU
  col_types = cols(
    CODI_ESTACIO = col_character(),
    NOM_ESTACIO  = col_character(),
    LATITUD      = col_double(),
    LONGITUD     = col_double(),
    .default     = col_guess()
  )
)


# Dades diàries per estació
meteo_estacions <- read_csv(
  "Dades_Finals/Meteo_Estacions.csv",
  col_types = cols(
    CODI_ESTACIO         = col_character(),
    NOM_ESTACIO          = col_character(),
    DATA_LECTURA         = col_date(),
    dir_ratxa_max_vent10 = col_double(),
    hr_mitjana           = col_double(),
    prec_diaria          = col_double(),
    ratxa_max_vent10     = col_double(),
    t_mitjana            = col_double(),
    t_max                = col_double(),
    t_min                = col_double()
  )
)

# Estacions com a capa sf (punts)
estacions_sf <- meta_estacions |>
  st_as_sf(coords = c("LONGITUD", "LATITUD"), crs = 4326)

# (si formes/incendis no fossin 4326 es podrien transformar aquí)
# formes   <- st_transform(formes, 4326)
incendis <- st_transform(incendis, 4326)

# Rang de dates per al dateInput
data_min <- min(meteo$DATA_LECTURA, na.rm = TRUE)
data_max <- max(meteo$DATA_LECTURA, na.rm = TRUE)

# 2. UI ------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Temperatura màxima diària per municipi"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput(
        inputId = "dia",
        label   = "Dia:",
        value   = data_min,
        min     = data_min,
        max     = data_max
      ),
      checkboxInput(
        inputId = "mostra_incendis",
        label   = "Mostrar perímetres d'incendis de l'any seleccionat",
        value   = FALSE
      ),
      checkboxInput(
        inputId = "mostra_estacions",
        label   = "Mostrar estacions meteorològiques (dades del dia seleccionat)",
        value   = FALSE
      )
    ),
    mainPanel(
      leafletOutput("mapa", height = 600)
    )
  )
)

# 3. SERVER --------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Paleta per a la t_max dels municipis
  pal_muni <- colorNumeric(
    palette  = "YlOrRd",
    domain   = meteo$t_max,
    na.color = "#CCCCCC"
  )
  
  # Dades meteo per municipi del dia seleccionat
  dades_dia <- reactive({
    req(input$dia)
    
    meteo |>
      filter(DATA_LECTURA == input$dia) |>
      select(
        `Codi municipi`,
        Municipi,
        t_max,
        CODI_ESTACIO,
        NOM_ESTACIO,
        distancia_km
      )
  })
  
  # Polígons de municipis + meteo
  poligons_dia <- reactive({
    formes |>
      left_join(dades_dia(), by = c("CODIMUNI_num" = "Codi municipi"))
  })
  
  # Incendis de l'any del dia seleccionat
  incendis_any <- reactive({
    req(input$dia)
    any_sel <- as.integer(format(input$dia, "%Y"))
    
    incendis |>
      filter(ANY == any_sel)
  })
  
  # Estacions amb dades del dia seleccionat
  estacions_dia <- reactive({
    req(input$dia)
    
    dades_est <- meteo_estacions |>
      filter(DATA_LECTURA == input$dia) |>
      select(
        CODI_ESTACIO,
        NOM_ESTACIO,
        t_max,
        t_min,
        t_mitjana,
        prec_diaria
      )
    
    estacions_sf |>
      left_join(dades_est, by = c("CODI_ESTACIO", "NOM_ESTACIO"))
  })
  
  # Mapa inicial
  output$mapa <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 1.8, lat = 41.8, zoom = 8)
  })
  
  # Actualitzar capes quan canviï data o checkboxes
  observe({
    capa_muni <- poligons_dia()
    capa_inc  <- incendis_any()
    capa_est  <- estacions_dia()
    
    proxy <- leafletProxy("mapa")
    
    # --- Municipis (capa base) ---
    proxy <- proxy |>
      clearGroup("municipis") |>
      addPolygons(
        data        = capa_muni,
        group       = "municipis",
        fillColor   = ~pal_muni(t_max),
        weight      = 1,
        color       = "#444444",
        fillOpacity = 0.8,
        label = ~{
          txt <- sprintf(
            paste0(
              "<strong>%s</strong><br/>",
              "Data: %s<br/>",
              "Tmax municipi: %s<br/>",
              "Estació usada: %s (%s)<br/>",
              "Distància: %s km"
            ),
            Municipi,
            format(input$dia, "%Y-%m-%d"),
            ifelse(is.na(t_max), "sense dada", sprintf("%.1f °C", t_max)),
            ifelse(is.na(NOM_ESTACIO), "-", NOM_ESTACIO),
            ifelse(is.na(CODI_ESTACIO), "-", CODI_ESTACIO),
            ifelse(is.na(distancia_km), "-", sprintf("%.1f", distancia_km))
          )
          lapply(txt, HTML)
        },
        highlight = highlightOptions(
          weight = 2,
          bringToFront = TRUE
        )
      )
    
    # --- Incendis (per any) ---
    proxy <- proxy |>
      clearGroup("incendis")
    
    if (isTRUE(input$mostra_incendis) && nrow(capa_inc) > 0) {
      proxy <- proxy |>
        addPolygons(
          data        = capa_inc,
          group       = "incendis",
          fillColor   = NA,
          weight      = 2,
          color       = "blue",
          fillOpacity = 0,
          label = ~{
            txt <- sprintf(
              paste0(
                "<strong>Incendi</strong><br/>",
                "Data: %s<br/>",
                "Municipi: %s<br/>",
                "Hectàrees: %.1f"
              ),
              DATA_INCEN,
              MUNICIPI,
              HECTAREES
            )
            lapply(txt, HTML)
          }
        )
    }
    
    # --- Estacions meteorològiques ---
    proxy <- proxy |>
      clearGroup("estacions")
    
    if (isTRUE(input$mostra_estacions) && nrow(capa_est) > 0) {
      proxy <- proxy |>
        addCircleMarkers(
          data        = capa_est,
          group       = "estacions",
          radius      = 5,
          stroke      = TRUE,
          weight      = 1,
          fillOpacity = 0.9,
          popup = ~{
            txt <- sprintf(
              paste0(
                "<strong>%s</strong><br/>",
                "Codi: %s<br/>",
                "Tmax estació: %s<br/>",
                "Tmin: %s<br/>",
                "Tmitjana: %s<br/>",
                "Prec. diària: %s mm"
              ),
              NOM_ESTACIO,
              CODI_ESTACIO,
              ifelse(is.na(t_max), "sense dada", sprintf("%.1f °C", t_max)),
              ifelse(is.na(t_min), "sense dada", sprintf("%.1f °C", t_min)),
              ifelse(is.na(t_mitjana), "sense dada", sprintf("%.1f °C", t_mitjana)),
              ifelse(is.na(prec_diaria), "sense dada", sprintf("%.1f", prec_diaria))
            )
            lapply(txt, HTML)
          }
        )
    }
    
    # Llegenda (municipis)
    proxy |>
      clearControls() |>
      addLegend(
        position = "bottomright",
        pal      = pal_muni,
        values   = meteo$t_max,
        title    = "Tmax municipi (°C)"
      )
  })
}

# 4. CÓRRER L'APP --------------------------------------------------------------
shinyApp(ui = ui, server = server)
