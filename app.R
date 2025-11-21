
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(purrr)
library(stringr)

carrega_incendis <- function(directory) {
  
  zips <- list.files(directory, pattern = "^incendis\\d{2}\\.zip$", full.names = TRUE)
  
  dades <- map_df(zips, function(zipfile) {
    
    # Any: extreu “86” → 1986
    any2 <- str_extract(basename(zipfile), "\\d{2}")
    any <- ifelse(as.numeric(any2) >= 86, 1900 + as.numeric(any2), 2000 + as.numeric(any2))
    
    # Directori temporal exclusiu per aquest ZIP
    tmpdir <- tempfile()
    dir.create(tmpdir)
    
    # Extreure contingut aquí
    unzip(zipfile, exdir = tmpdir)
    
    # Buscar el .shp correcte (només n’hi haurà un)
    shp <- list.files(tmpdir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shp) == 0) return(NULL)
    
    sfobj <- st_read(shp, quiet = TRUE)
    sfobj <- st_transform(sfobj, 4326)
    sfobj$any <- any
    sfobj
  })
  
  return(dades)
}
incendis_sf <- carrega_incendis("Dades/Perimetres_Incendis")

ui <- fluidPage(
  titlePanel("Incendis forestals Catalunya"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "any",
        label = "Any:",
        choices = 1986:2024,
        selected = 1999
      )
    ),
    mainPanel(
      leafletOutput("mapa", height = 600)
    )
  )
)

server <- function(input, output, session) {
  
  # Carrega dades fora de render* (només un cop)
  incendis <- incendis_sf
  
  output$mapa <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  
  observe({
    
    filt <- incendis %>% filter(any == input$any)
    
    leafletProxy("mapa") %>%
      clearShapes() %>%
      addPolygons(data = filt,
                  color = "red",
                  weight = 2,
                  fillOpacity = 0.3,
                  popup = ~paste0(
                    "<b>Any </b>: ", any, "<br>")
                  )
  })
}

shinyApp(ui, server)

