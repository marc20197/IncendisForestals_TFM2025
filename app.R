
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(DT)
library(sf)
incendis <- st_read("Dades/Perimetres_Incendis/incendis19/incendis2019.shp")

incendis
# Exemple: llegir un fitxer SHP
temp_dir <- tempdir()
unzip("incendis94.zip", exdir = temp_dir)

# Busca el fitxer .shp dins el zip
temp_dir <- tempdir()
getwd()
list.files()
unzip("incendis94.zip", exdir = temp_dir)

# Busca el fitxer .shp dins el zip
shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)

# Llegeix-lo amb sf
shape <- st_read(shp_file)

# Converteix a coordenades geogràfiques (lat/lon)
shape <- st_transform(shape, crs = 4326)
# shape <- readRDS("shape_simple.rds")
ui <- fluidPage(
  titlePanel("Mapa d'incendis de Catalunya (1994)"),
  
  leafletOutput("mapa", height = 600)
)
server <- function(input, output, session) {
  
  output$mapa <- renderLeaflet({
    leaflet(shape) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        color = "red",
        weight = 1,
        fillOpacity = 0.4
      )
  })
}
shinyApp(ui = ui, server = server)
