library(shiny)
library(leaflet)
library(sp)
library(rgdal)
library(raster)

# Maps
loss_map <- readOGR("/home/laurent/Dropbox/FloodFinJava/geodata/central_java_hospitals_losses.gpkg")
flood_map <- raster("/home/laurent/Dropbox/FloodFinJava/geodata/JBA/Sample data/Semarang/ID_FLRF_UD_Q200_RD_031.tif")

color_palette <-  colorNumeric(palette = "GnBu", domain = values(flood_map), na.color=NA)

# UI
ui <- fluidPage(
  titlePanel("A flood loss example"),
  sidebarLayout(position = "left",
    sidebarPanel("The other stuffs"),
    mainPanel("The MAP",
              leafletOutput("mymap")
              )
    )
  )

# Server
server <- function(input, output) {
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = loss_map) %>%
      addRasterImage(flood_map, colors = color_palette, opacity = 0.8) %>%
      addLegend(pal = color_palette, values = values(flood_map), title = "Max. water depth")
      })
}

shinyApp(ui = ui, server = server)
