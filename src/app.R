library(shiny)
library(leaflet)
library(rgdal)
library(raster)

# Maps
loss_map <- readOGR("/home/laurent/Dropbox/FloodFinJava/geodata/central_java_hospitals_losses.gpkg")
flood_maps <- c("/home/laurent/Dropbox/FloodFinJava/geodata/JBA/Sample data/Semarang/ID_FLRF_UD_Q200_RD_031.tif",
                "/home/laurent/Dropbox/FloodFinJava/geodata/JBA/Sample data/Semarang/ID_FLRF_UD_Q1500_RD_031.tif")
return_periods <- c(200,1500)
names(flood_maps) <- return_periods

color_palette <-  colorNumeric(palette = "YlGnBu", domain = c(0,30), na.color=NA)

# UI
ui <- fluidPage(
  titlePanel("A flood loss example"),
  sidebarLayout(position = "left",
    sidebarPanel("",
                 selectInput(inputId = "return_period", label = "Return period",
                             choices = return_periods)),
    mainPanel("",
              leafletOutput("map_area")
              )
    )
  )

# Server
server <- function(input, output) {
  output$map_area <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = loss_map) %>%
      addRasterImage(raster(flood_maps[input$return_period]), colors = color_palette, opacity = 0.8) %>%
      addLegend(pal = color_palette, values = values(raster(flood_maps[input$return_period])), title = "Max. water depth")
      })
}

shinyApp(ui = ui, server = server)
