library(shiny)
library(leaflet)
library(rgdal)
library(raster)
library(htmltools)
library(scales)

# Maps
loss_map <- readOGR("/home/laurent/Dropbox/FloodFinJava/geodata/central_java_hospitals_losses.gpkg")
flood_maps <- list('200'=raster("/home/laurent/Dropbox/FloodFinJava/geodata/JBA/Sample data/Semarang/ID_FLRF_UD_Q200_RD_031.tif"),
                   '1500'=raster("/home/laurent/Dropbox/FloodFinJava/geodata/JBA/Sample data/Semarang/ID_FLRF_UD_Q1500_RD_031.tif"))
return_periods <- names(flood_maps)
color_palette <-  colorNumeric(palette = "YlGnBu", domain = values(flood_maps[['1500']]), na.color=NA)

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

  returnPeriod <- reactive({input$return_period})

  perc_losses <- reactive({paste0("q", returnPeriod(), "_perc_losses")})
  popup_content <- reactive({paste0("<b>", htmlEscape(loss_map$name), "</b>",
                          "<br> Value: $", htmlEscape(loss_map$asset_value),
                          "<br> Losses: ", percent(loss_map[[perc_losses()]]))
                          })

  output$map_area <- renderLeaflet({
    leaflet() %>%
    addTiles() %>%
    addPolygons(data = loss_map, popup = popup_content()) %>%
    addRasterImage(flood_maps[[returnPeriod()]], colors = color_palette, opacity = 0.8) %>%
    addLegend(pal = color_palette, values = values(flood_maps[[returnPeriod()]]), title = "Max. water depth")
    })
}

shinyApp(ui = ui, server = server)
