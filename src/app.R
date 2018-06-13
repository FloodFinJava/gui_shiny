library(shiny)
library(leaflet)
library(rgdal)
library(raster)
library(htmltools)
library(scales)

# Maps
loss_map <- readOGR("/home/laurent/Dropbox/FloodFinJava/geodata/asset_losses.gpkg")
flood_maps <- list('200'=raster("/home/laurent/Dropbox/FloodFinJava/geodata/JBA/Sample data/Semarang/ID_FLRF_UD_Q200_RD_031.tif"),
                   '1500'=raster("/home/laurent/Dropbox/FloodFinJava/geodata/JBA/Sample data/Semarang/ID_FLRF_UD_Q1500_RD_031.tif"))
return_periods <- names(flood_maps)
color_palette <-  colorNumeric(palette = "Blues", domain = values(flood_maps[['1500']]), na.color=NA)

# UI
ui <- fluidPage(
  titlePanel("A flood loss example"),
  sidebarLayout(position = "left",
    sidebarPanel("",
                 selectInput(inputId = "return_period", label = "Return period",
                             choices = return_periods),
                 checkboxGroupInput("amenity", "Type of assets", selected = c("hospital"),
                                    choices = unique(loss_map$amenity), inline = TRUE),
                 verbatimTextOutput("summary")
                 ),
    mainPanel("",
              leafletOutput("map_area", height = 800)
              )
    )
  )


# Server
server <- function(input, output) {

  # Set the popup content as reactive elements
  perc_losses <- reactive({sprintf("Q%s_perc_losses", input$return_period)})
  popup_content <- reactive({paste0("<b>", htmlEscape(loss_map$name), "</b>",
                      sprintf("<br> Type: %s", htmlEscape(loss_map$amenity)),
                      sprintf("<br> Value (MRp): %s", prettyNum(loss_map$asset_value,
                                                                big.mark=" ", scientific=F)),
                      "<br> Losses: ", percent(loss_map[[perc_losses()]]),
                      sprintf("<br> Value loss (MRp): %s", format(loss_map[[perc_losses()]] * loss_map$asset_value,
                                                                  big.mark=" ", scientific=F))
                      )
                      })

  # get flood map extent
  map_extent <- reactive({flood_maps[[input$return_period]]@extent})

  # A selection of the assets to display (from checkbox)
  assets_select = reactive({loss_map[is.element(loss_map$amenity, input$amenity),]})

  # Display summary
  output$summary <- renderText({
    paste(
      sprintf("Number of flooded assets: %1.0f", sum(assets_select()[[perc_losses()]] != 0)),
      sprintf("Total financial losses (MRp): %s", format(sum(assets_select()[[perc_losses()]] * assets_select()$asset_value),
                                                         big.mark=" ", scientific=F)),
      sep = '\n'
    )
  })

  # Base map
  output$map_area <- renderLeaflet({
    leaflet() %>%
      fitBounds(map_extent()@xmin,
                map_extent()@ymin,
                map_extent()@xmax,
                map_extent()@ymax) %>%
      addTiles()
  })
  # draw / update the asset polygons
  observe({
    leafletProxy("map_area") %>%
      clearShapes() %>%
      addPolygons(data = assets_select(), popup = popup_content(), color = "#4daf4a")
  })
  # draw/update flood map
  observe({
    leafletProxy("map_area") %>%
      clearImages() %>%
      addRasterImage(flood_maps[[input$return_period]],
                     colors = color_palette, opacity = 0.8)
  })
  # draw/update flood map legend
  observe({
    leafletProxy("map_area") %>%
      clearControls() %>%
      addLegend(pal = color_palette,
                values = values(flood_maps[[input$return_period]]),
                title = "Max. water depth")
  })
}

shinyApp(ui = ui, server = server)
