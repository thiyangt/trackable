library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)

ui <- fluidPage(
  titlePanel("Draw an Elephant Tracking Path"),
  
  p("Use the polyline tool on the map to draw the movement path."),
  
  leafletOutput("map", height = "600px"),
  
  br(),
  
  h4("Extracted longitude and latitude"),
  
  tableOutput("coordinates"),
  
  downloadButton("download", "Download coordinates as CSV")
)

server <- function(input, output, session) {
  
  gps_points <- reactiveVal(
    data.frame(
      point_id = integer(),
      longitude = numeric(),
      latitude = numeric()
    )
  )
  
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(
        lng = 80.90,
        lat = 8.16,
        zoom = 12
      ) |>
      addDrawToolbar(
        targetGroup = "drawn",
        polylineOptions = drawPolylineOptions(),
        polygonOptions = FALSE,
        rectangleOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions()
      )
  })
  
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    
    if (feature$geometry$type == "LineString") {
      
      coords <- feature$geometry$coordinates
      
      new_points <- data.frame(
        point_id = seq_len(length(coords)),
        longitude = vapply(coords, function(x) x[[1]], numeric(1)),
        latitude = vapply(coords, function(x) x[[2]], numeric(1))
      )
      
      old_points <- gps_points()
      
      new_points$point_id <- seq_len(
        nrow(new_points) + nrow(old_points)
      )[-seq_len(nrow(old_points))]
      
      gps_points(bind_rows(old_points, new_points))
    }
  })
  
  output$coordinates <- renderTable({
    gps_points()
  }, digits = 6)
  
  output$download <- downloadHandler(
    filename = function() {
      "elephant_tracking_coordinates.csv"
    },
    content = function(file) {
      write.csv(gps_points(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
