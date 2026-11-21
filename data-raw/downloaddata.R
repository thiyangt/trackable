library(shiny)
library(leaflet)
library(leaflet.extras)
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
  
  lines <- reactiveValues(data = list())   # keyed by leaflet id
  
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 80.90, lat = 8.16, zoom = 12) |>
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
  
  get_coords <- function(feature) {
    if (!identical(feature$geometry$type, "LineString")) return(NULL)
    coords <- feature$geometry$coordinates
    data.frame(
      longitude = vapply(coords, function(x) as.numeric(x[[1]]), numeric(1)),
      latitude  = vapply(coords, function(x) as.numeric(x[[2]]), numeric(1))
    )
  }
  
  feature_id <- function(feature) as.character(feature$properties$`_leaflet_id`)
  
  observeEvent(input$map_draw_new_feature, {
    f <- input$map_draw_new_feature
    df <- get_coords(f)
    if (!is.null(df)) lines$data[[feature_id(f)]] <- df
  })
  
  observeEvent(input$map_draw_edited_features, {
    for (f in input$map_draw_edited_features$features) {
      df <- get_coords(f)
      if (!is.null(df)) lines$data[[feature_id(f)]] <- df
    }
  })
  
  observeEvent(input$map_draw_deleted_features, {
    for (f in input$map_draw_deleted_features$features) {
      lines$data[[feature_id(f)]] <- NULL
    }
  })
  
  gps_points <- reactive({
    if (length(lines$data) == 0) {
      return(data.frame(point_id = integer(), line_id = integer(),
                        longitude = numeric(), latitude = numeric()))
    }
    bind_rows(lines$data, .id = "leaflet_id") |>
      mutate(line_id = as.integer(factor(leaflet_id, levels = unique(leaflet_id))),
             point_id = row_number()) |>
      select(point_id, line_id, longitude, latitude)
  })
  
  output$coordinates <- renderTable(gps_points(), digits = 6)
  
  output$download <- downloadHandler(
    filename = function() "elephant_tracking_coordinates.csv",
    content = function(file) {
      write.csv(gps_points(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)