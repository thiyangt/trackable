library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)

# Approximate location of the Pinnawala Elephant Orphanage
orphanage_lng <- 80.3870
orphanage_lat <- 7.3005

ui <- fluidPage(
  titlePanel("Draw an Elephant Tracking Path: Pinnawala Elephant Orphanage"),
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
      addTiles(group = "Street map") |>
      addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      setView(lng = orphanage_lng, lat = orphanage_lat, zoom = 15) |>
      addMarkers(
        lng = orphanage_lng,
        lat = orphanage_lat,
        popup = "Pinnawala Elephant Orphanage"
      ) |>
      addLayersControl(
        baseGroups = c("Street map", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
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
    filename = function() "pinnawala_elephant_tracking_coordinates.csv",
    content = function(file) {
      write.csv(gps_points(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)