#' Identify Animal Crossings of Spatial Boundaries
#'
#' Identify locations where animal trajectories intersect
#' specified spatial boundaries.
#'
#' @param x A `trackable` object.
#' @param boundary An `sf` object containing lines or polygons
#'   representing geographical boundaries.
#' @param ... Additional arguments passed to spatial operations.
#'
#' @return An `sf` object containing crossing locations,
#'   animal identifiers and estimated crossing times.
#'
#' @details
#' Consecutive animal locations are connected to form movement
#' segments. Intersections between these segments and the
#' supplied spatial boundaries are identified.
#'
#' Crossing times are estimated by linear interpolation
#' along each movement segment.
#'
#' @seealso [track_residence_time()]
#'
#' @examples
#' # crossings <- track_crossings(trk, boundary = rivers)
#'
#' @export
track_crossings <- function(
    x,
    boundary,
    ...) {
  
  meta <- .track_metadata(x)
  
  if (!inherits(boundary, "sf")) {
    stop("`boundary` must be an sf object.", call. = FALSE)
  }
  
  if (is.na(sf::st_crs(boundary)) ||
      is.na(sf::st_crs(x))) {
    stop("Both objects must have a valid CRS.", call. = FALSE)
  }
  
  x <- .track_project(x)
  boundary <- sf::st_transform(boundary, sf::st_crs(x))
  
  groups <- .track_groups(x, meta$key)
  
  results <- list()
  k <- 1L
  
  for (indices in groups) {
    
    dat <- x[indices, ]
    
    dat <- dat[
      order(dat[[meta$time]]),
    ]
    
    if (nrow(dat) < 2L) next
    
    for (i in seq_len(nrow(dat) - 1L)) {
      
      t1 <- dat[[meta$time]][i]
      t2 <- dat[[meta$time]][i + 1L]
      
      elapsed <- as.numeric(
        difftime(t2, t1, units = "secs")
      )
      
      if (elapsed <= 0) next
      
      coords <- rbind(
        sf::st_coordinates(dat[i, ])[1, 1:2],
        sf::st_coordinates(dat[i + 1L, ])[1, 1:2]
      )
      
      segment <- sf::st_sfc(
        sf::st_linestring(coords),
        crs = sf::st_crs(x)
      )
      
      intersections <- suppressWarnings(
        sf::st_intersection(segment, boundary)
      )
      
      if (length(intersections) == 0L) next
      
      for (j in seq_along(intersections)) {
        
        geom <- intersections[j]
        
        if (length(geom) == 0L ||
            all(sf::st_is_empty(geom))) next
        
        points <- suppressWarnings(
          sf::st_cast(geom, "POINT")
        )
        
        if (length(points) == 0L) next
        
        for (p in seq_along(points)) {
          
          fraction <- as.numeric(
            sf::st_line_locate_point(
              segment,
              points[p]
            )
          )
          
          crossing_time <- t1 + fraction * elapsed
          
          animal_info <- sf::st_drop_geometry(
            dat[i, meta$key, drop = FALSE]
          )
          
          results[[k]] <- sf::st_sf(
            animal_info,
            crossing_time = crossing_time,
            geometry = points[p]
          )
          
          k <- k + 1L
        }
      }
    }
  }
  
  if (length(results) == 0L) {
    return(sf::st_sf(
      animal = character(),
      crossing_time = as.POSIXct(character(), tz = "UTC"),
      geometry = sf::st_sfc(crs = sf::st_crs(x))
    ))
  }
  
  do.call(rbind, results)
}
