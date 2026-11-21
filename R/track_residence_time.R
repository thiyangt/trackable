#' Calculate Animal Residence Time
#'
#' Estimate the time animals spend within specified geographical
#' areas using consecutive tracking observations.
#'
#' @param x A `trackable` object.
#' @param area An `sf` object containing polygons defining
#'   the geographical areas of interest.
#' @param max_gap Optional maximum time interval, in seconds,
#'   between consecutive observations. Larger intervals
#'   are excluded.
#' @param ... Additional arguments passed to spatial operations.
#'
#' @return A tibble containing estimated residence time
#'   for each animal and geographical area, in seconds.
#'
#' @details
#' Residence time is estimated by assigning the elapsed time
#' between consecutive observations to the polygon containing
#' the midpoint of the corresponding movement segment.
#' Therefore, results depend on the temporal resolution of
#' the tracking data.
#'
#' @seealso [track_crossings()]
#'
#' @examples
#' # residence <- track_residence_time(trk, area = study_areas)
#'
#' @export
track_residence_time <- function(
    x,
    area,
    max_gap = Inf,
    ...) {
  
  meta <- .track_metadata(x)
  
  if (!inherits(area, "sf")) {
    stop("`area` must be an sf object.", call. = FALSE)
  }
  
  if (!is.numeric(max_gap) || length(max_gap) != 1L ||
      is.na(max_gap) || max_gap <= 0) {
    stop("`max_gap` must be a positive number.", call. = FALSE)
  }
  
  if (is.na(sf::st_crs(area)) ||
      is.na(sf::st_crs(x))) {
    stop("Both objects must have a valid CRS.", call. = FALSE)
  }
  
  x <- .track_project(x)
  area <- sf::st_transform(area, sf::st_crs(x))
  
  area$.area_id <- seq_len(nrow(area))
  
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
      
      if (elapsed <= 0 || elapsed > max_gap) next
      
      segment <- sf::st_sfc(
        sf::st_linestring(
          rbind(
            sf::st_coordinates(dat[i, ])[1, 1:2],
            sf::st_coordinates(dat[i + 1L, ])[1, 1:2]
          )
        ),
        crs = sf::st_crs(x)
      )
      
      midpoint <- sf::st_line_interpolate_point(
        segment,
        0.5
      )
      
      hits <- sf::st_intersects(midpoint, area)
      
      if (length(hits[[1]]) == 0L) next
      
      for (j in hits[[1]]) {
        
        results[[k]] <- tibble::tibble(
          animal = as.character(
            dat[[meta$key[1]]][1]
          ),
          area_id = area$.area_id[j],
          residence_time = elapsed
        )
        
        k <- k + 1L
      }
    }
  }
  
  if (length(results) == 0L) {
    return(tibble::tibble(
      animal = character(),
      area_id = integer(),
      residence_time = double()
    ))
  }
  
  dplyr::bind_rows(results) |>
    dplyr::group_by(animal, area_id) |>
    dplyr::summarise(
      residence_time = sum(residence_time),
      .groups = "drop"
    )
}
