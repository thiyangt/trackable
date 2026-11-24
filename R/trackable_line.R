
#' Convert trackable points into trajectories
#'
#' @param x A trackable object created using [trackable()].
#'
#' @return An sf object containing LINESTRING geometries.
#' @export
trackable_lines <- function(x) {
  
  # Check input
  if (!inherits(x, "trackable")) {
    stop("`x` must be a trackable object.", call. = FALSE)
  }
  
  # Retrieve metadata
  key_names <- attr(x, "trackable_key")
  time_name <- attr(x, "trackable_time")
  coord_names <- attr(x, "trackable_coords")
  
  # Extract attributes
  dat <- sf::st_drop_geometry(x)
  
  # Split observations by trajectory
  groups <- dplyr::group_split(
    dplyr::group_by(
      dat,
      dplyr::across(dplyr::all_of(key_names))
    )
  )
  
  if (length(groups) == 0L) {
    stop("No observations available.", call. = FALSE)
  }
  
  # Check minimum observations
  n_obs <- vapply(groups, nrow, integer(1))
  
  if (any(n_obs < 2L)) {
    stop(
      "Each trajectory must contain at least two observations.",
      call. = FALSE
    )
  }
  
  # Arrange observations and create LINESTRING geometries
  geometries <- lapply(groups, function(d) {
    
    d <- d[order(d[[time_name]]), , drop = FALSE]
    
    coords <- as.matrix(
      d[, coord_names, drop = FALSE]
    )
    
    sf::st_linestring(coords)
  })
  
  # Create trajectory-level attributes
  attributes <- lapply(groups, function(d) {
    
    d <- d[order(d[[time_name]]), , drop = FALSE]
    
    out <- d[1, key_names, drop = FALSE]
    
    out$start_time <- d[[time_name]][1]
    out$end_time <- d[[time_name]][nrow(d)]
    out$n_obs <- nrow(d)
    
    out
  })
  
  attributes <- dplyr::bind_rows(attributes)
  
  # Create geometry column
  geometry <- sf::st_sfc(
    geometries,
    crs = sf::st_crs(x)
  )
  
  # Create sf object
  out <- sf::st_sf(
    attributes,
    geometry = geometry
  )
  
  # Assign class and metadata
  class(out) <- unique(
    c("trackable_lines", class(out))
  )
  
  attr(out, "trackable_key") <- key_names
  attr(out, "trackable_time") <- time_name
  attr(out, "trackable_coords") <- coord_names
  
  out
}
