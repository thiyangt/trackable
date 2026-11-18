# Extract tracking metadata
.track_metadata <- function(x) {
  time <- attr(x, "trackable_time")
  key <- attr(x, "trackable_key")
  coords <- attr(x, "trackable_coords")
  
  if (!inherits(x, "trackable")) {
    stop("`x` must be a trackable object.", call. = FALSE)
  }
  
  if (is.null(time) || is.null(key) || is.null(coords)) {
    stop("Tracking metadata are missing.", call. = FALSE)
  }
  
  list(time = time, key = key, coords = coords)
}


# Create a local projected CRS for spatial calculations
.track_project <- function(x) {
  if (is.na(sf::st_crs(x))) {
    stop("A valid CRS is required.", call. = FALSE)
  }
  
  if (isTRUE(sf::st_is_longlat(x))) {
    centre <- sf::st_coordinates(
      sf::st_centroid(sf::st_union(sf::st_geometry(x)))
    )
    
    zone <- floor((centre[1] + 180) / 6) + 1
    epsg <- if (centre[2] >= 0) {
      32600 + zone
    } else {
      32700 + zone
    }
    
    return(sf::st_transform(x, epsg))
  }
  
  x
}


# Get groups of observations
.track_groups <- function(x, key) {
  dat <- sf::st_drop_geometry(x)
  
  if (length(key) == 0L) {
    return(list(seq_len(nrow(x))))
  }
  
  groups <- interaction(
    dat[key],
    drop = TRUE,
    lex.order = TRUE
  )
  
  split(seq_len(nrow(x)), groups)
}
