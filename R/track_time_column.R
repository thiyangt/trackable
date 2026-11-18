# Resolve the timestamp column from trackable metadata
.track_time_column <- function(x) {
  meta <- .track_metadata(x)
  
  candidates <- c(
    meta$time,
    meta$index,
    attr(x, "track_time"),
    "time",
    "date_time"
  )
  
  candidates <- unlist(candidates, use.names = FALSE)
  candidates <- candidates[!is.na(candidates)]
  
  found <- candidates[candidates %in% names(x)]
  
  if (length(found) == 0) {
    stop("Could not identify the time column in `x`.",
         call. = FALSE)
  }
  
  found[[1]]
}


# Calculate elapsed time in seconds
.track_elapsed <- function(time) {
  as.numeric(diff(time), units = "secs")
}


# Calculate distances between consecutive GPS fixes
.track_step_distance <- function(x) {
  coords <- sf::st_coordinates(x)
  
  if (nrow(coords) < 2) {
    return(numeric())
  }
  
  geosphere::distHaversine(
    coords[-nrow(coords), 1:2, drop = FALSE],
    coords[-1, 1:2, drop = FALSE]
  )
}
