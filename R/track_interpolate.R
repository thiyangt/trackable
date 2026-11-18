#' Interpolate Missing Animal Locations
#'
#' Estimate missing spatial locations between consecutive
#' observed GPS fixes using linear interpolation.
#'
#' @param x A `trackable` object.
#' @param by Optional character vector specifying grouping variables.
#'   Defaults to the tracking keys stored in `x`.
#' @param method Interpolation method. Currently supports `"linear"`.
#' @param max_gap Maximum time interval in seconds over which
#'   interpolation is permitted. Defaults to Inf.
#' @param ... Currently unused.
#'
#' @return A spatial tracking object with interpolated locations.
#'
#' @seealso [track_regularise()], [track_gaps()]
#'
#' @export
track_interpolate <- function(x, by = NULL,
                              method = "linear",
                              max_gap = Inf, ...) {
  
  method <- match.arg(method, "linear")
  
  meta <- .track_metadata(x)
  
  if (is.null(by)) {
    by <- meta$key
  }
  
  if (!all(by %in% names(x))) {
    stop("Grouping variables not found in `x`.", call. = FALSE)
  }
  
  if (length(max_gap) != 1 || is.na(max_gap) || max_gap <= 0) {
    stop("`max_gap` must be a positive number of seconds.",
         call. = FALSE)
  }
  
  time_col <- .track_time_column(x)
  groups <- .track_groups(x, by)
  
  result <- lapply(groups, function(i) {
    
    d <- x[i, ]
    d <- d[order(d[[time_col]]), ]
    
    times <- d[[time_col]]
    
    if (anyNA(times) || anyDuplicated(times)) {
      stop("Timestamps must be complete and unique within groups.",
           call. = FALSE)
    }
    
    coords <- sf::st_coordinates(d)
    
    if (nrow(coords) != nrow(d)) {
      stop("Only single-point geometries are supported.",
           call. = FALSE)
    }
    
    missing <- sf::st_is_empty(d)
    
    if (!any(missing)) return(d)
    
    observed <- which(!missing)
    
    if (length(observed) < 2) return(d)
    
    for (j in which(missing)) {
      
      left <- observed[observed < j]
      right <- observed[observed > j]
      
      if (!length(left) || !length(right)) next
      
      a <- max(left)
      b <- min(right)
      
      elapsed <- as.numeric(
        difftime(times[b], times[a], units = "secs")
      )
      
      if (elapsed <= 0 || elapsed > max_gap) next
      
      fraction <- as.numeric(
        difftime(times[j], times[a], units = "secs")
      ) / elapsed
      
      xy <- coords[a, 1:2] +
        fraction * (coords[b, 1:2] - coords[a, 1:2])
      
      geom <- sf::st_sfc(
        sf::st_point(xy),
        crs = sf::st_crs(d)
      )
      
      sf::st_geometry(d)[j] <- geom
    }
    
    d
  })
  
  do.call(rbind, result)
}
