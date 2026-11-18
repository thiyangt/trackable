#' Calculate Convex Hulls for Animal Tracking Data
#'
#' Calculate the minimum convex polygon enclosing the observed
#' locations of each animal or group.
#'
#' @param x A `trackable` object.
#' @param by Optional character vector specifying grouping variables.
#'   Defaults to the tracking keys stored in `x`.
#' @param ... Additional arguments passed to [sf::st_convex_hull()].
#'
#' @return An `sf` object containing one convex hull polygon
#'   for each animal or group.
#'
#' @seealso [track_home_range()], [track_utilization_distribution()]
#'
#' @examples
#' # hull <- track_hull(trk)
#'
#' @export
track_hull <- function(x, by = NULL, ...) {
  
  meta <- .track_metadata(x)
  
  if (is.null(by)) {
    by <- meta$key
  }
  
  if (!all(by %in% names(x))) {
    stop("Grouping variables not found in `x`.", call. = FALSE)
  }
  
  groups <- .track_groups(x, by)
  
  result <- lapply(groups, function(i) {
    
    geometry <- sf::st_union(sf::st_geometry(x[i, ]))
    
    hull <- sf::st_convex_hull(geometry, ...)
    
    out <- sf::st_drop_geometry(x[i[1], by, drop = FALSE])
    
    sf::st_sf(
      out,
      geometry = hull
    )
  })
  
  do.call(rbind, result)
}
