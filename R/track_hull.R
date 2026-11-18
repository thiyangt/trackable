#' Calculate Convex or Concave Hulls for Animal Tracking Data
#'
#' Calculate a spatial boundary enclosing the observed locations
#' of each animal or group using either a convex or concave hull.
#' A convex hull provides the minimum convex polygon enclosing
#' all observed locations, whereas a concave hull can follow
#' irregular spatial patterns more closely.
#'
#' @param x A `trackable` object.
#' @param by Optional character vector specifying grouping variables.
#'   Defaults to the tracking keys stored in `x`.
#' @param method Character string specifying the hull method:
#'   `"convex"` for a minimum convex polygon or `"concave"` for
#'   a concave hull. Defaults to `"convex"`.
#' @param ... Additional arguments passed to the underlying
#'   hull calculation function.
#'
#' @return An `sf` object containing one hull polygon for each
#'   animal or group.
#'
#' @seealso [track_home_range()],
#'   [track_utilization_distribution()]
#'
#' @examples
#' # Calculate convex hulls
#' # track_hull(trk, method = "convex")
#'
#' # Calculate concave hulls
#' # track_hull(trk, method = "concave")
#'
#' @export
track_hull <- function(x, by = NULL,
                       method = c("convex", "concave"),
                       ...) {
  
  method <- match.arg(method)
  
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
    
    if (method == "convex") {
      hull <- sf::st_convex_hull(geometry, ...)
    } else {
      hull <- concaveman::concaveman(
        sf::st_as_sf(x[i, ]),
        ...
      )
      hull <- sf::st_geometry(hull)
    }
    
    out <- sf::st_drop_geometry(x[i[1], by, drop = FALSE])
    
    sf::st_sf(
      out,
      method = method,
      geometry = hull
    )
  })
  
  do.call(rbind, result)
}
