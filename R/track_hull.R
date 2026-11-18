#' Convex hull around observed locations
#'
#' Builds a minimum convex polygon (MCP) around each animal's fixes. With
#' `percent < 100` the fixes farthest from the arithmetic centroid are removed
#' first, which stops a few outlying fixes from inflating the hull.
#'
#' @param x A trackable object containing animal tracking observations.
#' @param id Name of the animal identifier column.
#' @param time Name of the date-time column.
#' @param lon Name of the longitude column in decimal degrees.
#' @param lat Name of the latitude column in decimal degrees.
#' @param percent Percentage of fixes (closest to the centroid) to keep, in
#'   (0, 100]. Default 100.
#' @param by_id If `TRUE` (default) one hull per animal; if `FALSE` one hull
#'   around all fixes pooled.
#' @param min_fixes Animals with fewer fixes are skipped with a warning.
#' @param crs Projected CRS (metres) used for the computation. Default: the UTM
#'   zone of the data.
#' @param crs_out Optional CRS for the returned geometry, e.g. `4326`. Default:
#'   the computation CRS.
#' @return An `sf` data frame with one polygon per animal and columns `id`,
#'   `n_fixes`, `n_used`, `percent`, `area_km2`, `perimeter_km`.
#' @examples
#' set.seed(1)
#' n <- 200
#' trk <- data.frame(
#'   animal = rep(c("E1", "E2"), each = n),
#'   timestamp = rep(seq(as.POSIXct("2026-01-01", tz = "UTC"), by = "1 hour",
#'                       length.out = n), 2),
#'   lon = 80.387 + c(cumsum(rnorm(n, 0, 0.0008)), cumsum(rnorm(n, 0.0002, 0.0008))),
#'   lat = 7.300 + c(cumsum(rnorm(n, 0, 0.0008)), cumsum(rnorm(n, 0, 0.0008)))
#' )
#' trk <- trackable(
#' data = trk,
#' time = timestamp,
#' x = lon,
#' y = lat,
#' key = c("animal"),
#' crs = 4326
#' )
#' track_hull(trk)
#' track_hull(trk, percent = 95, crs_out = 4326)
#' @export
track_hull <- function(x, id = NULL, time = NULL, lon = NULL, lat = NULL,
                       percent = 100, by_id = TRUE, min_fixes = 3L,
                       crs = NULL, crs_out = NULL) {
  if (!is.numeric(percent) || length(percent) != 1L || percent <= 0 || percent > 100) {
    stop("`percent` must be a single number in (0, 100].", call. = FALSE)
  }
  p <- .trk_prep(x, id, time, lon, lat, crs)
  d <- p$data
  if (!by_id) d$id <- "all"
  .trk_out(.trk_hull_sf(d, p$crs, percent, min_fixes), crs_out)
}
