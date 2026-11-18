#' Estimate Animal Home Ranges
#'
#' Estimate animal home ranges using kernel density estimation.
#'
#' Home ranges describe the areas regularly used by animals
#' during their daily activities.
#'
#' @param x A `trackable` object.
#' @param levels Numeric vector specifying the home-range
#'   isopleth levels as percentages. Common values are 50 and 95.
#' @param h Smoothing parameter passed to
#'   [adehabitatHR::kernelUD()].
#' @param grid Grid resolution used for kernel estimation.
#' @param extent Extent multiplier for the estimation grid.
#' @param ... Additional arguments passed to
#'   [adehabitatHR::kernelUD()].
#'
#' @return An `sf` object containing estimated home-range polygons.
#'
#' @details
#' Home ranges are estimated using kernel density estimation.
#' The returned polygons represent the specified utilization
#' distribution isopleths.
#'
#' @references
#' Worton, B. J. (1989). Kernel methods for estimating the
#' utilization distribution in home-range studies.
#' *Ecology*, 70(1), 164–168.
#'
#' @seealso [track_utilization_distribution()], [track_hull()]
#'
#' @examples
#' # hr <- track_home_range(trk, levels = c(50, 95))
#'
#' @export
track_home_range <- function(
    x,
    levels = c(50, 95),
    h = "href",
    grid = 100,
    extent = 1,
    ...) {
  
  if (!is.numeric(levels) ||
      any(levels <= 0 | levels >= 100)) {
    stop(
      "`levels` must contain percentages between 0 and 100.",
      call. = FALSE
    )
  }
  
  meta <- .track_metadata(x)
  
  projected <- .track_project(x)
  
  ud <- track_utilization_distribution(
    x,
    h = h,
    grid = grid,
    extent = extent,
    ...
  )
  
  hr <- adehabitatHR::getverticeshr(
    ud,
    percent = levels
  )
  
  result <- sf::st_as_sf(hr)
  
  # Return polygons in the original CRS
  sf::st_transform(result, sf::st_crs(x))
}
