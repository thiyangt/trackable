#' Estimate Animal Utilization Distributions
#'
#' Estimate utilization distributions from animal tracking data
#' using kernel density estimation.
#'
#' A utilization distribution describes the relative probability
#' of an animal occurring at different locations.
#'
#' @param x A `trackable` object.
#' @param h Smoothing parameter passed to
#'   [adehabitatHR::kernelUD()]. Defaults to "href".
#' @param grid Grid resolution used for kernel estimation.
#' @param extent Extent multiplier for the estimation grid.
#' @param ... Additional arguments passed to
#'   [adehabitatHR::kernelUD()].
#'
#' @return An `estUD` object containing the estimated
#'   utilization distribution for each animal.
#'
#' @details
#' Kernel density estimation requires projected coordinates.
#' Geographic coordinates are automatically transformed to a
#' local UTM coordinate reference system for estimation.
#'
#' @seealso [track_home_range()], [track_hull()]
#'
#' @examples
#' # ud <- track_utilization_distribution(trk)
#'
#' @export
track_utilization_distribution <- function(
    x,
    h = "href",
    grid = 100,
    extent = 1,
    ...) {
  
  if (!requireNamespace("adehabitatHR", quietly = TRUE)) {
    stop(
      "Please install the 'adehabitatHR' package.",
      call. = FALSE
    )
  }
  
  if (!requireNamespace("sp", quietly = TRUE)) {
    stop("Please install the 'sp' package.", call. = FALSE)
  }
  
  meta <- .track_metadata(x)
  
  if (anyDuplicated(
    sf::st_drop_geometry(x)[c(meta$key, meta$time)]
  )) {
    stop("Duplicate timestamps within tracking groups.",
         call. = FALSE)
  }
  
  projected <- .track_project(x)
  
  dat <- sf::st_drop_geometry(projected)
  
  # Create an animal identifier
  dat$.animal <- interaction(
    dat[meta$key],
    drop = TRUE,
    lex.order = TRUE
  )
  
  spatial <- sf::st_sf(
    dat[".animal"],
    geometry = sf::st_geometry(projected)
  )
  
  spatial <- sf::st_cast(spatial, "POINT")
  
  spatial$.animal <- factor(spatial$.animal)
  
  spatial_sp <- as(spatial, "Spatial")
  
  adehabitatHR::kernelUD(
    spatial_sp[".animal"],
    h = h,
    grid = grid,
    extent = extent,
    ...
  )
}
