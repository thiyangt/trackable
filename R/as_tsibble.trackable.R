#' Convert a trackable object to a tsibble
#'
#' Converts a `trackable` object into a `tsibble`, preserving
#' the temporal index, key variables, and tracking attributes
#' stored as columns.
#'
#' @param x A `trackable` object.
#' @param ... Additional arguments passed to
#'   [tsibble::as_tsibble()].
#'
#' @return A `tsibble` object containing the tracking data.
#'
#' @seealso [trackable()], [tsibble::as_tsibble()]
#'
#' @export
#'
#' @examples
#' elephant_data <- tibble::tibble(
#'   animal_id = "E001",
#'   time = as.POSIXct("2026-11-18 06:00:00") +
#'     (0:4) * 3600,
#'   x = c(500000, 500120, 500260, 500390, 500530),
#'   y = c(850000, 850040, 850090, 850130, 850180)
#' )
#'
#' elephant_track <- trackable(
#'   data = elephant_data,
#'   time = time,
#'   x = x,
#'   y = y,
#'   crs = 4326,
#'   key = animal_id
#' )
#'
#' elephant_tsibble <- tsibble::as_tsibble(elephant_track)
#'
#' elephant_tsibble
#'
#' @importFrom tsibble as_tsibble
as_tsibble.trackable <- function(x, ...) {
  
  data <- tibble::as_tibble(x)
  
  tsibble::as_tsibble(
    data,
    key = animal_id,
    index = time,
    ...
  )
}