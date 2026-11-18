#' Detect stopping periods in animal movement tracks
#'
#' Identify periods during which an animal exhibits low movement for a
#' specified minimum duration. A stopping period is identified using
#' consecutive observations with speeds below a specified threshold.
#'
#' @param x A `trackable` object.
#' @param speed_threshold Maximum speed (metres per second) considered
#'   low movement. Defaults to 0.2 m/s.
#' @param min_duration Minimum duration of a stopping period, in seconds.
#'   Defaults to 1800 seconds (30 minutes).
#' @param radius Maximum spatial displacement from the first location
#'   in a stopping period, in metres. Defaults to 50 metres.
#' @param max_gap Maximum allowable time between consecutive observations,
#'   in seconds. Larger gaps terminate a stopping period.
#' @param ... Additional arguments.
#'
#' @return A tibble containing detected stopping periods, including
#'   animal identifiers, start and end times, duration, number of
#'   observations, and spatial displacement.
#'
#' @details
#' Stopping periods are identified from consecutive low-speed
#' observations. A candidate stop must satisfy both the minimum
#' duration and spatial displacement criteria.
#'
#' A stopping period does not necessarily indicate resting or sleeping.
#'
#' @examples
#' # track_stop(elephant_tracks)
#' # track_stop(elephant_tracks, speed_threshold = 0.1,
#' #            min_duration = 3600)
#'
#' @export
track_stop <- function(x,
                       speed_threshold = 0.2,
                       min_duration = 1800,
                       radius = 50,
                       max_gap = Inf,
                       ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  dat <- .track_metrics(x)
  
  key <- x$key
  if (is.null(key) || length(key) == 0) {
    key <- character()
  }
  
  dat <- dat |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(key, ".time"))))
  
  group_cols <- c(key)
  
  dat <- dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      .low = !is.na(.speed_m_s) &
        .speed_m_s <= speed_threshold &
        .elapsed_s <= max_gap,
      .run = cumsum(.low != dplyr::lag(.low, default = FALSE))
    ) |>
    dplyr::ungroup()
  
  dat <- dat |>
    dplyr::filter(.low) |>
    dplyr::group_by(dplyr::across(
      dplyr::all_of(c(key, ".run"))
    )) |>
    dplyr::summarise(
      start_time = min(.time),
      end_time = max(.time),
      duration = as.numeric(
        difftime(end_time, start_time, units = "secs")
      ),
      n_observations = dplyr::n(),
      displacement = {
        idx <- dplyr::first(dplyr::row_number())
        dx <- .x <- dplyr::first(.longitude)
        dy <- dplyr::first(.latitude)
        .distance_from_start <- .track_distance(
          dx, dy, .data$.longitude, .data$.latitude
        )
        max(.distance_from_start, na.rm = TRUE)
      },
      .groups = "drop"
    ) |>
    dplyr::filter(
      duration >= min_duration,
      displacement <= radius
    ) |>
    dplyr::select(-.run)
  
  tibble::as_tibble(dat)
}

