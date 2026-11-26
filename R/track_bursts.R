#' Detect rapid movement episodes
#'
#' Identify periods of relatively rapid movement in animal tracking
#' data using a speed threshold.
#'
#' @param x A `trackable` object.
#' @param speed_threshold Minimum speed in metres per second used
#''   to identify rapid movement. Defaults to 2 m/s.
#' @param quantile Optional quantile between 0 and 1 used to determine
#'   an animal-specific speed threshold. If supplied, this overrides
#'   `speed_threshold`.
#' @param min_duration Minimum duration of a rapid movement episode,
#'   in seconds. Defaults to 60 seconds.
#' @param max_gap Maximum allowable time between consecutive
#'   observations, in seconds.
#' @param time Optional character string specifying the name of
#'   the time column. Defaults to the time index stored in the
#'   `trackable` object.
#' @param coords Optional character vector specifying the names
#'   of the two coordinate columns (longitude and latitude).
#'   Defaults to the coordinate columns stored in the
#'   `trackable` object.
#' @param ... Additional arguments.
#'
#' @return A tibble containing rapid movement episodes, with animal
#'   identifiers, start and end times, duration, maximum speed,
#'   and mean speed.
#'
#' @details
#' A movement burst is defined operationally as a sequence of
#' consecutive observations with speeds exceeding the specified
#' threshold.
#'
#' @examples
#' # track_bursts(elephant_tracks)
#' # track_bursts(elephant_tracks, quantile = 0.90)
#'
#' @export
track_bursts <- function(x,
                         speed_threshold = 2,
                         quantile = NULL,
                         min_duration = 60,
                         max_gap = Inf,
                         time = NULL,
                         coords = NULL,
                         ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  if (!is.null(quantile) &&
      (length(quantile) != 1 ||
       quantile < 0 || quantile > 1)) {
    stop("`quantile` must be between 0 and 1.")
  }
  
  if (is.null(time)) {
    time <- x$index
  }
  
  if (is.null(coords)) {
    coords <- c("x", "y")
  }
  
  key <- x$key
  if (is.null(key)) key <- character()
  
  dat <- tibble::as_tibble(x) |>
    dplyr::arrange(dplyr::across(
      dplyr::all_of(c(key, time))
    )) |>
    dplyr::group_by(dplyr::across(
      dplyr::all_of(key)
    )) |>
    dplyr::mutate(
      .elapsed_s = as.numeric(
        difftime(
          .data[[time]],
          dplyr::lag(.data[[time]]),
          units = "secs"
        )
      ),
      .distance_m = geosphere::distGeo(
        cbind(
          .data[[coords[1]]],
          .data[[coords[2]]]
        ),
        cbind(
          dplyr::lag(.data[[coords[1]]]),
          dplyr::lag(.data[[coords[2]]])
        )
      ),
      .speed_m_s = .distance_m / .elapsed_s
    )
  
  if (!is.null(quantile)) {
    dat <- dat |>
      dplyr::mutate(
        .threshold = stats::quantile(
          .speed_m_s,
          probs = quantile,
          na.rm = TRUE
        )
      )
  } else {
    dat <- dat |>
      dplyr::mutate(.threshold = speed_threshold)
  }
  
  dat <- dat |>
    dplyr::mutate(
      .burst = !is.na(.speed_m_s) &
        .speed_m_s >= .threshold &
        .elapsed_s <= max_gap,
      .run = cumsum(
        .burst != dplyr::lag(.burst, default = FALSE)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.burst) |>
    dplyr::group_by(dplyr::across(
      dplyr::all_of(c(key, ".run"))
    )) |>
    dplyr::summarise(
      start_time = min(.data[[time]]),
      end_time = max(.data[[time]]),
      duration = as.numeric(
        difftime(end_time, start_time, units = "secs")
      ),
      mean_speed = mean(.speed_m_s, na.rm = TRUE),
      max_speed = max(.speed_m_s, na.rm = TRUE),
      n_observations = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(duration >= min_duration) |>
    dplyr::select(-.run)
  
  tibble::as_tibble(dat)
}