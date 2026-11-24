#' Detect stopping periods in animal movement tracks
#'
#' Identify periods during which an animal exhibits low movement
#' for a specified minimum duration.
#'
#' @param x A `trackable` object.
#' @param speed_threshold Maximum speed in metres per second.
#'   Defaults to 0.2.
#' @param min_duration Minimum stopping duration in seconds.
#'   Defaults to 1800 (30 minutes).
#' @param radius Maximum displacement from the first location
#'   in a stopping period, in metres. Defaults to 50.
#' @param max_gap Maximum time between consecutive observations,
#'   in seconds. Larger gaps terminate a stopping period.
#' @param ... Additional arguments.
#'
#' @return A tibble containing detected stopping periods.
#'
#' @details
#' Stopping periods are identified using consecutive observations
#' with speeds below the specified threshold. Candidate periods
#' must satisfy both the minimum duration and spatial displacement
#' criteria.
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
  
  time_col <- attr(x, "trackable_time")
  key <- attr(x, "trackable_key")
  coords <- attr(x, "trackable_coords")
  
  lon_col <- coords["x"]
  lat_col <- coords["y"]
  
  dat <- tibble::as_tibble(x)
  
  dat <- dat |>
    dplyr::arrange(
      dplyr::across(
        dplyr::all_of(c(key, time_col))
      )
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(key))
    ) |>
    dplyr::mutate(
      .elapsed_s = as.numeric(
        difftime(
          .data[[time_col]],
          dplyr::lag(.data[[time_col]]),
          units = "secs"
        )
      ),
      
      .step_m = geosphere::distHaversine(
        cbind(
          dplyr::lag(.data[[lon_col]]),
          dplyr::lag(.data[[lat_col]])
        ),
        cbind(
          .data[[lon_col]],
          .data[[lat_col]]
        )
      ),
      
      .speed_m_s = .step_m / .elapsed_s,
      
      .low = !is.na(.speed_m_s) &
        .elapsed_s > 0 &
        .elapsed_s <= max_gap &
        .speed_m_s <= speed_threshold,
      
      .run = cumsum(
        .low != dplyr::lag(.low, default = FALSE)
      )
    ) |>
    dplyr::ungroup()
  
  dat |>
    dplyr::filter(.low) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(key, ".run"))
      )
    ) |>
    dplyr::summarise(
      start_time = min(.data[[time_col]]),
      end_time = max(.data[[time_col]]),
      
      duration = as.numeric(
        difftime(end_time, start_time, units = "secs")
      ),
      
      n_observations = dplyr::n(),
      
      displacement = max(
        geosphere::distHaversine(
          cbind(
            dplyr::first(.data[[lon_col]]),
            dplyr::first(.data[[lat_col]])
          ),
          cbind(
            .data[[lon_col]],
            .data[[lat_col]]
          )
        ),
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) |>
    dplyr::filter(
      duration >= min_duration,
      displacement <= radius
    ) |>
    dplyr::select(-.run) |>
    tibble::as_tibble()
}
