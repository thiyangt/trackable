#' Summarise animal movement activity over time
#'
#' Calculate movement summaries for specified temporal intervals.
#'
#' @param x A `trackable` object.
#' @param by Temporal aggregation level. One of `"hour"`, `"day"`,
#'   `"week"`, `"month"`, or `"year"`.
#' @param active_threshold Speed threshold (m/s) for identifying
#'   active observations. Defaults to 0.2.
#' @param ... Additional arguments.
#'
#' @return A tibble containing temporal movement summaries for each
#'   animal.
#'
#' @details
#' Distances are calculated between consecutive observed locations
#' using the Haversine formula. Each movement step is assigned to
#' the interval containing its destination observation.
#'
#' @export
track_activity <- function(x, by = "hour",
                           active_threshold = 0.2, ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  by <- match.arg(
    by,
    c("hour", "day", "week", "month", "year")
  )
  
  stopifnot(
    is.numeric(active_threshold),
    length(active_threshold) == 1L,
    is.finite(active_threshold),
    active_threshold >= 0
  )
  
  dat <- tibble::as_tibble(x)
  
  # Identify grouping variables
  key <- x$key
  if (is.null(key)) key <- character()
  
  # Calculate movement metrics
  dat <- dat |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(key)),
      time
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(key))
    ) |>
    dplyr::mutate(
      .distance_m = geosphere::distHaversine(
        cbind(dplyr::lag(x), dplyr::lag(y)),
        cbind(x, y)
      ),
      .distance_m = dplyr::if_else(
        is.na(dplyr::lag(time)) |
          as.numeric(difftime(
            time, dplyr::lag(time), units = "secs"
          )) <= 0,
        NA_real_,
        .distance_m
      ),
      .time_diff = as.numeric(
        difftime(time, dplyr::lag(time), units = "secs")
      ),
      .speed_m_s = .distance_m / .time_diff
    ) |>
    dplyr::ungroup()
  
  # Define temporal intervals
  dat$.period <- switch(
    by,
    hour = lubridate::floor_date(dat$time, "hour"),
    day = lubridate::floor_date(dat$time, "day"),
    week = lubridate::floor_date(dat$time, "week"),
    month = lubridate::floor_date(dat$time, "month"),
    year = lubridate::floor_date(dat$time, "year")
  )
  
  # Aggregate movement activity
  dat |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(key, ".period"))
      )
    ) |>
    dplyr::summarise(
      n_observations = dplyr::n(),
      
      total_distance_m = sum(
        .distance_m, na.rm = TRUE
      ),
      
      mean_speed_m_s = if (all(is.na(.speed_m_s))) {
        NA_real_
      } else {
        mean(.speed_m_s, na.rm = TRUE)
      },
      
      max_speed_m_s = if (all(is.na(.speed_m_s))) {
        NA_real_
      } else {
        max(.speed_m_s, na.rm = TRUE)
      },
      
      active_observations = sum(
        .speed_m_s > active_threshold,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) |>
    dplyr::rename(period = .period) |>
    tibble::as_tibble()
}
