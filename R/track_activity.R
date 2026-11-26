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
#' @param time Optional character string specifying the name of
#'   the time column. Defaults to the time index stored in the
#'   `trackable` object.
#' @param coords Optional character vector specifying the names
#'   of the two coordinate columns (longitude and latitude).
#'   Defaults to the coordinate columns stored in the
#'   `trackable` object.
#'
#' @return A tibble containing temporal movement summaries for each
#'   animal, including observation count, total distance travelled,
#'   mean speed, maximum speed, and active observation count.
#'
#' @details
#' Movement distances are calculated between consecutive observed
#' locations using the Haversine formula. Each movement step is
#' assigned to the interval containing its destination observation.
#'
#' @export
track_activity <- function(x,
                           by = "hour",
                           active_threshold = 0.2,
                           time = NULL,
                           coords = NULL,
                           ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  # Validate arguments
  by <- match.arg(
    by,
    c("hour", "day", "week", "month", "year")
  )
  
  if (!is.numeric(active_threshold) ||
      length(active_threshold) != 1L ||
      !is.finite(active_threshold) ||
      active_threshold < 0) {
    stop("`active_threshold` must be a non-negative number.")
  }
  
  # Extract trackable metadata
  if (is.null(time)) {
    time <- attr(x, "trackable_time")
  }
  
  key <- attr(x, "trackable_key")
  coords_meta <- attr(x, "trackable_coords")
  
  if (is.null(coords)) {
    coords <- unname(coords_meta[c("x", "y")])
  }
  
  if (is.null(key)) {
    key <- character()
  }
  
  # Extract data
  dat <- tibble::as_tibble(
    sf::st_drop_geometry(x)
  )
  
  # Validate columns
  required <- c(time, key, coords)
  
  missing_cols <- setdiff(required, names(dat))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (length(coords) != 2L) {
    stop("`coords` must contain two coordinate column names.")
  }
  
  # Calculate movement metrics
  dat <- dat |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(key)),
      .data[[time]]
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(key))
    ) |>
    dplyr::mutate(
      .previous_time = dplyr::lag(.data[[time]]),
      
      .elapsed_s = as.numeric(
        difftime(
          .data[[time]],
          .previous_time,
          units = "secs"
        )
      ),
      
      .distance_m = geosphere::distHaversine(
        cbind(
          dplyr::lag(.data[[coords[1]]]),
          dplyr::lag(.data[[coords[2]]])
        ),
        cbind(
          .data[[coords[1]]],
          .data[[coords[2]]]
        )
      ),
      
      .distance_m = dplyr::if_else(
        is.na(.elapsed_s) | .elapsed_s <= 0,
        NA_real_,
        .distance_m
      ),
      
      .speed_m_s = .distance_m / .elapsed_s
    ) |>
    dplyr::ungroup()
  
  # Create temporal intervals
  dat$.period <- lubridate::floor_date(
    dat[[time]],
    unit = by
  )
  
  # Summarise activity
  dat |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(key, ".period"))
      )
    ) |>
    dplyr::summarise(
      n_observations = dplyr::n(),
      
      total_distance_m = sum(
        .distance_m,
        na.rm = TRUE
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