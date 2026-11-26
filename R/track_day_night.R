#' Compare daytime and nighttime animal movement
#'
#' Summarise movement activity during daytime and nighttime.
#'
#' @param x A `trackable` object.
#' @param day_start Hour at which daytime begins, using local
#'   clock time. Defaults to 6.
#' @param night_start Hour at which nighttime begins. Defaults to 18.
#' @param tz Time zone used to determine local clock time.
#'   Defaults to the time zone of the tracking timestamps.
#' @param time Optional character string specifying the name of
#'   the time column. Defaults to the time index stored in the
#'   `trackable` object.
#' @param coords Optional character vector specifying the names
#'   of the two coordinate columns (longitude and latitude).
#'   Defaults to the coordinate columns stored in the
#'   `trackable` object.
#' @param ... Additional arguments.
#'
#' @return A tibble containing movement summaries for daytime and
#'   nighttime, including total distance, mean speed, and
#'   observation counts.
#'
#' @details
#' Daytime is defined as the interval from `day_start` (inclusive)
#' to `night_start` (exclusive). Nighttime is the remaining interval.
#' These are clock-time definitions and do not automatically account
#' for sunrise, sunset, or seasonal changes in daylight.
#'
#' @examples
#' # track_day_night(elephant_tracks)
#' # track_day_night(elephant_tracks,
#' #                 day_start = 6,
#' #                 night_start = 18)
#'
#' @export
track_day_night <- function(x,
                            day_start = 6,
                            night_start = 18,
                            tz = NULL,
                            time = NULL,
                            coords = NULL,
                            ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  # Validate hours
  if (length(day_start) != 1L ||
      length(night_start) != 1L ||
      !is.finite(day_start) ||
      !is.finite(night_start) ||
      day_start < 0 || day_start > 23 ||
      night_start < 0 || night_start > 23 ||
      day_start == night_start) {
    stop("Invalid daytime or nighttime hours.")
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
  
  # Determine timezone
  if (is.null(tz)) {
    tz <- attr(dat[[time]], "tzone")
    
    if (is.null(tz) || !nzchar(tz[1])) {
      tz <- "UTC"
    } else {
      tz <- tz[1]
    }
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
  
  # Classify daytime and nighttime
  hour <- as.integer(
    format(dat[[time]], "%H", tz = tz)
  )
  
  is_day <- if (day_start < night_start) {
    hour >= day_start & hour < night_start
  } else {
    hour >= day_start | hour < night_start
  }
  
  dat$.period <- ifelse(
    is_day,
    "daytime",
    "nighttime"
  )
  
  # Summarise movement by period
  dat |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(key, ".period"))
      )
    ) |>
    dplyr::summarise(
      n_observations = dplyr::n(),
      
      total_distance = sum(
        .distance_m,
        na.rm = TRUE
      ),
      
      mean_speed = if (all(is.na(.speed_m_s))) {
        NA_real_
      } else {
        mean(.speed_m_s, na.rm = TRUE)
      },
      
      max_speed = if (all(is.na(.speed_m_s))) {
        NA_real_
      } else {
        max(.speed_m_s, na.rm = TRUE)
      },
      
      .groups = "drop"
    ) |>
    dplyr::rename(period = .period) |>
    tibble::as_tibble()
}