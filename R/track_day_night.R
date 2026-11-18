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
                            ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  if (day_start < 0 || day_start > 23 ||
      night_start < 0 || night_start > 23 ||
      day_start == night_start) {
    stop("Invalid daytime or nighttime hours.")
  }
  
  dat <- .track_metrics(x)
  
  if (is.null(tz)) {
    tz <- attr(dat$.time, "tzone")
    if (is.null(tz) || !nzchar(tz[1])) {
      tz <- "UTC"
    }
  }
  
  hour <- as.integer(format(dat$.time, "%H", tz = tz))
  
  is_day <- if (day_start < night_start) {
    hour >= day_start & hour < night_start
  } else {
    hour >= day_start | hour < night_start
  }
  
  dat$.period <- ifelse(is_day, "daytime", "nighttime")
  
  key <- x$key
  if (is.null(key)) key <- character()
  
  dat |>
    dplyr::group_by(dplyr::across(
      dplyr::all_of(c(key, ".period"))
    )) |>
    dplyr::summarise(
      n_observations = dplyr::n(),
      total_distance = sum(.distance_m, na.rm = TRUE),
      mean_speed = mean(.speed_m_s, na.rm = TRUE),
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