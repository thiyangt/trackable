#' Summarise animal movement activity over time
#'
#' Calculate movement summaries for specified temporal intervals.
#'
#' @param x A `trackable` object.
#' @param by Temporal aggregation level. One of `"hour"`, `"day"`,
#'   `"week"`, `"month"`, or `"year"`.
#' @param ... Additional arguments.
#'
#' @return A tibble containing temporal movement summaries for each
#'   animal, including observation count, total distance travelled,
#'   mean speed, maximum speed, and active observation count.
#'
#' @details
#' This function provides an overview of temporal variation in
#' movement activity. Distance is calculated from consecutive
#' observed locations. The first observation in each group does
#' not contribute a distance.
#'
#' @examples
#' # track_activity(elephant_tracks, by = "hour")
#' # track_activity(elephant_tracks, by = "day")
#' # track_activity(elephant_tracks, by = "month")
#'
#' @export
track_activity <- function(x, by = "hour", ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  by <- match.arg(
    by,
    c("hour", "day", "week", "month", "year")
  )
  
  dat <- .track_metrics(x)
  
  dat$.period <- switch(
    by,
    hour = format(dat$.time, "%Y-%m-%d %H"),
    day = format(dat$.time, "%Y-%m-%d"),
    week = format(dat$.time, "%G-W%V"),
    month = format(dat$.time, "%Y-%m"),
    year = format(dat$.time, "%Y")
  )
  
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
      active_observations = sum(
        .speed_m_s > 0.2, na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(period = .period) |>
    tibble::as_tibble()
}
