#' Examine seasonal variation in animal movement
#'
#' Summarise movement activity across months, quarters, or
#' predefined meteorological seasons.
#'
#' @param x A `trackable` object.
#' @param by Temporal grouping method. One of `"month"`,
#'   `"quarter"`, or `"season"`.
#' @param seasons Optional named list defining the months
#'   associated with each season. Each element should contain
#'   integer month numbers from 1 to 12.
#' @param ... Additional arguments.
#'
#' @return A tibble containing seasonal movement summaries,
#'   including total distance, mean speed, maximum speed,
#'   and observation counts.
#'
#' @details
#' By default, meteorological seasons follow the conventional
#' Northern Hemisphere grouping:
#' December-February, March-May, June-August, and
#' September-November.
#'
#' For tropical or Southern Hemisphere studies, users can supply
#' an alternative definition through `seasons`.
#'
#' @examples
#' # track_seasonality(elephant_tracks, by = "month")
#' # track_seasonality(elephant_tracks, by = "quarter")
#'
#' # Example seasonal definition for a study area:
#' # seasons <- list(
#' #   First_inter_monsoon = c(3, 4),
#' #   Southwest_monsoon = c(5, 6, 7, 8, 9),
#' #   Second_inter_monsoon = c(10),
#' #   Northeast_monsoon = c(11, 12, 1, 2)
#' # )
#' # track_seasonality(
#' #   elephant_tracks,
#' #   by = "season",
#' #   seasons = seasons
#' # )
#'
#' @export
track_seasonality <- function(x,
                              by = "season",
                              seasons = NULL,
                              ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  by <- match.arg(by, c("month", "quarter", "season"))
  
  dat <- .track_metrics(x)
  
  month <- as.integer(format(dat$.time, "%m"))
  year <- as.integer(format(dat$.time, "%Y"))
  
  if (by == "month") {
    dat$.period <- format(dat$.time, "%Y-%m")
  }
  
  if (by == "quarter") {
    dat$.period <- paste0(
      year, "-Q", (month - 1) %/% 3 + 1
    )
  }
  
  if (by == "season") {
    
    if (is.null(seasons)) {
      seasons <- list(
        Summer = c(12, 1, 2),
        Autumn = c(3, 4, 5),
        Winter = c(6, 7, 8),
        Spring = c(9, 10, 11)
      )
    }
    
    season_names <- names(seasons)
    
    if (is.null(season_names) ||
        anyDuplicated(season_names) ||
        any(unlist(seasons) < 1 | unlist(seasons) > 12) ||
        anyDuplicated(unlist(seasons))) {
      stop("`seasons` must be a named list with ",
           "non-overlapping month numbers from 1 to 12.")
    }
    
    dat$.period <- vapply(month, function(m) {
      matched <- names(Filter(
        function(z) m %in% z, seasons
      ))
      
      if (length(matched) == 0) {
        NA_character_
      } else {
        matched[1]
      }
    }, character(1))
    
    dat$.period <- paste(year, dat$.period, sep = "-")
  }
  
  key <- x$key
  if (is.null(key)) key <- character()
  
  dat |>
    dplyr::filter(!is.na(.period)) |>
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
