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
#' @param time Optional character string specifying the name of
#'   the time column. Defaults to the time index stored in the
#'   `trackable` object.
#' @param coords Optional character vector specifying the names
#'   of the two coordinate columns (longitude and latitude).
#'   Defaults to the coordinate columns stored in the
#'   `trackable` object.
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
                              time = NULL,
                              coords = NULL,
                              ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  by <- match.arg(
    by,
    c("month", "quarter", "season")
  )
  
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
  
  # Extract month and year
  month <- as.integer(
    format(dat[[time]], "%m")
  )
  
  year <- as.integer(
    format(dat[[time]], "%Y")
  )
  
  # Define temporal periods
  if (by == "month") {
    
    dat$.period <- format(
      dat[[time]],
      "%Y-%m"
    )
    
  } else if (by == "quarter") {
    
    dat$.period <- paste0(
      year,
      "-Q",
      (month - 1) %/% 3 + 1
    )
    
  } else if (by == "season") {
    
    if (is.null(seasons)) {
      seasons <- list(
        Summer = c(12, 1, 2),
        Autumn = c(3, 4, 5),
        Winter = c(6, 7, 8),
        Spring = c(9, 10, 11)
      )
    }
    
    # Validate seasonal definitions
    season_months <- unlist(
      seasons,
      use.names = FALSE
    )
    
    if (is.null(names(seasons)) ||
        any(names(seasons) == "") ||
        anyDuplicated(names(seasons)) ||
        !is.numeric(season_months) ||
        anyNA(season_months) ||
        any(!is.finite(season_months)) ||
        any(season_months != as.integer(season_months)) ||
        any(season_months < 1 | season_months > 12) ||
        anyDuplicated(season_months)) {
      
      stop(
        "`seasons` must be a named list with ",
        "non-overlapping month numbers from 1 to 12."
      )
    }
    
    season_names <- names(seasons)
    
    season <- vapply(month, function(m) {
      
      matched <- season_names[
        vapply(seasons, function(z) m %in% z, logical(1))
      ]
      
      if (length(matched) == 0) {
        NA_character_
      } else {
        matched[1]
      }
      
    }, character(1))
    
    dat$.period <- ifelse(
      is.na(season),
      NA_character_,
      paste(year, season, sep = "-")
    )
  }
  
  # Summarise movement by temporal period
  dat |>
    dplyr::filter(!is.na(.period)) |>
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