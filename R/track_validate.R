#' Validate Animal Tracking Data
#'
#' Check the structural, spatial, and temporal consistency
#' of animal tracking data.
#'
#' The function checks animal identifiers, timestamps,
#' spatial coordinates, duplicate observations, and
#' chronological ordering.
#'
#' @param x A `trackable` object.
#' @param check_order Logical. Check chronological ordering
#'   within each animal.
#' @param check_duplicates Logical. Check for duplicated
#'   timestamps within each animal.
#' @param check_coordinates Logical. Check spatial coordinates.
#' @param check_time Logical. Check for missing or invalid timestamps.
#' @param ... Currently unused.
#'
#' @return A tibble containing validation checks, their status,
#'   the number of affected observations, and a description.
#'
#' @seealso [track_gaps()], [track_duplicates()],
#'   [track_outliers()]
#'
#' @export
track_validate <- function(x,
                           check_order = TRUE,
                           check_duplicates = TRUE,
                           check_coordinates = TRUE,
                           check_time = TRUE,
                           ...) {
  
  if (!inherits(x, "trackable")) {
    stop("`x` must be a trackable object.", call. = FALSE)
  }
  
  meta <- .track_metadata(x)
  time_col <- .track_time_column(x)
  
  key <- meta$key
  
  checks <- list()
  
  add_check <- function(name, failed, description) {
    
    checks[[length(checks) + 1]] <<- tibble::tibble(
      check = name,
      status = if (failed == 0) "PASS" else "WARNING",
      n_issues = as.integer(failed),
      description = description
    )
  }
  
  # Check identifiers
  missing_keys <- if (length(key) > 0) {
    sum(!stats::complete.cases(sf::st_drop_geometry(x)[, key,
                                                       drop = FALSE]))
  } else {
    nrow(x)
  }
  
  add_check(
    "identifiers",
    missing_keys,
    "Missing tracking identifiers."
  )
  
  # Check timestamps
  if (check_time) {
    
    missing_time <- sum(is.na(x[[time_col]]))
    
    add_check(
      "timestamps",
      missing_time,
      "Missing or invalid timestamps."
    )
  }
  
  # Check duplicated timestamps
  if (check_duplicates) {
    
    duplicate_time <- sum(vapply(
      .track_groups(x, key),
      function(i) {
        sum(duplicated(x[[time_col]][i]))
      },
      numeric(1)
    ))
    
    add_check(
      "duplicate_timestamps",
      duplicate_time,
      "Repeated timestamps within tracking groups."
    )
  }
  
  # Check chronological ordering
  if (check_order) {
    
    unordered <- sum(vapply(
      .track_groups(x, key),
      function(i) {
        sum(diff(as.numeric(x[[time_col]][i])) < 0,
            na.rm = TRUE)
      },
      numeric(1)
    ))
    
    add_check(
      "chronological_order",
      unordered,
      "Observations not arranged chronologically."
    )
  }
  
  # Check spatial coordinates
  if (check_coordinates) {
    
    geom <- sf::st_geometry(x)
    
    empty <- sf::st_is_empty(geom)
    
    invalid <- !sf::st_is_valid(geom)
    
    invalid[is.na(invalid)] <- FALSE
    
    bad_geometry <- sum(!empty & invalid) +
      sum(is.na(empty))
    
    add_check(
      "spatial_geometry",
      bad_geometry,
      "Invalid or missing spatial geometries."
    )
    
    if (sf::st_is_longlat(x)) {
      
      coords <- sf::st_coordinates(x)
      
      bad_coordinates <- sum(
        !is.finite(coords[, 1]) |
          !is.finite(coords[, 2]) |
          coords[, 1] < -180 |
          coords[, 1] > 180 |
          coords[, 2] < -90 |
          coords[, 2] > 90
      )
      
      add_check(
        "coordinate_ranges",
        bad_coordinates,
        "Coordinates outside valid longitude/latitude ranges."
      )
    }
  }
  
  dplyr::bind_rows(checks)
}
