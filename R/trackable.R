
#' Create a trackable object
#'
#' Create a tidyverse-friendly data structure for animal tracking data.
#' Each row represents an observation at a particular time and location.
#' Multiple trajectories can be stored in a single object.
#'
#' @param data A data frame or tibble containing tracking observations.
#' @param time The temporal index column, supplied as a bare name or
#'   a character string.
#' @param x The x-coordinate column, supplied as a bare name or
#'   a character string.
#' @param y The y-coordinate column, supplied as a bare name or
#'   a character string.
#' @param key A character vector of grouping variables, or an unquoted
#'   expression such as `c(animal_id, Length, Sex)`.
#' @param crs Coordinate reference system. Use `4326` for longitude
#'   and latitude.
#' @param ... Additional arguments passed to [sf::st_as_sf()].
#'
#' @return An object inheriting from `trackable` and `sf`.
#'
#' @examples
#' dat <- tibble::tibble(
#'   animal_id = c("E001", "E001", "E002", "E002"),
#'   time = as.Date("2026-01-01") + c(0, 1, 0, 1),
#'   x = c(80.77, 80.78, 80.81, 80.82),
#'   y = c(7.87, 7.88, 7.90, 7.91),
#'   Length = c(250, 250, 270, 270),
#'   Sex = c("Female", "Female", "Male", "Male")
#' )
#'
#' trk <- trackable(
#'   dat,
#'   time = time,
#'   x = x,
#'   y = y,
#'   key = c("animal_id", "Length", "Sex"),
#'   crs = 4326
#' )
#'
#' trk
#'
#' @export
trackable <- function(data, time, x, y, key, crs = NA, ...) {
  
  # Check input data
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  
  # Capture column names without evaluating them
  get_column_name <- function(expr) {
    if (is.character(expr) && length(expr) == 1L) {
      return(expr)
    }
    
    rlang::as_name(rlang::ensym(expr))
  }
  
  time_name <- get_column_name(substitute(time))
  x_name <- get_column_name(substitute(x))
  y_name <- get_column_name(substitute(y))
  
  # Capture multiple key variables
  
  # Capture multiple key variables
  key_expr <- rlang::enexpr(key)
  
  if (rlang::is_null(key_expr)) {
    stop("Supply at least one key variable.", call. = FALSE)
  }
  
  if (rlang::is_character(key_expr)) {
    key_names <- rlang::eval_tidy(key_expr, data = data)
  } else {
    key_names <- all.vars(key_expr)
  }
  
  if (length(key_names) == 0L || any(!nzchar(key_names))) {
    stop("Supply at least one valid key variable.", call. = FALSE)
  }
  
  if (!all(key_names %in% names(data))) {
    stop(
      "Unknown key variables: ",
      paste(setdiff(key_names, names(data)), collapse = ", "),
      call. = FALSE
    )
  }
  
  
  # Validate required columns
  required <- unique(c(time_name, x_name, y_name, key_names))
  missing_cols <- setdiff(required, names(data))
  
  if (length(missing_cols) > 0L) {
    stop(
      "Missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Validate time and coordinates
  if (!inherits(data[[time_name]], c("Date", "POSIXt"))) {
    stop(
      "`time` must be a Date or POSIXt column.",
      call. = FALSE
    )
  }
  
  if (!is.numeric(data[[x_name]]) ||
      !is.numeric(data[[y_name]])) {
    stop(
      "`x` and `y` must be numeric.",
      call. = FALSE
    )
  }
  
  if (anyNA(data[[time_name]])) {
    stop("`time` cannot contain missing values.", call. = FALSE)
  }
  
  if (anyNA(data[key_names])) {
    stop("Key variables cannot contain missing values.", call. = FALSE)
  }
  
  # Create spatial features; retain x and y columns
  out <- sf::st_as_sf(
    data,
    coords = c(x_name, y_name),
    crs = crs,
    remove = FALSE,
    ...
  )
  
  # Add tracking metadata
  class(out) <- unique(c("trackable", class(out)))
  
  attr(out, "trackable_time") <- time_name
  attr(out, "trackable_key") <- key_names
  attr(out, "trackable_coords") <- c(x = x_name, y = y_name)
  
  out
}

#'
#'library(trackable)
library(dplyr)
library(sf)
library(tibble)

set.seed(123)

dat <- tibble(
  animal_id = rep(c("E001", "E002", "E003"), each = 4),
  time = rep(
    as.POSIXct("2022-01-01 08:00:00", tz = "UTC") +
      86400 * 0:3,
    times = 3
  ),
  x = runif(12, 80.70, 80.90),
  y = runif(12, 7.80, 8.00),
  Length = rep(c(250, 270, 250), each = 4),
  Sex = rep(c("Female", "Male", "Female"), each = 4)
)

dat
trk <- trackable(
  data = dat,
  time = time,
  x = x,
  y = y,
  key = c("animal_id", "Length", "Sex"),
  crs = 4326
)

trk
