#' Forecast Location Using Step Length and Turning Angle
#'
#' Forecast the next location using recent step lengths
#' and turning angles.
#'
#' @param .data A trackable object containing one animal's
#'   movement observations.
#' @param n Number of recent observations to use.
#' @param horizon Forecast horizon in hours.
#'
#' @return A tibble containing the forecast animal ID,
#'   timestamp, and coordinates.
#'
#' @details
#' The method estimates the mean step length and circular
#' mean turning angle from recent movements. The mean step
#' length is scaled by the forecast horizon relative to the
#' average observation interval.
#' 
#' @examples
#' elephant_tibble <- tibble::tibble(
#' animal_id = "E001",
#' time = as.POSIXct("2026-11-18 06:00:00") +
#'    (0:4) * 3600,
#'  x = c(500000, 500120, 500260, 500390, 500530),
#'  y = c(850000, 850040, 850090, 850130, 850180)
#' )
#' 
#' elephant_track <- trackable(data = elephant_tibble,
#' time = time, x=x, y=y, crs = 4326, key = animal_id
#' )
#' 
#' track_forecast_step(elephant_track)
#'
#' @export
track_forecast_step <- function(
    .data,
    n = 5,
    horizon = 1
) {
  
  if (length(horizon) != 1 ||
      !is.finite(horizon) || horizon <= 0)
    stop("`horizon` must be positive.")
  
  if (length(n) != 1 ||
      !is.finite(n) || n < 3)
    stop("`n` must be at least 3.")
  
  # Extract attributes
  data <- if (inherits(.data, "sf")) {
    sf::st_drop_geometry(.data)
  } else {
    tibble::as_tibble(.data)
  }
  
  # Identify columns
  id_col <- intersect(
    c("animal_id", "elephant"),
    names(data)
  )[1]
  
  time_col <- intersect(
    c("time", "datetime"),
    names(data)
  )[1]
  
  x_col <- intersect(
    c("x", "lon", "longitude"),
    names(data)
  )[1]
  
  y_col <- intersect(
    c("y", "lat", "latitude"),
    names(data)
  )[1]
  
  if (any(is.na(c(id_col, time_col, x_col, y_col))))
    stop("Required animal, time, or coordinate columns are missing.")
  
  # Standardise column names
  data <- data |>
    dplyr::rename(
      animal_id = dplyr::all_of(id_col),
      time = dplyr::all_of(time_col),
      x = dplyr::all_of(x_col),
      y = dplyr::all_of(y_col)
    )
  
  # Select the most recently observed animal
  selected_id <- data$animal_id[
    which.max(data$time)
  ]
  
  data <- data |>
    dplyr::filter(.data$animal_id == selected_id) |>
    dplyr::arrange(.data$time) |>
    dplyr::slice_tail(n = n)
  
  if (nrow(data) < 3)
    stop("At least three observations are required for this animal.")
  
  if (anyNA(data[, c("time", "x", "y")]))
    stop("Missing time or coordinate values are not allowed.")
  
  # Time differences in hours
  dt <- as.numeric(
    difftime(
      data$time[-1],
      data$time[-nrow(data)],
      units = "hours"
    )
  )
  
  if (any(!is.finite(dt)) || any(dt <= 0))
    stop("Observation times must be strictly increasing.")
  
  # Calculate movement steps
  dx <- diff(data$x)
  dy <- diff(data$y)
  
  step_length <- sqrt(dx^2 + dy^2)
  
  bearing <- atan2(dy, dx)
  
  # Turning angles
  turning <- atan2(
    sin(diff(bearing)),
    cos(diff(bearing))
  )
  
  # Circular mean turning angle
  mean_turn <- atan2(
    mean(sin(turning)),
    mean(cos(turning))
  )
  
  # Mean movement rate
  mean_step_rate <- mean(step_length / dt)
  
  # Last observed direction
  last_bearing <- tail(bearing, 1)
  
  # Forecast direction
  forecast_bearing <- last_bearing + mean_turn
  
  # Forecast distance
  distance <- mean_step_rate * horizon
  
  last <- data[nrow(data), ]
  
  tibble::tibble(
    animal_id = last$animal_id,
    time = last$time + horizon * 3600,
    x = last$x + distance * cos(forecast_bearing),
    y = last$y + distance * sin(forecast_bearing)
  )
}
