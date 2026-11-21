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
#' elephant_tibble <- tibble(
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
track_forecast_step <- function(.data,
                                n = 5,
                                horizon = 1) {
  
  if (length(horizon) != 1 || !is.finite(horizon) ||
      horizon <= 0) {
    stop("`horizon` must be positive.")
  }
  
  data <- .track_forecast_prepare(.data, n)
  
  dx <- diff(data$x)
  dy <- diff(data$y)
  
  dt <- as.numeric(
    diff(data$time), units = "hours"
  )
  
  # Step lengths and movement directions
  step_length <- sqrt(dx^2 + dy^2)
  
  bearing <- atan2(dy, dx)
  
  if (length(step_length) < 2) {
    stop("At least three observations are required.")
  }
  
  # Turning angles, wrapped to [-pi, pi]
  turning <- atan2(
    sin(diff(bearing)),
    cos(diff(bearing))
  )
  
  # Circular mean turning angle
  mean_turn <- atan2(
    mean(sin(turning)),
    mean(cos(turning))
  )
  
  # Mean step length per hour
  mean_step_rate <- mean(step_length / dt)
  
  # Last observed movement direction
  last_bearing <- tail(bearing, 1)
  
  # Forecast direction
  forecast_bearing <- last_bearing + mean_turn
  
  # Forecast displacement
  distance <- mean_step_rate * horizon
  
  last <- dplyr::slice_tail(data, n = 1)
  
  tibble::tibble(
    animal_id = last$animal_id,
    time = last$time + horizon * 3600,
    x = last$x + distance * cos(forecast_bearing),
    y = last$y + distance * sin(forecast_bearing)
  )
}
