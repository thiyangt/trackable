#' Forecast Location Using Weighted Movement Velocities
#'
#' Forecast the next location using a weighted average of
#' recent movement velocities. More recent movements receive
#' higher weights.
#'
#' @param .data A trackable object containing one animal's
#'   movement observations.
#' @param n Number of recent observations to use.
#' @param horizon Forecast horizon in hours.
#' @param decay Controls the rate at which weights decrease
#'   for older observations. Must be between 0 and 1.
#'
#' @return A tibble containing the forecast animal ID,
#'   timestamp, and coordinates.
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
#' track_forecast_smooth(elephant_track)
#'
#' @export
track_forecast_smooth <- function(.data,
                                  n = 5,
                                  horizon = 1,
                                  decay = 0.7) {
  
  if (length(horizon) != 1 || !is.finite(horizon) ||
      horizon <= 0) {
    stop("`horizon` must be positive.")
  }
  
  if (length(decay) != 1 || !is.finite(decay) ||
      decay <= 0 || decay > 1) {
    stop("`decay` must be greater than 0 and at most 1.")
  }
  
  data <- .track_forecast_prepare(.data, n)
  
  dt <- as.numeric(
    diff(data$time), units = "hours"
  )
  
  vx <- diff(data$x) / dt
  vy <- diff(data$y) / dt
  
  # Give more weight to recent velocities
  weights <- decay ^ rev(seq_along(vx) - 1)
  
  weights <- weights / sum(weights)
  
  # Weighted average velocity
  vx_hat <- sum(weights * vx)
  vy_hat <- sum(weights * vy)
  
  last <- dplyr::slice_tail(data, n = 1)
  
  tibble::tibble(
    animal_id = last$animal_id,
    time = last$time + horizon * 3600,
    x = last$x + vx_hat * horizon,
    y = last$y + vy_hat * horizon
  )
}
