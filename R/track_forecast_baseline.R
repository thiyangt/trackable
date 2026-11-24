#' Forecast the Next Location
#'
#' Forecast the next location of an animal using its recent
#' movement history.
#'
#' @param .data A trackable object.
#' @param n Number of most recent observations used for forecasting.
#' @param horizon Forecast horizon in hours. Defaults to 1.
#' @importFrom tibble tibble
#'
#' @return A tibble containing the predicted location and forecast time.
#'
#' @examples
#' elephant_tibble <- tibble(
#' animal_id = "E001",
#' time = as.POSIXct("2026-11-18 06:00:00") +
#'    (0:4) * 3600,
#'  x = c(500000, 500120, 500260, 500390, 500530),
#'  y = c(850000, 850040, 850090, 850130, 850180)
#' )
#' elephant_track <- trackable(data = elephant_tibble,
#' time = time, x=x, y=y, crs = 4326, key = animal_id
#' )
#' 
#' track_forecast_baseline(elephant_track)
#'
#' @export
track_forecast_baseline <- function(.data, n = 3, horizon = 1) {
  
  if (n < 2) {
    stop("At least two observations are required.")
  }
  
  if (horizon <= 0) {
    stop("`horizon` must be positive.")
  }
  
  data <- .data |>
    dplyr::arrange(time) |>
    dplyr::slice_tail(n = n)
  
  if (nrow(data) < 2) {
    stop("Insufficient observations for forecasting.")
  }
  
  # Calculate movement rates
  dt <- as.numeric(
    difftime(data$time[-1],
             data$time[-nrow(data)],
             units = "hours")
  )
  
  if (any(dt <= 0)) {
    stop("Timestamps must be strictly increasing.")
  }
  
  vx <- diff(data$x) / dt
  vy <- diff(data$y) / dt
  
  # Average recent movement velocity
  mean_vx <- mean(vx)
  mean_vy <- mean(vy)
  
  # Current location
  last <- dplyr::slice_tail(data, n = 1)
  
  # Forecast next location
  tibble::tibble(
    animal_id = last$animal_id,
    time = last$time + horizon * 3600,
    x = last$x + mean_vx * horizon,
    y = last$y + mean_vy * horizon
  )
}
