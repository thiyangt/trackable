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
