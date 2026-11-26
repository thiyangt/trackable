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
#' @param id Name of the animal identifier column.
#' @param time Name of the datetime column.
#' @param x Name of the x-coordinate column.
#' @param y Name of the y-coordinate column.
#'
#' @return A tibble containing the forecast animal ID,
#'   timestamp, and coordinates.
#'
#' @export
track_forecast_smooth <- function(
    .data,
    n = 5,
    horizon = 1,
    decay = 0.7,
    id = "elephant",
    time = "datetime",
    x = "x_km",
    y = "y_km"
) {
  
  # Validate columns
  required_cols <- c(id, time, x, y)
  
  missing_cols <- setdiff(required_cols, names(.data))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Validate parameters
  if (length(n) != 1 || !is.finite(n) ||
      n < 2 || n != as.integer(n)) {
    stop("`n` must be an integer greater than or equal to 2.")
  }
  
  if (length(horizon) != 1 || !is.finite(horizon) ||
      horizon <= 0) {
    stop("`horizon` must be positive.")
  }
  
  if (length(decay) != 1 || !is.finite(decay) ||
      decay <= 0 || decay > 1) {
    stop("`decay` must be greater than 0 and at most 1.")
  }
  
  # Prepare data and remove duplicate timestamps
  data <- .data |>
    dplyr::transmute(
      animal_id = .data[[id]],
      time = .data[[time]],
      x = .data[[x]],
      y = .data[[y]]
    ) |>
    dplyr::arrange(time) |>
    dplyr::distinct(time, .keep_all = TRUE)
  
  if (!inherits(data$time, "POSIXct")) {
    stop("`time` must be a POSIXct datetime variable.")
  }
  
  if (anyNA(data)) {
    stop("Missing values are not allowed.")
  }
  
  if (nrow(data) < n) {
    stop("Insufficient unique timestamps for the specified `n`.")
  }
  
  # Select the most recent observations
  data <- data |>
    dplyr::slice_tail(n = n)
  
  # Calculate time differences in hours
  dt <- as.numeric(
    difftime(
      data$time[-1],
      data$time[-nrow(data)],
      units = "hours"
    )
  )
  
  if (any(dt <= 0)) {
    stop("Observation times must be strictly increasing.")
  }
  
  # Calculate movement velocities
  vx <- diff(data$x) / dt
  vy <- diff(data$y) / dt
  
  # Assign higher weights to recent velocities
  weights <- decay ^ rev(seq_along(vx) - 1)
  
  weights <- weights / sum(weights)
  
  # Weighted average velocities
  vx_hat <- sum(weights * vx)
  vy_hat <- sum(weights * vy)
  
  # Current location
  last <- dplyr::slice_tail(data, n = 1)
  
  # Forecast future location
  tibble::tibble(
    animal_id = last$animal_id,
    time = last$time + horizon * 3600,
    x = last$x + vx_hat * horizon,
    y = last$y + vy_hat * horizon
  )
}
