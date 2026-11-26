#' Forecast Using a Historical Analogue Model
#'
#' @param .data A trackable object.
#' @param horizon Forecast horizon in hours.
#' @param radius Search radius in coordinate units.
#' @param lookback Historical search window in hours.
#'
#' @return Predicted animal location.
#' @export
track_forecast_regression_locationmatch <- function(
    .data,
    horizon = 1,
    radius = 0.1,
    lookback = Inf,
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
  
  data <- .data |>
    dplyr::transmute(
      animal_id = .data[[id]],
      time = .data[[time]],
      x = .data[[x]],
      y = .data[[y]]
    ) |>
    dplyr::arrange(time)
  
  # Current location
  current <- dplyr::slice_tail(data, n = 1)
  
  # Historical location matching
  history <- data |>
    dplyr::filter(time < current$time) |>
    dplyr::mutate(
      distance = sqrt(
        (x - current$x)^2 +
          (y - current$y)^2
      )
    ) |>
    dplyr::filter(
      distance <= radius,
      time >= current$time - lookback * 3600
    )
  
  # Find corresponding future locations
  matches <- lapply(seq_len(nrow(history)), function(i) {
    
    target <- history$time[i] + horizon * 3600
    
    difference <- abs(
      as.numeric(
        difftime(data$time, target, units = "hours")
      )
    )
    
    j <- which.min(difference)
    
    if (difference[j] > 0.25) return(NULL)
    
    tibble::tibble(
      x = history$x[i],
      y = history$y[i],
      future_x = data$x[j],
      future_y = data$y[j]
    )
    
  }) |>
    dplyr::bind_rows()
  
  if (nrow(matches) < 3) {
    stop("At least three historical analogues are required.")
  }
  
  # Regression models
  model_x <- stats::lm(future_x ~ x + y, data = matches)
  model_y <- stats::lm(future_y ~ x + y, data = matches)
  
  newdata <- data.frame(
    x = current$x,
    y = current$y
  )
  
  tibble::tibble(
    animal_id = current$animal_id,
    time = current$time + horizon * 3600,
    x = as.numeric(stats::predict(model_x, newdata)),
    y = as.numeric(stats::predict(model_y, newdata)),
    n_matches = nrow(matches)
  )
}