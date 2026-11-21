#' Forecast Using a Historical Analogue Model
#'
#' @param .data A trackable object.
#' @param horizon Forecast horizon in hours.
#' @param radius Search radius in coordinate units.
#' @param lookback Historical search window in hours.
#'
#' @return Predicted animal location.
#' @export
track_forecast_regression_locationmatch <- function(.data,
                                 horizon = 1,
                                 radius = 100,
                                 lookback = Inf) {
  
  data <- .track_forecast_prepare(
    .data,
    n = nrow(.data)
  )
  
  current <- dplyr::slice_tail(data, n = 1)
  
  # Historical matching observations
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
  
  # Find future coordinates for each match
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
