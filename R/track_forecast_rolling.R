#' Generate Rolling Forecasts of Animal Movement
#'
#' Applies a forecasting method repeatedly to successive training datasets
#' to generate rolling forecasts of animal movement. At each forecasting
#' origin, the specified forecasting method is applied to the observations
#' available up to that point.
#'
#' The forecasting method is supplied as a function, allowing different
#' movement forecasting approaches to be used within the same rolling
#' forecasting framework.
#'
#' @param .data A `trackable` object or data frame containing animal
#'   movement observations. Must include `animal_id`, `time`, `x`,
#'   and `y` columns.
#' @param forecast_method A forecasting function used to generate
#'   predictions. Defaults to [track_forecast_baseline()].
#'   The function must accept the training data as its first argument
#'   and return a data frame or tibble containing the predicted
#'   `animal_id`, `time`, `x`, and `y` values.
#' @param n An integer specifying the number of most recent observations
#'   used by the forecasting method. Defaults to 3. The interpretation
#'   of this argument depends on the supplied forecasting method.
#' @param horizon A positive numeric value specifying the forecasting
#'   horizon in hours. Defaults to 1.
#' @param ... Additional arguments passed to `forecast_method`.
#'
#' @return A tibble containing the rolling forecasts generated at each
#'   forecasting origin. The output includes the predicted animal
#'   identifier, forecast time, predicted coordinates, and
#'   `forecast_origin`, indicating the time of the last observation
#'   available when each forecast was generated.
#'
#' @details
#' The function uses an expanding training dataset. At each iteration,
#' the training dataset includes all observations available up to the
#' current forecasting origin. The forecasting method determines how
#' these observations are used to generate predictions.
#'
#' For example, [track_forecast_baseline()] uses the most recent `n`
#' observations to estimate average movement velocity before
#' extrapolating the animal's location.
#'
#' This design allows different forecasting methods to be evaluated
#' using a common rolling forecasting framework.
#'
#' @seealso [track_forecast_baseline()]
#'
#' @examples
#' data <- tibble::tibble(
#'   animal_id = "E001",
#'   time = seq(
#'     as.POSIXct("2022-01-01", tz = "UTC"),
#'     by = "hour",
#'     length.out = 8
#'   ),
#'   x = c(80.70, 80.72, 80.75, 80.79,
#'         80.82, 80.84, 80.87, 80.90),
#'   y = c(7.80, 7.81, 7.83, 7.84,
#'         7.86, 7.87, 7.89, 7.90)
#' )
#'
#' # Generate rolling baseline forecasts
#' track_forecast_rolling(
#'   data,
#'   forecast_method = track_forecast_baseline,
#'   n = 3,
#'   horizon = 1
#' )
#'
#' @export
track_forecast_rolling <- function(
    .data,
    forecast_method = track_forecast_baseline,
    n = 3,
    horizon = 1,
    ...
) {
  
  if (n < 2) {
    stop("At least two observations are required.")
  }
  
  if (nrow(.data) <= n) {
    stop("Insufficient observations for rolling forecasting.")
  }
  
  .data <- .data |>
    dplyr::arrange(time)
  
  results <- vector("list", nrow(.data) - n)
  
  for (i in seq_along(results)) {
    
    # Expanding training data
    train_data <- .data |>
      dplyr::slice_head(n = n + i - 1)
    
    # Apply forecasting method
    results[[i]] <- forecast_method(
      train_data,
      n = n,
      horizon = horizon,
      ...
    ) |>
      dplyr::mutate(
        forecast_origin = dplyr::last(train_data$time)
      )
  }
  
  dplyr::bind_rows(results)
}
