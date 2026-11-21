#' Forecast Using Similar Historical Locations
#'
#' @param .data A trackable object.
#' @param horizon Forecast horizon in hours.
#' @param radius Search radius in coordinate units.
#' @param lookback Historical search window in hours.
#' @param decay Spatial weighting parameter.
#'
#' @return Predicted animal location.
#' @export
track_forecast_weighted_locationmatch <- function(.data,
                                  horizon = 1,
                                  radius = 100,
                                  lookback = Inf,
                                  decay = 0.5) {
  
  if (!is.finite(decay) || decay <= 0) {
    stop("`decay` must be positive.")
  }
  
  matches <- .track_find_analogues(
    .data,
    horizon = horizon,
    radius = radius,
    lookback = lookback
  )
  
  # Spatial weights
  weights <- exp(-decay * matches$distance / radius)
  weights <- weights / sum(weights)
  
  last <- dplyr::slice_tail(
    .data |> dplyr::arrange(time),
    n = 1
  )
  
  tibble::tibble(
    animal_id = last$animal_id,
    time = last$time + horizon * 3600,
    x = sum(weights * matches$future_x),
    y = sum(weights * matches$future_y),
    n_matches = nrow(matches)
  )
}
