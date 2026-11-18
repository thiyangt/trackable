#' Forecast Using Historical Movement Steps
#'
#' @param .data A trackable object.
#' @param horizon Forecast horizon in hours.
#' @param radius Search radius in coordinate units.
#' @param lookback Historical search window in hours.
#'
#' @return Predicted animal location.
#' @export
track_forecast_step_locationmatch <- function(.data,
                                horizon = 1,
                                radius = 100,
                                lookback = Inf) {
  
  matches <- .track_find_analogues(
    .data,
    horizon = horizon,
    radius = radius,
    lookback = lookback
  )
  
  last <- dplyr::slice_tail(
    .data |> dplyr::arrange(time),
    n = 1
  )
  
  # Historical displacement vectors
  dx <- matches$future_x -
    (last$x + 0 * matches$distance)
  
  dy <- matches$future_y -
    (last$y + 0 * matches$distance)
  
  tibble::tibble(
    animal_id = last$animal_id,
    time = last$time + horizon * 3600,
    x = last$x + median(dx),
    y = last$y + median(dy),
    n_matches = nrow(matches)
  )
}
