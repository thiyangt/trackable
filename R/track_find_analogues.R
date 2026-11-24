track_find_analogues <- function(.data,
                                  horizon = 1,
                                  radius = 100,
                                  time_tolerance = 0.25,
                                  lookback = Inf) {
  
  data <- .track_forecast_prepare(
    .data,
    n = nrow(.data)
  )
  
  current <- dplyr::slice_tail(data, n = 1)
  
  # Historical observations only
  history <- data |>
    dplyr::filter(
      time < current$time,
      time >= current$time - lookback * 3600
    )
  
  if (nrow(history) == 0) {
    stop("No historical observations available.")
  }
  
  # Distance from historical locations to current location
  history <- history |>
    dplyr::mutate(
      distance = sqrt(
        (x - current$x)^2 +
          (y - current$y)^2
      )
    ) |>
    dplyr::filter(distance <= radius)
  
  if (nrow(history) == 0) {
    stop("No historical locations found within `radius`.")
  }
  
  # Find the location approximately horizon hours
  # after each historical matching location
  history$target_time <- history$time + horizon * 3600
  
  future <- data |>
    dplyr::select(
      future_time = time,
      future_x = x,
      future_y = y
    )
  
  matches <- lapply(seq_len(nrow(history)), function(i) {
    
    difference <- abs(
      as.numeric(
        difftime(
          future$future_time,
          history$target_time[i],
          units = "hours"
        )
      )
    )
    
    j <- which.min(difference)
    
    if (length(j) == 0 ||
        difference[j] > time_tolerance) {
      return(NULL)
    }
    
    tibble::tibble(
      distance = history$distance[i],
      future_x = future$future_x[j],
      future_y = future$future_y[j]
    )
  })
  
  dplyr::bind_rows(matches)
}
