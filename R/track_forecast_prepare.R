# Internal helper to prepare movement data
track_forecast_prepare <- function(.data, n = 5) {
  
  required <- c("animal_id", "time", "x", "y")
  
  missing <- setdiff(required, names(.data))
  
  if (length(missing) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }
  
  if (length(unique(.data$animal_id)) != 1) {
    stop("Please supply data for one animal at a time.")
  }
  
  if (n < 2 || n != as.integer(n)) {
    stop("`n` must be an integer greater than or equal to 2.")
  }
  
  data <- .data |>
    dplyr::arrange(.data$time) |>
    dplyr::filter(
      is.finite(.data$x),
      is.finite(.data$y),
      !is.na(.data$time)
    ) |>
    dplyr::slice_tail(n = n)
  
  if (nrow(data) < 2) {
    stop("At least two valid observations are required.")
  }
  
  if (anyDuplicated(data$time)) {
    stop("Timestamps must be unique.")
  }
  
  dt <- as.numeric(diff(data$time), units = "hours")
  
  if (any(!is.finite(dt)) || any(dt <= 0)) {
    stop("Timestamps must be strictly increasing.")
  }
  
  data
}
