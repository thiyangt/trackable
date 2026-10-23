#' @export
fill_gaps.trackable <- function(
    .data,
    ...,
    .full = FALSE,
    .start = NULL,
    .end = NULL
) {
  stopifnot(inherits(.data, "trackable"))
  
  # Convert to tsibble
  ts <- tsibble::as_tsibble(
    .data$data,
    key = tidyselect::all_of(.data$key),
    index = !!rlang::sym(.data$time_name)
  )
  
  # Fill missing timestamps
  filled <- tsibble::fill_gaps(
    ts,
    ...,
    .full = .full,
    .start = .start,
    .end = .end
  )
  
  # Reconstruct trackable
  trackable(
    data = tibble::as_tibble(filled),
    time = !!rlang::sym(.data$time_name),
    x = !!rlang::sym(.data$x_name),
    y = !!rlang::sym(.data$y_name),
    key = .data$key,
    crs = .data$crs
  )
}