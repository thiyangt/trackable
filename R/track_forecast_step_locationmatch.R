#' Forecast Animal Location Using Historical Step Displacements
#'
#' Identifies historical observations at locations close to the animal's
#' current position and uses their subsequent movement to forecast its
#' future location. For each historical observation, the function finds
#' the GPS observation closest to the specified forecast horizon and
#' calculates the corresponding displacement in the x and y directions.
#' The median historical displacement is then added to the animal's
#' current location to obtain the forecast.
#'
#' If no historical matches are found within the initial search radius,
#' the function progressively increases the radius until matches are
#' found or the maximum search radius is reached.
#'
#' @param .data A data frame containing animal movement observations.
#' @param horizon Forecast horizon in hours.
#' @param radius Initial spatial search radius, in the same units as the
#'   coordinate variables.
#' @param lookback Historical search window in hours. Defaults to `Inf`,
#'   which considers all available historical observations.
#' @param tolerance Maximum acceptable difference, in hours, between the
#'   target forecast time and the nearest observed timestamp.
#' @param max_radius Maximum spatial search radius used when expanding the
#'   initial search radius.
#' @param id Name of the animal identifier column.
#' @param time Name of the timestamp column.
#' @param x Name of the x-coordinate column.
#' @param y Name of the y-coordinate column.
#'
#' @return A tibble containing the animal identifier, forecast timestamp,
#'   predicted x and y coordinates, number of historical matches, and
#'   spatial search radius used.
#'
#' @details
#' Historical observations are selected based on their spatial proximity
#' to the animal's most recent recorded location and their occurrence
#' within the specified lookback period.
#'
#' For each selected observation at time \(t_i\), the function identifies
#' the nearest observation to \(t_i + h\), where \(h\) is the forecast
#' horizon. Historical displacement vectors are calculated as
#'
#' \deqn{
#' \Delta x_i = x_{t_i+h} - x_{t_i}, \qquad
#' \Delta y_i = y_{t_i+h} - y_{t_i}.
#' }
#'
#' The median displacement components are used to forecast the next
#' location:
#'
#' \deqn{
#' \hat{x}_{t+h} = x_t + \mathrm{median}(\Delta x_i),
#' }
#' \deqn{
#' \hat{y}_{t+h} = y_t + \mathrm{median}(\Delta y_i).
#' }
#'
#' The search radius is progressively expanded when no historical
#' matches are available within the initial radius. The resulting
#' `search_radius` and `n_matches` values provide information about the
#' historical observations used for the forecast.
#'
#' Coordinates should use a suitable projected coordinate reference
#' system so that spatial distances are meaningful. The input should
#' contain observations for a single animal.
#'
#' @examples
#' \dontrun{
#' track_forecast_step_locationmatch(
#'   trk_ele,
#'   horizon = 1,
#'   radius = 0.1,
#'   lookback = 168,
#'   tolerance = 0.5,
#'   max_radius = 5
#' )
#' }
#'
#' @export
track_forecast_step_locationmatch <- function(
    .data,
    horizon = 1,
    radius = 0.1,
    lookback = Inf,
    tolerance = 0.5,
    max_radius = 5,
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
  if (!is.numeric(horizon) || length(horizon) != 1 ||
      !is.finite(horizon) || horizon <= 0) {
    stop("`horizon` must be positive.")
  }
  
  if (!is.numeric(radius) || length(radius) != 1 ||
      !is.finite(radius) || radius <= 0) {
    stop("`radius` must be positive.")
  }
  
  if (!is.numeric(max_radius) || length(max_radius) != 1 ||
      !is.finite(max_radius) || max_radius < radius) {
    stop("`max_radius` must be greater than or equal to `radius`.")
  }
  
  if (!is.numeric(tolerance) || length(tolerance) != 1 ||
      !is.finite(tolerance) || tolerance < 0) {
    stop("`tolerance` must be non-negative.")
  }
  
  if (length(lookback) != 1 || is.na(lookback) ||
      lookback <= 0) {
    stop("`lookback` must be positive or Inf.")
  }
  
  # Prepare data
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
    stop("`time` must be POSIXct.")
  }
  
  if (anyNA(data)) {
    stop("Missing values are not allowed.")
  }
  
  if (nrow(data) < 2) {
    stop("At least two observations are required.")
  }
  
  # Current location
  last <- dplyr::slice_tail(data, n = 1)
  
  # Historical observations
  history <- data |>
    dplyr::filter(
      time < last$time,
      time >= last$time - lookback * 3600
    ) |>
    dplyr::mutate(
      distance = sqrt(
        (x - last$x)^2 +
          (y - last$y)^2
      )
    )
  
  # Match future locations
  find_matches <- function(search_radius) {
    
    candidates <- history |>
      dplyr::filter(distance <= search_radius)
    
    matches <- lapply(seq_len(nrow(candidates)), function(i) {
      
      target_time <- candidates$time[i] + horizon * 3600
      
      differences <- abs(
        as.numeric(
          difftime(data$time, target_time, units = "hours")
        )
      )
      
      j <- which.min(differences)
      
      if (differences[j] > tolerance) {
        return(NULL)
      }
      
      tibble::tibble(
        dx = data$x[j] - candidates$x[i],
        dy = data$y[j] - candidates$y[i]
      )
      
    }) |>
      dplyr::bind_rows()
    
    matches
  }
  
  # Expand search radius when necessary
  search_radius <- radius
  matches <- find_matches(search_radius)
  
  while (nrow(matches) == 0 &&
         search_radius < max_radius) {
    
    search_radius <- min(
      search_radius * 2,
      max_radius
    )
    
    matches <- find_matches(search_radius)
  }
  
  if (nrow(matches) == 0) {
    stop(
      "No historical movement matches found, even at ",
      max_radius, " coordinate units."
    )
  }
  
  # Median historical displacement
  dx_hat <- stats::median(matches$dx)
  dy_hat <- stats::median(matches$dy)
  
  # Forecast location
  tibble::tibble(
    animal_id = last$animal_id,
    time = last$time + horizon * 3600,
    x = last$x + dx_hat,
    y = last$y + dy_hat,
    n_matches = nrow(matches),
    search_radius = search_radius
  )
}
