#' Forecast the Next Location
#'
#' Forecast the next location of an animal using its recent
#' movement history and mean velocity.
#'
#' @param .data A trackable object.
#' @param n Number of most recent observations used for forecasting.
#' @param horizon Forecast horizon in hours. Defaults to 1.
#'
#' @return A tibble containing the predicted location and forecast time.
#'
#' @importFrom tibble tibble
#' @export
track_forecast_baseline <- function(.data, n = 3, horizon = 1) {
  
  if (n < 2) {
    stop("`n` must be at least 2.")
  }
  
  if (horizon <= 0) {
    stop("`horizon` must be greater than 0.")
  }
  
  # Extract trackable metadata
  time_col <- attr(.data, "trackable_time")
  key <- attr(.data, "trackable_key")
  coords <- attr(.data, "trackable_coords")
  
  if (is.null(time_col) || is.null(coords)) {
    stop("Time and coordinate metadata must be available.")
  }
  
  coord_cols <- unname(coords[c("x", "y")])
  
  if (length(coord_cols) != 2 || anyNA(coord_cols)) {
    stop("Coordinate metadata must identify x and y columns.")
  }
  
  # Extract data
  dat <- sf::st_drop_geometry(.data)
  
  required_cols <- c(time_col, key, coord_cols)
  missing_cols <- setdiff(required_cols, names(dat))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Forecast for one track
  forecast_one <- function(data) {
    
    data <- data |>
      dplyr::arrange(.data[[time_col]]) |>
      dplyr::slice_tail(n = n)
    
    if (nrow(data) < 2) {
      stop("At least two observations are required per track.")
    }
    
    dt <- as.numeric(
      difftime(
        data[[time_col]][-1],
        data[[time_col]][-nrow(data)],
        units = "hours"
      )
    )
    
    if (any(dt <= 0)) {
      stop("Observation times must be strictly increasing.")
    }
    
    vx <- diff(data[[coord_cols[1]]]) / dt
    vy <- diff(data[[coord_cols[2]]]) / dt
    
    mean_vx <- mean(vx, na.rm = TRUE)
    mean_vy <- mean(vy, na.rm = TRUE)
    
    last <- data[nrow(data), ]
    
    tibble::tibble(
      !!time_col := last[[time_col]] + horizon * 3600,
      !!coord_cols[1] := last[[coord_cols[1]]] + mean_vx * horizon,
      !!coord_cols[2] := last[[coord_cols[2]]] + mean_vy * horizon
    )
  }
  
  # Forecast each animal separately
  if (length(key) > 0) {
    
    dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
      dplyr::group_modify(~ forecast_one(.x)) |>
      dplyr::ungroup()
    
  } else {
    forecast_one(dat)
  }
}