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
track_forecast_weighted_locationmatch <- function(
    .data,
    horizon = 1,
    radius = 0.1,
    lookback = Inf,
    decay = 0.5
) {
  
  # Validation
  if (horizon <= 0 || !is.finite(horizon))
    stop("`horizon` must be positive.")
  
  if (radius <= 0 || !is.finite(radius))
    stop("`radius` must be positive.")
  
  if (decay <= 0 || !is.finite(decay))
    stop("`decay` must be positive.")
  
  if (lookback <= 0 || is.na(lookback))
    stop("`lookback` must be positive.")
  
  # Extract attributes
  dat <- if (inherits(.data, "sf")) {
    sf::st_drop_geometry(.data)
  } else {
    tibble::as_tibble(.data)
  }
  
  required <- c("elephant", "datetime", "lon", "lat")
  
  if (!all(required %in% names(dat)))
    stop("Data must contain elephant, datetime, lon, and lat.")
  
  dat <- dat |>
    dplyr::arrange(.data$datetime)
  
  last <- dat[nrow(dat), ]
  
  current_time <- last$datetime[[1]]
  current_lon <- last$lon[[1]]
  current_lat <- last$lat[[1]]
  
  # Historical observations
  historical <- dat |>
    dplyr::filter(
      .data$datetime < current_time,
      .data$datetime >= current_time - lookback * 3600
    )
  
  if (nrow(historical) == 0)
    stop("No historical observations found.")
  
  # Haversine distance in kilometres
  haversine <- function(lon1, lat1, lon2, lat2) {
    
    rad <- pi / 180
    
    lon1 <- lon1 * rad
    lat1 <- lat1 * rad
    lon2 <- lon2 * rad
    lat2 <- lat2 * rad
    
    a <- sin((lat2 - lat1) / 2)^2 +
      cos(lat1) * cos(lat2) *
      sin((lon2 - lon1) / 2)^2
    
    6371 * 2 * asin(pmin(1, sqrt(a)))
  }
  
  historical <- historical |>
    dplyr::mutate(
      distance = haversine(
        .data$lon,
        .data$lat,
        current_lon,
        current_lat
      )
    ) |>
    dplyr::filter(.data$distance <= radius)
  
  # Find subsequent GPS observations
  future <- dat |>
    dplyr::transmute(
      target_time = .data$datetime,
      future_lon = .data$lon,
      future_lat = .data$lat
    )
  
  matches <- historical |>
    dplyr::mutate(
      target_time = .data$datetime + horizon * 3600
    ) |>
    dplyr::left_join(future, by = "target_time") |>
    dplyr::filter(
      is.finite(.data$future_lon),
      is.finite(.data$future_lat)
    )
  
  if (nrow(matches) == 0)
    stop("No historical movement matches were found.")
  
  # Spatial weights
  weights <- exp(-decay * matches$distance / radius)
  weights <- weights / sum(weights)
  
  # Weighted forecast
  tibble::tibble(
    elephant = last$elephant[[1]],
    datetime = current_time + horizon * 3600,
    lon = sum(weights * matches$future_lon),
    lat = sum(weights * matches$future_lat),
    n_matches = nrow(matches)
  )
}
