#' Create a trackable object
#'
#' @description
#' Creates a `trackable` object for tidy spatio-temporal tracking data.
#' A `trackable` object extends the `sf` class and combines individual,
#' temporal, and spatial dimensions.
#'
#' @param data A data frame containing tracking observations.
#' @param animal A variable identifying the tracked individual.
#' @param time A variable containing the observation time.
#' @param geometry An existing `sf` geometry column, or `NULL`.
#' @param longitude Longitude variable used when `geometry` is `NULL`.
#' @param latitude Latitude variable used when `geometry` is `NULL`.
#' @param location An optional variable identifying the location.
#' @param animal_group An optional variable identifying an animal group.
#' @param crs Coordinate reference system used to create the geometry.
#'
#' @return An object of class `trackable` inheriting from `sf`.
#' @export
trackable <- function(data,
                      animal,
                      time,
                      geometry = NULL,
                      longitude = NULL,
                      latitude = NULL,
                      location = NULL,
                      animal_group = NULL,
                      crs = 4326) {
  
  # Capture arguments before modifying data
  animal_quo <- rlang::enquo(animal)
  time_quo <- rlang::enquo(time)
  
  location_quo <- rlang::enquo(location)
  animal_group_quo <- rlang::enquo(animal_group)
  
  longitude_quo <- rlang::enquo(longitude)
  latitude_quo <- rlang::enquo(latitude)
  
  # ---- Required variables ----
  
  data <- dplyr::rename(
    data,
    animal = !!animal_quo,
    time = !!time_quo
  )
  
  # ---- Optional variables ----
  
  if (rlang::quo_is_null(location_quo)) {
    data$location <- NA_character_
  } else {
    data <- dplyr::rename(
      data,
      location = !!location_quo
    )
  }
  
  if (rlang::quo_is_null(animal_group_quo)) {
    data$animal_group <- NA_character_
  } else {
    data <- dplyr::rename(
      data,
      animal_group = !!animal_group_quo
    )
  }
  
  # ---- Geometry ----
  
  if (inherits(data, "sf")) {
    
    # Existing sf object
    data <- sf::st_as_sf(data)
    
  } else if (!is.null(geometry)) {
    
    geometry_quo <- rlang::enquo(geometry)
    
    data <- sf::st_as_sf(
      data,
      sf_column_name = rlang::as_name(geometry_quo)
    )
    
  } else {
    
    if (
      rlang::quo_is_null(longitude_quo) ||
      rlang::quo_is_null(latitude_quo)
    ) {
      rlang::abort(
        "Provide either an sf geometry or longitude and latitude."
      )
    }
    
    data <- dplyr::rename(
      data,
      longitude = !!longitude_quo,
      latitude = !!latitude_quo
    )
    
    data <- sf::st_as_sf(
      data,
      coords = c("longitude", "latitude"),
      crs = crs,
      remove = FALSE
    )
  }
  
  # ---- Key ----
  
  data <- data |>
    dplyr::arrange(animal, time) |>
    dplyr::mutate(
      key = animal
    )
  
  # ---- Trackable class ----
  
  new_trackable(data)
}
