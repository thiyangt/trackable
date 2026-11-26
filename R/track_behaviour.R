#' Classify candidate animal movement states
#'
#' Assign candidate movement states to individual tracking
#' observations using speed thresholds.
#'
#' @param x A `trackable` object.
#' @param stop_threshold Maximum speed (metres per second) for
#'   classifying an observation as stationary. Defaults to 0.2.
#' @param travel_threshold Minimum speed (metres per second)
#'   for classifying an observation as travelling. Defaults to 1.
#' @param time Optional character string specifying the name of
#'   the time column. Defaults to the time index stored in the
#'   `trackable` object.
#' @param coords Optional character vector specifying the names
#'   of the two coordinate columns (longitude and latitude).
#'   Defaults to the coordinate columns stored in the
#'   `trackable` object.
#' @param ... Additional arguments.
#'
#' @return A tibble containing the original tracking observations,
#'   calculated speed, and a candidate movement-state classification.
#'
#' @details
#' Three candidate movement states are considered:
#' \itemize{
#'   \item `stationary`: speed at or below `stop_threshold`.
#'   \item `intermediate`: speed between the two thresholds.
#'   \item `travelling`: speed at or above `travel_threshold`.
#' }
#'
#' These classifications describe movement patterns rather than
#' confirmed ecological behaviours. For example, stationary
#' observations cannot independently establish resting, feeding,
#' or sleeping.
#'
#' @examples
#' # track_behaviour(elephant_tracks)
#' # track_behaviour(elephant_tracks,
#' #                 stop_threshold = 0.1,
#' #                 travel_threshold = 1.5)
#'
#' @export
track_behaviour <- function(x,
                            stop_threshold = 0.2,
                            travel_threshold = 1,
                            time = NULL,
                            coords = NULL,
                            ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  # Validate thresholds
  if (length(stop_threshold) != 1L ||
      length(travel_threshold) != 1L ||
      !is.finite(stop_threshold) ||
      !is.finite(travel_threshold) ||
      stop_threshold < 0 ||
      stop_threshold >= travel_threshold) {
    stop(
      "`stop_threshold` must be non-negative and smaller than ",
      "`travel_threshold`."
    )
  }
  
  # Extract trackable metadata
  if (is.null(time)) {
    time <- attr(x, "trackable_time")
  }
  
  key <- attr(x, "trackable_key")
  coords_meta <- attr(x, "trackable_coords")
  
  if (is.null(coords)) {
    coords <- unname(coords_meta[c("x", "y")])
  }
  
  if (is.null(key)) {
    key <- character()
  }
  
  # Extract data
  dat <- tibble::as_tibble(
    sf::st_drop_geometry(x)
  )
  
  # Validate columns
  required <- c(time, key, coords)
  
  missing_cols <- setdiff(required, names(dat))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (length(coords) != 2L) {
    stop("`coords` must contain two coordinate column names.")
  }
  
  # Calculate movement metrics
  dat <- dat |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(key)),
      .data[[time]]
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(key))
    ) |>
    dplyr::mutate(
      .previous_time = dplyr::lag(.data[[time]]),
      
      elapsed_s = as.numeric(
        difftime(
          .data[[time]],
          .previous_time,
          units = "secs"
        )
      ),
      
      distance_m = geosphere::distHaversine(
        cbind(
          dplyr::lag(.data[[coords[1]]]),
          dplyr::lag(.data[[coords[2]]])
        ),
        cbind(
          .data[[coords[1]]],
          .data[[coords[2]]]
        )
      ),
      
      distance_m = dplyr::if_else(
        is.na(elapsed_s) | elapsed_s <= 0,
        NA_real_,
        distance_m
      ),
      
      speed_m_s = distance_m / elapsed_s
    ) |>
    dplyr::ungroup()
  
  # Classify movement behaviour
  dat |>
    dplyr::mutate(
      behaviour = dplyr::case_when(
        is.na(speed_m_s) ~ NA_character_,
        speed_m_s <= stop_threshold ~ "stationary",
        speed_m_s < travel_threshold ~ "intermediate",
        speed_m_s >= travel_threshold ~ "travelling"
      )
    ) |>
    tibble::as_tibble()
}