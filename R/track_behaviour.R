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
                            ...) {
  
  stopifnot(inherits(x, "trackable"))
  
  if (stop_threshold >= travel_threshold) {
    stop("`stop_threshold` must be smaller than ",
         "`travel_threshold`.")
  }
  
  dat <- .track_metrics(x)
  
  dat |>
    dplyr::mutate(
      behaviour = dplyr::case_when(
        is.na(.speed_m_s) ~ NA_character_,
        .speed_m_s <= stop_threshold ~ "stationary",
        .speed_m_s < travel_threshold ~ "intermediate",
        .speed_m_s >= travel_threshold ~ "travelling"
      )
    ) |>
    dplyr::rename(
      speed_m_s = .speed_m_s,
      distance_m = .distance_m
    ) |>
    tibble::as_tibble()
}
