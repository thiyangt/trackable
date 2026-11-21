#' Identify Potential Outliers in Animal Tracking Data
#'
#' Identify potentially implausible GPS observations using
#' movement speeds calculated between consecutive locations.
#' Observations implying speeds above a specified threshold
#' are flagged for further investigation.
#'
#' @param x A `trackable` object.
#' @param by Optional character vector specifying grouping variables.
#'   Defaults to the tracking keys stored in `x`.
#' @param max_speed Maximum plausible movement speed in metres
#'   per second. Defaults to 10.
#' @param ... Currently unused.
#'
#' @return A tibble containing potentially anomalous observations,
#'   calculated step distances, elapsed times, speeds, and
#'   outlier indicators.
#'
#' @seealso [track_gaps()], [track_duplicates()],
#'   [track_validate()]
#'
#' @export
track_outliers <- function(x, by = NULL,
                           max_speed = 10, ...) {
  
  if (length(max_speed) != 1 ||
      is.na(max_speed) || max_speed <= 0) {
    stop("`max_speed` must be a positive value in m/s.",
         call. = FALSE)
  }
  
  meta <- .track_metadata(x)
  
  if (is.null(by)) {
    by <- meta$key
  }
  
  if (!all(by %in% names(x))) {
    stop("Grouping variables not found in `x`.", call. = FALSE)
  }
  
  time_col <- .track_time_column(x)
  groups <- .track_groups(x, by)
  
  result <- lapply(groups, function(i) {
    
    d <- x[i, ]
    d <- d[order(d[[time_col]]), ]
    
    if (nrow(d) < 2) return(NULL)
    
    elapsed <- .track_elapsed(d[[time_col]])
    distance <- .track_step_distance(d)
    
    speed <- distance / elapsed
    
    flagged <- c(FALSE, speed > max_speed)
    
    flagged[is.na(flagged)] <- FALSE
    
    if (!any(flagged)) return(NULL)
    
    tibble::tibble(
      sf::st_drop_geometry(d[flagged, by, drop = FALSE]),
      row = i[order(d[[time_col]])][flagged],
      timestamp = d[[time_col]][flagged],
      step_distance_m = c(NA_real_, distance)[flagged],
      elapsed_time_s = c(NA_real_, elapsed)[flagged],
      speed_m_s = c(NA_real_, speed)[flagged],
      outlier = flagged[flagged]
    )
  })
  
  dplyr::bind_rows(result)
}
