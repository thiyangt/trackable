#' Identify Gaps in Animal Tracking Data
#'
#' Identify missing or unusually long time intervals between
#' consecutive observations for each animal or group.
#'
#' @param x A `trackable` object.
#' @param by Optional character vector specifying grouping variables.
#'   Defaults to the tracking keys stored in `x`.
#' @param threshold Optional maximum expected time interval.
#'   Intervals exceeding this value are flagged. Accepts a
#'   lubridate duration or a value convertible to seconds.
#'   If `NULL`, all positive intervals are returned.
#' @param ... Currently unused.
#'
#' @return A tibble containing the group identifiers, start time,
#'   end time, and elapsed time in seconds for each gap.
#'
#' @seealso [track_regularise()], [track_interpolate()]
#'
#' @export
track_gaps <- function(x, by = NULL, threshold = NULL, ...) {
  
  meta <- .track_metadata(x)
  
  if (is.null(by)) {
    by <- meta$key
  }
  
  if (!all(by %in% names(x))) {
    stop("Grouping variables not found in `x`.", call. = FALSE)
  }
  
  time_col <- .track_time_column(x)
  groups <- .track_groups(x, by)
  
  threshold_sec <- if (is.null(threshold)) {
    NULL
  } else {
    as.numeric(threshold, units = "secs")
  }
  
  result <- lapply(groups, function(i) {
    
    d <- x[i, ]
    d <- d[order(d[[time_col]]), ]
    
    if (nrow(d) < 2) return(NULL)
    
    elapsed <- .track_elapsed(d[[time_col]])
    
    keep <- !is.na(elapsed) & elapsed > 0
    
    if (!is.null(threshold_sec)) {
      keep <- keep & elapsed > threshold_sec
    }
    
    if (!any(keep)) return(NULL)
    
    out <- sf::st_drop_geometry(d[-nrow(d), by, drop = FALSE])
    out <- out[keep, , drop = FALSE]
    
    tibble::tibble(
      out,
      start_time = d[[time_col]][-nrow(d)][keep],
      end_time = d[[time_col]][-1][keep],
      gap_seconds = elapsed[keep]
    )
  })
  
  dplyr::bind_rows(result)
}
