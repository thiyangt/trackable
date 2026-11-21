#' Identify Duplicate Observations in Animal Tracking Data
#'
#' Identify repeated timestamps, repeated spatial locations,
#' or observations that have both duplicated timestamps and
#' locations within each animal or group.
#'
#' @param x A `trackable` object.
#' @param by Optional character vector specifying grouping variables.
#'   Defaults to the tracking keys stored in `x`.
#' @param type Type of duplication to identify: `"time"`,
#'   `"location"`, or `"both"`.
#' @param ... Currently unused.
#'
#' @return A tibble containing duplicated observations,
#'   their group identifiers, and duplication indicators.
#'
#' @seealso [track_validate()], [track_outliers()]
#'
#' @export
track_duplicates <- function(x, by = NULL,
                             type = c("time", "location", "both"),
                             ...) {
  
  type <- match.arg(type)
  
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
    
    duplicate_time <- duplicated(d[[time_col]]) |
      duplicated(d[[time_col]], fromLast = TRUE)
    
    duplicate_location <- duplicated(sf::st_as_binary(
      sf::st_geometry(d)
    )) | duplicated(sf::st_as_binary(
      sf::st_geometry(d)
    ), fromLast = TRUE)
    
    duplicate_location[is.na(sf::st_is_empty(d)) |
                         sf::st_is_empty(d)] <- FALSE
    
    keep <- switch(
      type,
      time = duplicate_time,
      location = duplicate_location,
      both = duplicate_time & duplicate_location
    )
    
    if (!any(keep)) return(NULL)
    
    out <- sf::st_drop_geometry(d[keep, by, drop = FALSE])
    
    tibble::tibble(
      out,
      row = i[keep],
      timestamp = d[[time_col]][keep],
      duplicate_time = duplicate_time[keep],
      duplicate_location = duplicate_location[keep]
    )
  })
  
  dplyr::bind_rows(result)
}
