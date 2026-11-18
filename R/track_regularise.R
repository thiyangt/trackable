#' Regularise Animal Tracking Data
#'
#' Create a regular time index for each animal or group by
#' inserting rows for missing observation times.
#'
#' @param x A `trackable` object.
#' @param by Optional character vector specifying grouping variables.
#'   Defaults to the tracking keys stored in `x`.
#' @param interval Observation interval, supplied as a duration
#'   or a value convertible to seconds.
#' @param ... Additional arguments passed to
#'   [seq.POSIXt()].
#'
#' @return A spatial tracking object with a regularised time index.
#'   Inserted observations have empty spatial geometries.
#'
#' @seealso [track_gaps()], [track_interpolate()]
#'
#' @export
track_regularise <- function(x, by = NULL, interval, ...) {
  
  meta <- .track_metadata(x)
  
  if (is.null(by)) {
    by <- meta$key
  }
  
  if (!all(by %in% names(x))) {
    stop("Grouping variables not found in `x`.", call. = FALSE)
  }
  
  time_col <- .track_time_column(x)
  
  step <- as.numeric(interval, units = "secs")
  
  if (length(step) != 1 || is.na(step) || step <= 0) {
    stop("`interval` must be a positive time interval.",
         call. = FALSE)
  }
  
  groups <- .track_groups(x, by)
  
  result <- lapply(groups, function(i) {
    
    d <- x[i, ]
    d <- d[order(d[[time_col]]), ]
    
    times <- d[[time_col]]
    
    if (anyNA(times)) {
      stop("Missing timestamps must be resolved first.",
           call. = FALSE)
    }
    
    if (!inherits(times, "POSIXct")) {
      stop("`track_regularise()` currently requires POSIXct timestamps.",
           call. = FALSE)
    }
    
    if (anyDuplicated(times)) {
      stop("Duplicated timestamps must be resolved first.",
           call. = FALSE)
    }
    
    new_times <- seq(
      from = min(times),
      to = max(times),
      by = step,
      ...
    )
    
    index <- match(new_times, times)
    
    # Preserve existing observations and attributes
    out <- d[match(index[!is.na(index)], seq_len(nrow(d))), ]
    
    # Construct a row for each missing timestamp
    missing <- which(is.na(index))
    
    if (length(missing) > 0) {
      
      template <- d[rep(1, length(missing)), ]
      
      for (nm in names(template)) {
        if (nm != attr(template, "sf_column")) {
          template[[nm]] <- NA
        }
      }
      
      template[[time_col]] <- new_times[missing]
      
      template <- sf::st_set_geometry(
        template,
        sf::st_sfc(
          rep(list(sf::st_point()), length(missing)),
          crs = sf::st_crs(d)
        )
      )
      
      out <- rbind(d, template)
    }
    
    out <- out[order(out[[time_col]]), ]
    
    out
  })
  
  result <- do.call(rbind, result)
  
  result
}
