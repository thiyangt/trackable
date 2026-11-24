#' Calculate movement metrics for tracked animals
#'
#' Calculate distances, speeds, bearings, turning angles, cumulative
#' distances, and displacement from the starting location.
#' @importFrom rlang .data
#' @importFrom stats complete.cases
#' @importFrom dplyr n
#' @param x A trackable object containing animal tracking observations.
#' @param id Name of the animal identifier column.
#' @param time Name of the date-time column.
#' @param lon Name of the longitude column in decimal degrees.
#' @param lat Name of the latitude column in decimal degrees.
#'
#' @return The input object with additional movement metric columns.
#' @export
movement_metrics <- function(
    x,
    id = "animal_id",
    time = "date_time",
    lon = "lon",
    lat = "lat"
) {
  
  # Check required columns
  required <- c(id, time, lon, lat)
  
  missing_cols <- setdiff(required, names(x))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Validate coordinates
  if (!is.numeric(x[[lon]]) || !is.numeric(x[[lat]])) {
    stop("Longitude and latitude must be numeric.",
         call. = FALSE)
  }
  
  if (any(!is.na(x[[lon]]) &
          (!is.finite(x[[lon]]) | abs(x[[lon]]) > 180)) ||
      any(!is.na(x[[lat]]) &
          (!is.finite(x[[lat]]) | abs(x[[lat]]) > 90))) {
    stop(
      "Longitude must be between -180 and 180, ",
      "and latitude between -90 and 90.",
      call. = FALSE
    )
  }
  
  # Validate timestamps
  if (!inherits(x[[time]], "POSIXt")) {
    stop(
      "`time` must be POSIXct or POSIXlt.",
      call. = FALSE
    )
  }
  
  if (anyNA(x[[id]]) || anyNA(x[[time]])) {
    stop(
      "Animal identifiers and timestamps must not contain missing values.",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(x[c(id, time)])) {
    stop(
      "Each animal must have unique timestamps.",
      call. = FALSE
    )
  }
  
  # Check dependencies
  if (!requireNamespace("geosphere", quietly = TRUE)) {
    stop("Please install the 'geosphere' package.",
         call. = FALSE)
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install the 'dplyr' package.",
         call. = FALSE)
  }
  
  # Preserve original column names and row order
  original_names <- names(x)
  
  x$.movement_row <- seq_len(nrow(x))
  
  # Calculate metrics independently for each animal
  x <- dplyr::group_by(x, .data[[id]])
  
  x <- dplyr::group_modify(
    x,
    function(.x, .y) {
      
      # Sort observations chronologically
      z <- .x[order(.x[[time]]), , drop = FALSE]
      
      n <- nrow(z)
      
      # Current and previous coordinates
      current <- cbind(z[[lon]], z[[lat]])
      
      previous <- rbind(
        c(NA_real_, NA_real_),
        current[-n, , drop = FALSE]
      )
      
      # Time elapsed between observations
      previous_time <- c(
        as.POSIXct(NA, origin = "1970-01-01",
                   tz = "UTC"),
        as.numeric(z[[time]][-n])
      )
      
      elapsed_time_s <- as.numeric(
        difftime(
          z[[time]],
          as.POSIXct(previous_time,
                     origin = "1970-01-01",
                     tz = "UTC"),
          units = "secs"
        )
      )
      
      # Valid consecutive coordinate pairs
      valid <- complete.cases(current, previous)
      
      # Step distance in metres
      step_distance_m <- rep(NA_real_, n)
      
      if (any(valid)) {
        step_distance_m[valid] <-
          geosphere::distHaversine(
            previous[valid, , drop = FALSE],
            current[valid, , drop = FALSE]
          )
      }
      
      # Speed in metres per second
      speed_m_s <- ifelse(
        !is.na(elapsed_time_s) &
          elapsed_time_s > 0 &
          !is.na(step_distance_m),
        step_distance_m / elapsed_time_s,
        NA_real_
      )
      
      # Bearing in degrees
      bearing_deg <- rep(NA_real_, n)
      
      if (any(valid)) {
        bearing_deg[valid] <-
          geosphere::bearing(
            previous[valid, , drop = FALSE],
            current[valid, , drop = FALSE]
          ) %% 360
      }
      
      # Turning angle in degrees
      previous_bearing <- c(
        NA_real_,
        bearing_deg[-n]
      )
      
      change <- bearing_deg - previous_bearing
      
      turn_angle_deg <- (change + 180) %% 360 - 180
      
      # Cumulative distance travelled
      cumulative_distance_m <- cumsum(
        dplyr::coalesce(step_distance_m, 0)
      )
      
      # Displacement from first recorded location
      origin <- matrix(
        rep(current[1, ], n),
        nrow = n,
        ncol = 2,
        byrow = TRUE
      )
      
      displacement_m <- rep(NA_real_, n)
      
      valid_origin <- complete.cases(current, origin)
      
      if (any(valid_origin)) {
        displacement_m[valid_origin] <-
          geosphere::distHaversine(
            origin[valid_origin, , drop = FALSE],
            current[valid_origin, , drop = FALSE]
          )
      }
      
      # Append movement metrics
      z$elapsed_time_s <- elapsed_time_s
      z$step_distance_m <- step_distance_m
      z$speed_m_s <- speed_m_s
      z$bearing_deg <- bearing_deg
      z$turn_angle_deg <- turn_angle_deg
      z$cumulative_distance_m <- cumulative_distance_m
      z$displacement_m <- displacement_m
      
      z
    }
  )
  
  # Restore original row order
  x <- dplyr::ungroup(x)
  
  x <- dplyr::arrange(x, .data$.movement_row)
  
  # Remove internal row identifier
  x <- dplyr::select(
    x,
    -.data$.movement_row
  )
  
  # Restore original columns, followed by metrics
  new_cols <- setdiff(names(x), original_names)
  
  x <- dplyr::select(
    x,
    dplyr::all_of(original_names),
    dplyr::all_of(new_cols)
  )
  
  x
}
