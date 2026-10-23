
#' Calculate movement metrics for tracked animals
#'
#' Calculate distances, speeds, bearings, turning angles, cumulative
#' distances, and displacement from the starting location.
#'
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
    stop(
      "Longitude and latitude must be numeric.",
      call. = FALSE
    )
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
  
  if (!inherits(x[[time]], "POSIXt")) {
    stop(
      "`time` must be a date-time column of class POSIXct or POSIXlt.",
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
  
  if (!requireNamespace("geosphere", quietly = TRUE)) {
    stop(
      "Please install the 'geosphere' package.",
      call. = FALSE
    )
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "Please install the 'dplyr' package.",
      call. = FALSE
    )
  }
  
  # Preserve original order
  original_names <- names(x)
  x$.movement_row <- seq_len(nrow(x))
  
  # Sort for calculations
  x <- dplyr::arrange(
    x,
    .data[[id]],
    .data[[time]]
  )
  
  # Calculate metrics independently for each animal
  x <- dplyr::group_by(x, .data[[id]])
  
  x <- dplyr::mutate(
    x,
    
    # Previous coordinates and timestamp
    .prev_lon = dplyr::lag(.data[[lon]]),
    .prev_lat = dplyr::lag(.data[[lat]]),
    .prev_time = dplyr::lag(.data[[time]]),
    
    # Time since previous observation, in seconds
    elapsed_time_s = as.numeric(
      difftime(
        .data[[time]],
        .data$.prev_time,
        units = "secs"
      )
    ),
    
    # Distance between consecutive locations, in metres
    step_distance_m = {
      current <- cbind(.data[[lon]], .data[[lat]])
      previous <- cbind(.data$.prev_lon, .data$.prev_lat)
      
      result <- rep(NA_real_, nrow(x))
      
      valid <- complete.cases(current, previous)
      
      if (any(valid)) {
        result[valid] <- geosphere::distHaversine(
          previous[valid, , drop = FALSE],
          current[valid, , drop = FALSE]
        )
      }
      
      result
    },
    
    # Speed in metres per second
    speed_m_s = dplyr::if_else(
      !is.na(.data$elapsed_time_s) &
        .data$elapsed_time_s > 0 &
        !is.na(.data$step_distance_m),
      .data$step_distance_m / .data$elapsed_time_s,
      NA_real_
    ),
    
    # Initial bearing from previous to current location, in degrees
    bearing_deg = {
      current <- cbind(.data[[lon]], .data[[lat]])
      previous <- cbind(.data$.prev_lon, .data$.prev_lat)
      
      result <- rep(NA_real_, nrow(x))
      valid <- complete.cases(current, previous)
      
      if (any(valid)) {
        result[valid] <- geosphere::bearing(
          previous[valid, , drop = FALSE],
          current[valid, , drop = FALSE]
        ) %% 360
      }
      
      result
    },
    
    # Turning angle between consecutive movement bearings
    turn_angle_deg = {
      change <- .data$bearing_deg -
        dplyr::lag(.data$bearing_deg)
      
      (change + 180) %% 360 - 180
    },
    
    # Total distance travelled since the first observation
    cumulative_distance_m = cumsum(
      dplyr::coalesce(.data$step_distance_m, 0)
    ),
    
    # Displacement from the first recorded location
    displacement_m = {
      first_lon <- dplyr::first(.data[[lon]])
      first_lat <- dplyr::first(.data[[lat]])
      
      current <- cbind(.data[[lon]], .data[[lat]])
      origin <- matrix(
        c(first_lon, first_lat),
        nrow = nrow(x),
        ncol = 2,
        byrow = TRUE
      )
      
      result <- rep(NA_real_, nrow(x))
      valid <- complete.cases(current, origin)
      
      if (any(valid)) {
        result[valid] <- geosphere::distHaversine(
          origin[valid, , drop = FALSE],
          current[valid, , drop = FALSE]
        )
      }
      
      result
    }
  )
  
  x <- dplyr::ungroup(x)
  
  # Restore original row order
  x <- dplyr::arrange(x, .data$.movement_row)
  
  x <- dplyr::select(
    x,
    -.data$.movement_row,
    -.data$.prev_lon,
    -.data$.prev_lat,
    -.data$.prev_time
  )
  
  # Restore original column order, followed by new metrics
  new_cols <- setdiff(names(x), original_names)
  
  x <- dplyr::select(
    x,
    dplyr::all_of(original_names),
    dplyr::all_of(new_cols)
  )
  
  x
}
library(dplyr)
library(tibble)

elephant_data <- tribble(
  ~animal_id, ~date_time,             ~lon,     ~lat,
  "E01",      "2026-10-23 08:00:00",  81.6740, 7.8920,
  "E01",      "2026-10-23 08:05:00",  81.6750, 7.8925,
  "E01",      "2026-10-23 08:10:00",  81.6760, 7.8930,
  "E01",      "2026-10-23 08:15:00",  81.6760, 7.8930,
  "E01",      "2026-10-23 08:20:00",  81.6770, 7.8940,
  "E02",      "2026-10-23 08:00:00",  81.6800, 7.8950,
  "E02",      "2026-10-23 08:05:00",  81.6805, 7.8960,
  "E02",      "2026-10-23 08:10:00",  81.6815, 7.8965,
  "E02",      "2026-10-23 08:15:00",  81.6820, 7.8975,
  "E02",      "2026-10-23 08:20:00",  81.6830, 7.8980
) |>
  mutate(date_time = as.POSIXct(
    date_time,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ))

elephant_track <- trackable(
  elephant_data,
  id = animal_id,
  time = date_time,
  key = c("animal_id"),
  lon = lon,
  lat = lat,
  crs = 4326
)
