
#' Calculate movement metrics for a trackable object
#'
#' @param x A trackable object containing animal locations over time.
#' @param id Name of the animal identifier column.
#' @param time Name of the timestamp column.
#' @param lon Name of the longitude column, in decimal degrees.
#' @param lat Name of the latitude column, in decimal degrees.
#' @param earth_radius Mean Earth radius in metres.
#'
#' @return The input object with additional movement metric columns.
#' @export
movement_metrics <- function(
    x,
    id = "animal_id",
    time = "date_time",
    lon = "lon",
    lat = "lat",
    earth_radius = 6371008.8
) {
  
  required <- c(id, time, lon, lat)
  
  if (!all(required %in% names(x))) {
    stop(
      "Missing required columns: ",
      paste(setdiff(required, names(x)), collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.numeric(x[[lon]]) || !is.numeric(x[[lat]])) {
    stop("Longitude and latitude must be numeric.", call. = FALSE)
  }
  
  if (any(abs(x[[lon][!is.na(x[[lon]])]]) > 180) ||
      any(abs(x[[lat][!is.na(x[[lat]])]]) > 90)) {
    stop("Invalid longitude or latitude values.", call. = FALSE)
  }
  
  if (length(earth_radius) != 1L ||
      !is.numeric(earth_radius) ||
      !is.finite(earth_radius) ||
      earth_radius <= 0) {
    stop("`earth_radius` must be a positive finite number.",
         call. = FALSE)
  }
  
  # Preserve the original row order
  dat <- dplyr::mutate(x, .movement_row = seq_len(nrow(x)))
  
  dat <- dplyr::arrange(
    dat,
    .data[[id]],
    .data[[time]]
  )
  
  dat <- dplyr::group_by(dat, .data[[id]])
  
  dat <- dplyr::mutate(
    dat,
    
    .lon_rad = .data[[lon]] * pi / 180,
    .lat_rad = .data[[lat]] * pi / 180,
    
    .prev_lon = dplyr::lag(.data$.lon_rad),
    .prev_lat = dplyr::lag(.data$.lat_rad),
    .prev_time = dplyr::lag(.data[[time]]),
    
    # Differences between consecutive locations
    .d_lon = .data$.lon_rad - .data$.prev_lon,
    .d_lat = .data$.lat_rad - .data$.prev_lat,
    
    # Haversine distance
    .a = sin(.data$.d_lat / 2)^2 +
      cos(.data$.prev_lat) *
      cos(.data$.lat_rad) *
      sin(.data$.d_lon / 2)^2,
    
    step_distance_m = 2 * earth_radius *
      atan2(
        sqrt(pmax(0, pmin(1, .data$.a))),
        sqrt(pmax(0, 1 - pmax(0, pmin(1, .data$.a))))
      ),
    
    # Time between consecutive observations
    elapsed_time_s = as.numeric(
      difftime(
        .data[[time]],
        .data$.prev_time,
        units = "secs"
      )
    ),
    
    # Speed in metres per second
    speed_m_s = dplyr::if_else(
      is.finite(.data$elapsed_time_s) &
        .data$elapsed_time_s > 0 &
        is.finite(.data$step_distance_m),
      .data$step_distance_m / .data$elapsed_time_s,
      NA_real_
    ),
    
    # Initial bearing from the previous to the current location
    bearing_deg = (
      atan2(
        sin(.data$.d_lon) * cos(.data$.lat_rad),
        cos(.data$.prev_lat) * sin(.data$.lat_rad) -
          sin(.data$.prev_lat) * cos(.data$.lat_rad) *
          cos(.data$.d_lon)
      ) * 180 / pi + 360
    ) %% 360,
    
    # Overall displacement from the first recorded location
    .first_lon = dplyr::first(.data$.lon_rad),
    .first_lat = dplyr::first(.data$.lat_rad),
    
    .disp_d_lon = .data$.lon_rad - .data$.first_lon,
    .disp_d_lat = .data$.lat_rad - .data$.first_lat,
    
    displacement_m = 2 * earth_radius * atan2(
      sqrt(pmax(
        0,
        pmin(
          1,
          sin(.data$.disp_d_lat / 2)^2 +
            cos(.data$.first_lat) *
            cos(.data$.lat_rad) *
            sin(.data$.disp_d_lon / 2)^2
        )
      )),
      sqrt(pmax(
        0,
        1 - pmax(
          0,
          pmin(
            1,
            sin(.data$.disp_d_lat / 2)^2 +
              cos(.data$.first_lat) *
              cos(.data$.lat_rad) *
              sin(.data$.disp_d_lon / 2)^2
          )
        )
      ))
    ),
    
    # Signed turning angle in degrees, between -180 and 180
    turn_angle_deg = {
      change <- .data$bearing_deg -
        dplyr::lag(.data$bearing_deg)
      
      (change + 180) %% 360 - 180
    }
  )
  
  dat <- dplyr::ungroup(dat)
  
  dat <- dplyr::arrange(dat, .data$.movement_row)
  
  dat <- dplyr::select(
    dat,
    -dplyr::all_of(c(
      ".movement_row", ".lon_rad", ".lat_rad",
      ".prev_lon", ".prev_lat", ".prev_time",
      ".d_lon", ".d_lat", ".a",
      ".first_lon", ".first_lat",
      ".disp_d_lon", ".disp_d_lat"
    ))
  )
  
  dat
}
