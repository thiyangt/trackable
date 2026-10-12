
#' Create a trackable object
#'
#' Create a tidyverse-friendly object for animal tracking data.
#' Each row represents one observation at a particular time and location.
#' Multiple trajectories can be stored in one object.
#'
#' @param data A data frame or tibble.
#' @param time Temporal index column, supplied as a bare name or string.
#' @param x X-coordinate column, supplied as a bare name or string.
#' @param y Y-coordinate column, supplied as a bare name or string.
#' @param key Character vector of grouping variables, e.g.
#'   `c("animal_id", "Length", "Sex")`.
#' @param crs Coordinate reference system. Use 4326 for longitude/latitude.
#' @param ... Additional arguments passed to [sf::st_as_sf()].
#'
#' @return An object of class `trackable` and `sf`.
#' @export
trackable <- function(data, time, x, y, key, crs = NA, ...) {
  
  # Check input data
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  
  # Get a single column name from a bare name or character string
  get_name <- function(expr, arg) {
    if (is.character(expr) && length(expr) == 1L) {
      return(expr)
    }
    
    if (is.symbol(expr)) {
      return(as.character(expr))
    }
    
    stop(
      "`", arg, "` must be a column name or a single string.",
      call. = FALSE
    )
  }
  
  time_name <- get_name(substitute(time), "time")
  x_name <- get_name(substitute(x), "x")
  y_name <- get_name(substitute(y), "y")
  
  # Evaluate key as supplied by the caller
  key_expr <- substitute(key)
  
  if (is.character(key_expr)) {
    key_names <- key_expr
  } else if (is.call(key_expr) && identical(key_expr[[1]], as.name("c"))) {
    key_names <- vapply(
      as.list(key_expr)[-1],
      function(z) {
        if (is.character(z) && length(z) == 1L) {
          return(z)
        }
        if (is.symbol(z)) {
          return(as.character(z))
        }
        stop("Each key must be a column name or string.", call. = FALSE)
      },
      character(1)
    )
  } else if (is.symbol(key_expr)) {
    key_names <- as.character(key_expr)
  } else {
    stop(
      '`key` must be a character vector, e.g. ',
      'c("animal_id", "Length", "Sex").',
      call. = FALSE
    )
  }
  
  if (length(key_names) == 0L || any(!nzchar(key_names))) {
    stop("Supply at least one valid key variable.", call. = FALSE)
  }
  
  key_names <- unique(key_names)
  
  # Check that required columns exist
  required <- unique(c(time_name, x_name, y_name, key_names))
  missing_cols <- setdiff(required, names(data))
  
  if (length(missing_cols) > 0L) {
    stop(
      "Columns not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Validate time
  if (!inherits(data[[time_name]], c("Date", "POSIXt"))) {
    stop("`time` must be a Date or POSIXt column.", call. = FALSE)
  }
  
  # Validate coordinates
  if (!is.numeric(data[[x_name]]) || !is.numeric(data[[y_name]])) {
    stop("`x` and `y` must be numeric columns.", call. = FALSE)
  }
  
  if (anyNA(data[[time_name]]) || anyNA(data[key_names])) {
    stop("Time and key columns cannot contain missing values.",
         call. = FALSE)
  }
  
  # Create sf geometry while retaining x and y
  out <- sf::st_as_sf(
    data,
    coords = c(x_name, y_name),
    crs = crs,
    remove = FALSE,
    ...
  )
  
  # Assign class and tracking metadata
  class(out) <- unique(c("trackable", class(out)))
  attr(out, "trackable_time") <- time_name
  attr(out, "trackable_key") <- key_names
  attr(out, "trackable_coords") <- c(x = x_name, y = y_name)
  
  out
}
#' @rdname trackable
#' @param n Number of rows to print.
#' @param width Maximum display width.
#' @export
print.trackable <- function(x, ..., n = 10, width = NULL) {
  
  key_names <- attr(x, "trackable_key")
  time_name <- attr(x, "trackable_time")
  
  n_obs <- nrow(x)
  n_cols <- ncol(x) - 1L
  
  if (n_obs > 0L) {
    dates <- x[[time_name]]
    span_days <- as.numeric(max(dates) - min(dates)) + 1
    
    span <- if (span_days >= 365.25) {
      paste0(round(span_days / 365.25, 1), "Y")
    } else if (span_days >= 30.44) {
      paste0(round(span_days / 30.44, 1), "M")
    } else {
      paste0(span_days, "D")
    }
  } else {
    span <- "0D"
  }
  
  n_keys <- if (length(key_names) > 0L && n_obs > 0L) {
    nrow(unique(sf::st_drop_geometry(x)[key_names]))
  } else {
    0L
  }
  
  cat(
    "# A trackable: ", n_obs, " x ", n_cols,
    " [", span, "]\n",
    sep = ""
  )
  
  cat(
    "#> # Key:       ",
    paste(key_names, collapse = ", "),
    " [", n_keys, "]\n",
    sep = ""
  )
  
  print(
    tibble::as_tibble(sf::st_drop_geometry(x)),
    n = n,
    width = width,
    ...
  )
  
  invisible(x)
}
#' @examples
#' set.seed(123)
#'
#' dat <- tibble::tibble(
#'   animal_id = rep(c("E001", "E002", "E003"), each = 4),
#'   time = rep(
#'     as.POSIXct("2022-01-01 08:00:00", tz = "UTC") +
#'       86400 * 0:3,
#'     times = 3
#'   ),
#'   x = runif(12, 80.70, 80.90),
#'   y = runif(12, 7.80, 8.00),
#'   Length = rep(c(250, 270, 250), each = 4),
#'   Sex = rep(c("Female", "Male", "Female"), each = 4)
#' )
#'
#' dat
#'
#' trk <- trackable(
#'   data = dat,
#'   time = time,
#'   x = x,
#'   y = y,
#'   key = c("animal_id", "Length", "Sex"),
#'   crs = 4326
#' )
#'
#' trk
#'
#' # Access spatial features
#' sf::st_geometry(trk)
#'
#' # Access tracking metadata
#' attr(trk, "trackable_key")
#' attr(trk, "trackable_time")
#'
#' # Filter observations using dplyr
#' dplyr::filter(trk, animal_id == "E001")
