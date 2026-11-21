#' Calculate Spatial Overlap Between Animals
#'
#' Quantify spatial overlap between animal home ranges.
#'
#' The function calculates pairwise spatial overlap using
#' polygon intersections and unions.
#'
#' @param x An `sf` object containing home-range polygons.
#' @param id Character string identifying the animal or group
#'   identifier column.
#' @param method Overlap measure. Currently supports
#'   `"jaccard"` and `"overlap"`.
#' @param ... Additional arguments reserved for future methods.
#'
#' @return A tibble containing pairwise overlap measurements.
#'
#' @details
#' The Jaccard index is calculated as the intersection area
#' divided by the union area. The `"overlap"` measure is
#' calculated as the intersection area divided by the smaller
#' of the two home-range areas.
#'
#' @seealso [track_home_range()]
#'
#' @examples
#' # overlap <- track_overlap(hr, id = "animal_id")
#'
#' @export
track_overlap <- function(
    x,
    id,
    method = "jaccard",
    ...) {
  
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  
  if (!id %in% names(x)) {
    stop("`id` must identify the animal or group.", call. = FALSE)
  }
  
  if (!method %in% c("jaccard", "overlap")) {
    stop(
      "`method` must be 'jaccard' or 'overlap'.",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(x[[id]])) {
    stop(
      "Each animal must have one home-range geometry.",
      call. = FALSE
    )
  }
  
  x <- .track_project(x)
  
  n <- nrow(x)
  
  if (n < 2L) {
    return(tibble::tibble(
      animal_1 = character(),
      animal_2 = character(),
      overlap = double()
    ))
  }
  
  areas <- as.numeric(sf::st_area(x))
  
  result <- list()
  k <- 1L
  
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      
      intersection <- suppressWarnings(
        sf::st_intersection(
          sf::st_geometry(x[i, ]),
          sf::st_geometry(x[j, ])
        )
      )
      
      intersection_area <- if (length(intersection) == 0L) {
        0
      } else {
        sum(as.numeric(sf::st_area(intersection)))
      }
      
      value <- if (method == "jaccard") {
        intersection_area /
          (areas[i] + areas[j] - intersection_area)
      } else {
        intersection_area / min(areas[i], areas[j])
      }
      
      result[[k]] <- tibble::tibble(
        animal_1 = as.character(x[[id]][i]),
        animal_2 = as.character(x[[id]][j]),
        overlap = value
      )
      
      k <- k + 1L
    }
  }
  
  dplyr::bind_rows(result)
}
