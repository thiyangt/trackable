# track_range_ecology.R
# Range and ecology tools for animal tracking data.
#
#   track_hull()                      convex hull (MCP) around observed locations
#   track_home_range()                home range by kernel density (KDE) or MCP
#   track_utilization_distribution()  relative intensity of space use (KDE surface)
#   track_overlap()                   spatial overlap between animals or groups
#   track_residence_time()            time spent inside defined areas
#   track_crossings()                 crossings of rivers, roads, habitat boundaries
#
# Requires the 'sf' package. Everything else is base R (stats, grDevices, utils).
# All distances and areas are computed in a projected CRS (metres). By default
# a UTM zone is chosen automatically from the data; override with `crs`.

# ---- Constructor ------------------------------------------------------------

#' Create a trackable object
#'
#' Stores the names of the id, time, longitude and latitude columns on a data
#' frame so they do not have to be repeated in every `track_*()` call.
#'
#' @param x A data frame of animal tracking observations.
#' @param id,time,lon,lat Column names (single strings).
#' @return `x` with class `"trackable"` added.
#' @export
as_trackable <- function(x, id, time, lon, lat) {
  if (!is.data.frame(x)) stop("`x` must be a data frame.", call. = FALSE)
  cols <- list(id = id, time = time, lon = lon, lat = lat)
  miss <- setdiff(unlist(cols), names(x))
  if (length(miss)) {
    stop("Column(s) not found in `x`: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  attr(x, "trk_cols") <- cols
  class(x) <- unique(c("trackable", class(x)))
  x
}

# ---- Internal helpers -------------------------------------------------------

.trk_resolve <- function(x, id, time, lon, lat) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or a trackable object.", call. = FALSE)
  }
  stored <- attr(x, "trk_cols", exact = TRUE)
  args <- list(id = id, time = time, lon = lon, lat = lat)
  out <- list()
  for (nm in c("id", "time", "lon", "lat")) {
    v <- args[[nm]]
    if (is.null(v)) v <- stored[[nm]]
    if (is.null(v)) {
      stop("`", nm, "` was not supplied and `x` is not a trackable object that stores it. ",
           "Pass the column name, or create `x` with as_trackable().", call. = FALSE)
    }
    if (!is.character(v) || length(v) != 1L) {
      stop("`", nm, "` must be a single column name (string).", call. = FALSE)
    }
    out[[nm]] <- v
  }
  out
}

.trk_time <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "POSIXlt")) return(as.POSIXct(x))
  if (inherits(x, "Date")) return(as.POSIXct(x))
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    out <- tryCatch(
      as.POSIXct(x, tz = "UTC",
                 tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                                "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M",
                                "%Y-%m-%d", "%Y/%m/%d")),
      error = function(e) NULL
    )
    if (is.null(out) || any(is.na(out) & !is.na(x))) {
      stop("Could not parse the time column. Use POSIXct, or text like ",
           "'2026-01-31 14:05:00'.", call. = FALSE)
    }
    return(out)
  }
  stop("The time column must be POSIXct, Date, or a date-time string.", call. = FALSE)
}

.trk_utm <- function(lon, lat) {
  zone <- floor((stats::median(lon) + 180) / 6) + 1
  zone <- min(max(zone, 1), 60)
  base <- if (stats::median(lat) >= 0) 32600L else 32700L
  sf::st_crs(as.integer(base + zone))
}

.trk_unit_secs <- function(units) {
  c(secs = 1, mins = 60, hours = 3600, days = 86400)[[units]]
}

.trk_points_sfc <- function(x, y, crs) {
  if (length(x) == 0L) return(sf::st_sfc(crs = crs))
  sf::st_geometry(sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = crs))
}

.trk_out <- function(obj, crs_out) {
  if (is.null(crs_out)) obj else sf::st_transform(obj, crs_out)
}

.trk_check_grid <- function(grid_size) {
  if (!is.numeric(grid_size) || length(grid_size) != 1L || grid_size < 20) {
    stop("`grid_size` must be a single number >= 20.", call. = FALSE)
  }
}

# Validate, clean, sort and project the fixes. Returns list(data, crs).
# `data` has: id, time, lon, lat, x, y (projected, metres), and optionally group.
.trk_prep <- function(x, id, time, lon, lat, crs = NULL, group = NULL) {
  cols <- .trk_resolve(x, id, time, lon, lat)
  need <- unlist(cols)
  if (!is.null(group)) need <- c(need, group)
  miss <- setdiff(need, names(x))
  if (length(miss)) {
    stop("Column(s) not found in `x`: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  d <- data.frame(
    id = as.character(x[[cols$id]]),
    time = .trk_time(x[[cols$time]]),
    lon = as.numeric(x[[cols$lon]]),
    lat = as.numeric(x[[cols$lat]]),
    stringsAsFactors = FALSE
  )
  if (!is.null(group)) d$group <- as.character(x[[group]])
  
  bad <- is.na(d$id) | is.na(d$time) | !is.finite(d$lon) | !is.finite(d$lat)
  if (any(bad)) {
    warning(sum(bad), " fix(es) with missing id, time or coordinates were dropped.",
            call. = FALSE)
    d <- d[!bad, , drop = FALSE]
  }
  if (!nrow(d)) stop("No usable fixes in `x`.", call. = FALSE)
  if (any(abs(d$lon) > 180 | abs(d$lat) > 90)) {
    stop("`lon` must be within [-180, 180] and `lat` within [-90, 90] (decimal degrees).",
         call. = FALSE)
  }
  
  d <- d[order(d$id, d$time), , drop = FALSE]
  dup <- duplicated(d[c("id", "time")])
  if (any(dup)) {
    warning(sum(dup), " duplicated id/time fix(es) were dropped.", call. = FALSE)
    d <- d[!dup, , drop = FALSE]
  }
  
  crs <- if (is.null(crs)) .trk_utm(d$lon, d$lat) else sf::st_crs(crs)
  if (is.na(crs) || isTRUE(sf::st_is_longlat(crs))) {
    stop("`crs` must be a projected CRS in metres (e.g. a UTM zone such as 32644).",
         call. = FALSE)
  }
  ll <- sf::st_as_sf(d[c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
  xy <- sf::st_coordinates(sf::st_transform(ll, crs))
  d$x <- xy[, 1]
  d$y <- xy[, 2]
  rownames(d) <- NULL
  list(data = d, crs = crs)
}

# Consecutive-fix segments within each animal. `valid` is FALSE across animals
# and when the time gap exceeds `max_gap`.
.trk_segments <- function(d, max_gap = NULL, units = "hours") {
  n <- nrow(d)
  if (n < 2L) return(NULL)
  if (!is.null(max_gap) && (!is.numeric(max_gap) || length(max_gap) != 1L || max_gap <= 0)) {
    stop("`max_gap` must be a single positive number.", call. = FALSE)
  }
  i <- seq_len(n - 1L)
  seg <- data.frame(
    id = d$id[i], t1 = d$time[i], t2 = d$time[i + 1L],
    x1 = d$x[i], y1 = d$y[i], x2 = d$x[i + 1L], y2 = d$y[i + 1L],
    stringsAsFactors = FALSE
  )
  seg$dt <- as.numeric(difftime(seg$t2, seg$t1, units = "secs"))
  ok <- d$id[i] == d$id[i + 1L] & seg$dt > 0
  if (!is.null(max_gap)) ok <- ok & seg$dt <= max_gap * .trk_unit_secs(units)
  seg$valid <- ok
  seg
}

# ---- Kernel density helpers -------------------------------------------------

# Reference bandwidth (metres) for a bivariate normal kernel.
.trk_href <- function(xy) {
  sigma <- sqrt(0.5 * (stats::var(xy[, 1]) + stats::var(xy[, 2])))
  if (!is.finite(sigma) || sigma <= 0) {
    stop("Cannot estimate a bandwidth: the locations have no spread.", call. = FALSE)
  }
  sigma * nrow(xy)^(-1 / 6)
}

# Gaussian KDE on a regular grid. z is a density (per m^2) normalised so that
# sum(z) * cell_area == 1. Layout: z[i, j] is the cell at x = x[i], y = y[j].
.trk_kde <- function(xy, h = NULL, grid_size = 150L, extent = NULL, pad = 3) {
  n <- nrow(xy)
  if (is.null(h)) h <- .trk_href(xy)
  if (!is.numeric(h) || length(h) != 1L || !is.finite(h) || h <= 0) {
    stop("`h` must be a single positive number (metres).", call. = FALSE)
  }
  if (is.null(extent)) {
    extent <- c(range(xy[, 1]) + c(-1, 1) * pad * h,
                range(xy[, 2]) + c(-1, 1) * pad * h)
  }
  gx <- seq(extent[1], extent[2], length.out = grid_size)
  gy <- seq(extent[3], extent[4], length.out = grid_size)
  kx <- stats::dnorm(outer(gx, xy[, 1], "-") / h) / h
  ky <- stats::dnorm(outer(gy, xy[, 2], "-") / h) / h
  z <- (kx %*% t(ky)) / n
  dx <- gx[2] - gx[1]
  dy <- gy[2] - gy[1]
  z <- z / (sum(z) * dx * dy)
  list(x = gx, y = gy, z = z, h = h, cell_area = dx * dy, n = n)
}

# Density value of the contour that encloses `percent`% of the UD volume.
.trk_level <- function(ud, percent) {
  z <- as.vector(ud$z)
  p <- z * ud$cell_area
  o <- order(p, decreasing = TRUE)
  cp <- cumsum(p[o])
  k <- which(cp >= percent / 100 - 1e-12)[1]
  if (is.na(k)) k <- length(cp)
  z[o[k]]
}

# Isopleth polygon (sfc of length 1) or NULL.
.trk_ud_polygon <- function(ud, percent, crs) {
  lev <- max(.trk_level(ud, percent), 1e-300)
  nx <- length(ud$x)
  ny <- length(ud$y)
  dx <- ud$x[2] - ud$x[1]
  dy <- ud$y[2] - ud$y[1]
  # Pad the grid with a zero border so that every contour is a closed ring.
  gx <- c(ud$x[1] - dx, ud$x, ud$x[nx] + dx)
  gy <- c(ud$y[1] - dy, ud$y, ud$y[ny] + dy)
  z <- matrix(0, nx + 2L, ny + 2L)
  z[2:(nx + 1L), 2:(ny + 1L)] <- ud$z
  cl <- grDevices::contourLines(gx, gy, z, levels = lev)
  if (!length(cl)) return(NULL)
  rings <- list()
  for (ln in cl) {
    m <- cbind(ln$x, ln$y)
    if (nrow(m) < 3L) next
    if (any(m[1, ] != m[nrow(m), ])) m <- rbind(m, m[1, ])
    if (nrow(m) < 4L) next
    rings[[length(rings) + 1L]] <- sf::st_sfc(sf::st_polygon(list(m)), crs = crs)
  }
  if (!length(rings)) return(NULL)
  # Even-odd combination: nested rings become holes, islands inside holes return.
  geom <- Reduce(function(a, b) sf::st_sym_difference(a, b), rings)
  geom <- sf::st_make_valid(geom)
  if (length(geom) != 1L || all(sf::st_is_empty(geom))) return(NULL)
  geom
}

.trk_ud_list <- function(d, h, grid_size, extent, pad, min_fixes) {
  out <- list()
  for (u in unique(d$id)) {
    xy <- as.matrix(d[d$id == u, c("x", "y")])
    if (nrow(xy) < min_fixes) {
      warning("'", u, "' has fewer than ", min_fixes, " fixes and was skipped.", call. = FALSE)
      next
    }
    ud <- .trk_kde(xy, h, grid_size, extent, pad)
    ud$id <- u
    out[[u]] <- ud
  }
  if (!length(out)) {
    stop("No animal has at least ", min_fixes, " fixes.", call. = FALSE)
  }
  out
}

# ---- Convex hull ------------------------------------------------------------

.trk_hull_sf <- function(d, crs, percent, min_fixes) {
  dfs <- list()
  geoms <- list()
  for (u in unique(d$id)) {
    xy_all <- as.matrix(d[d$id == u, c("x", "y")])
    if (nrow(xy_all) < max(3L, min_fixes)) {
      warning("'", u, "' has fewer than ", max(3L, min_fixes),
              " fixes and was skipped.", call. = FALSE)
      next
    }
    xy <- xy_all
    if (percent < 100) {
      cen <- colMeans(xy_all)
      dist <- sqrt((xy_all[, 1] - cen[1])^2 + (xy_all[, 2] - cen[2])^2)
      n_keep <- max(3L, ceiling(nrow(xy_all) * percent / 100))
      xy <- xy_all[order(dist)[seq_len(n_keep)], , drop = FALSE]
    }
    xy <- unique(xy)
    idx <- if (nrow(xy) >= 3L) grDevices::chull(xy) else integer(0)
    if (length(idx) < 3L) {
      warning("'", u, "': fixes are collinear or identical; no hull was built.", call. = FALSE)
      next
    }
    hx <- xy[idx, 1]
    hy <- xy[idx, 2]
    nx <- c(hx[-1], hx[1])
    ny <- c(hy[-1], hy[1])
    area <- 0.5 * abs(sum(hx * ny - nx * hy))
    perim <- sum(sqrt((nx - hx)^2 + (ny - hy)^2))
    ring <- rbind(xy[idx, , drop = FALSE], xy[idx[1], , drop = FALSE])
    dfs[[length(dfs) + 1L]] <- data.frame(
      id = u, n_fixes = nrow(xy_all), n_used = nrow(xy), percent = percent,
      area_km2 = area / 1e6, perimeter_km = perim / 1000, stringsAsFactors = FALSE
    )
    geoms[[length(geoms) + 1L]] <- sf::st_polygon(list(ring))
  }
  if (!length(dfs)) stop("No hull could be built.", call. = FALSE)
  sf::st_sf(do.call(rbind, dfs), geometry = sf::st_sfc(geoms, crs = crs))
}

#' Convex hull around observed locations
#'
#' Builds a minimum convex polygon (MCP) around each animal's fixes. With
#' `percent < 100` the fixes farthest from the arithmetic centroid are removed
#' first, which stops a few outlying fixes from inflating the hull.
#'
#' @param x A trackable object containing animal tracking observations.
#' @param id Name of the animal identifier column.
#' @param time Name of the date-time column.
#' @param lon Name of the longitude column in decimal degrees.
#' @param lat Name of the latitude column in decimal degrees.
#' @param percent Percentage of fixes (closest to the centroid) to keep, in
#'   (0, 100]. Default 100.
#' @param by_id If `TRUE` (default) one hull per animal; if `FALSE` one hull
#'   around all fixes pooled.
#' @param min_fixes Animals with fewer fixes are skipped with a warning.
#' @param crs Projected CRS (metres) used for the computation. Default: the UTM
#'   zone of the data.
#' @param crs_out Optional CRS for the returned geometry, e.g. `4326`. Default:
#'   the computation CRS.
#' @return An `sf` data frame with one polygon per animal and columns `id`,
#'   `n_fixes`, `n_used`, `percent`, `area_km2`, `perimeter_km`.
#' @examples
#' set.seed(1)
#' n <- 200
#' trk <- data.frame(
#'   animal = rep(c("E1", "E2"), each = n),
#'   timestamp = rep(seq(as.POSIXct("2026-01-01", tz = "UTC"), by = "1 hour",
#'                       length.out = n), 2),
#'   lon = 80.387 + c(cumsum(rnorm(n, 0, 0.0008)), cumsum(rnorm(n, 0.0002, 0.0008))),
#'   lat = 7.300 + c(cumsum(rnorm(n, 0, 0.0008)), cumsum(rnorm(n, 0, 0.0008)))
#' )
#' trk <- as_trackable(trk, id = "animal", time = "timestamp", lon = "lon", lat = "lat")
#' track_hull(trk)
#' track_hull(trk, percent = 95, crs_out = 4326)
#' @export
track_hull <- function(x, id = NULL, time = NULL, lon = NULL, lat = NULL,
                       percent = 100, by_id = TRUE, min_fixes = 3L,
                       crs = NULL, crs_out = NULL) {
  if (!is.numeric(percent) || length(percent) != 1L || percent <= 0 || percent > 100) {
    stop("`percent` must be a single number in (0, 100].", call. = FALSE)
  }
  p <- .trk_prep(x, id, time, lon, lat, crs)
  d <- p$data
  if (!by_id) d$id <- "all"
  .trk_out(.trk_hull_sf(d, p$crs, percent, min_fixes), crs_out)
}

# ---- Utilization distribution -----------------------------------------------

#' Utilization distribution (relative intensity of space use)
#'
#' Estimates a bivariate Gaussian kernel density surface for each animal on a
#' regular grid. The surface is normalised so the cell probabilities sum to 1.
#'
#' Notes: the kernel treats fixes as independent, so autocorrelated (e.g.
#' hourly) data give an over-confident surface. The default reference
#' bandwidth (`href`) tends to over-smooth multi-modal use; try a smaller `h`.
#'
#' @inheritParams track_hull
#' @param h Kernel bandwidth in metres. Default: the reference bandwidth
#'   `sigma * n^(-1/6)`.
#' @param grid_size Number of grid cells along each axis (>= 20).
#' @param extent Optional `c(xmin, xmax, ymin, ymax)` in the computation CRS.
#'   Default: the data range padded by `pad * h`.
#' @param pad Padding, in bandwidths, around the data when `extent` is `NULL`.
#' @param min_fixes Animals with fewer fixes are skipped with a warning.
#' @return An object of class `"track_ud"`. Use `as.data.frame()` to get a table
#'   of cells with `density`, `probability`, `isopleth` (the smallest UD
#'   percentage that contains the cell) and `lon`/`lat`.
#' @examples
#' \dontrun{
#' ud <- track_utilization_distribution(trk)
#' ud
#' head(as.data.frame(ud))
#' }
#' @export
track_utilization_distribution <- function(x, id = NULL, time = NULL, lon = NULL, lat = NULL,
                                           h = NULL, grid_size = 150L, extent = NULL,
                                           pad = 3, by_id = TRUE, min_fixes = 5L,
                                           crs = NULL) {
  .trk_check_grid(grid_size)
  p <- .trk_prep(x, id, time, lon, lat, crs)
  d <- p$data
  if (!by_id) d$id <- "all"
  uds <- .trk_ud_list(d, h, grid_size, extent, pad, min_fixes)
  structure(list(ud = uds, crs = p$crs), class = "track_ud")
}

#' @export
print.track_ud <- function(x, ...) {
  info <- do.call(rbind, lapply(x$ud, function(u) {
    data.frame(id = u$id, n_fixes = u$n, h_m = round(u$h, 1),
               grid = paste(length(u$x), "x", length(u$y)),
               cell_m2 = round(u$cell_area, 1), stringsAsFactors = FALSE)
  }))
  cat("<track_ud> kernel utilization distributions, CRS: ", x$crs$input, "\n", sep = "")
  print(info, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.track_ud <- function(x, row.names = NULL, optional = FALSE, ...) {
  parts <- lapply(x$ud, function(ud) {
    nx <- length(ud$x)
    ny <- length(ud$y)
    dens <- as.vector(ud$z)
    pr <- dens * ud$cell_area
    o <- order(pr, decreasing = TRUE)
    iso <- numeric(length(pr))
    iso[o] <- cumsum(pr[o]) * 100
    data.frame(id = ud$id, x = rep(ud$x, times = ny), y = rep(ud$y, each = nx),
               density = dens, probability = pr, isopleth = iso,
               stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, parts)
  pts <- sf::st_geometry(sf::st_as_sf(df[c("x", "y")], coords = c("x", "y"), crs = x$crs))
  ll <- sf::st_coordinates(sf::st_transform(pts, 4326))
  df$lon <- ll[, 1]
  df$lat <- ll[, 2]
  rownames(df) <- NULL
  df
}

# ---- Home range -------------------------------------------------------------

#' Home range estimate
#'
#' Estimates home range polygons with kernel density estimation (`"kde"`, the
#' `percent` isopleth of the utilization distribution) or a minimum convex
#' polygon (`"mcp"`).
#'
#' @inheritParams track_utilization_distribution
#' @param method `"kde"` (default) or `"mcp"`.
#' @param percent One or more percentages, e.g. `c(50, 95)` for core area and
#'   home range. Must be in (0, 100) for `"kde"`, in (0, 100] for `"mcp"`.
#' @return An `sf` data frame with one polygon per animal and percentage and
#'   columns `id`, `method`, `percent`, `n_fixes`, `h_m` (bandwidth, `NA` for
#'   MCP), `area_km2`.
#' @examples
#' \dontrun{
#' hr <- track_home_range(trk, method = "kde", percent = c(50, 95), crs_out = 4326)
#' plot(sf::st_geometry(hr))
#' }
#' @export
track_home_range <- function(x, id = NULL, time = NULL, lon = NULL, lat = NULL,
                             method = c("kde", "mcp"), percent = 95,
                             h = NULL, grid_size = 150L, pad = 3, by_id = TRUE,
                             min_fixes = 5L, crs = NULL, crs_out = NULL) {
  method <- match.arg(method)
  if (!is.numeric(percent) || !length(percent) || any(percent <= 0) || any(percent > 100)) {
    stop("`percent` must be numbers in (0, 100].", call. = FALSE)
  }
  if (method == "kde" && any(percent >= 100)) {
    stop("For method = \"kde\", `percent` must be below 100.", call. = FALSE)
  }
  .trk_check_grid(grid_size)
  p <- .trk_prep(x, id, time, lon, lat, crs)
  d <- p$data
  crs <- p$crs
  if (!by_id) d$id <- "all"
  
  if (method == "mcp") {
    hh <- do.call(rbind, lapply(percent, function(pc) .trk_hull_sf(d, crs, pc, min_fixes)))
    hh$method <- "mcp"
    hh$h_m <- NA_real_
    res <- hh[, c("id", "method", "percent", "n_fixes", "h_m", "area_km2")]
    return(.trk_out(res, crs_out))
  }
  
  uds <- .trk_ud_list(d, h, grid_size, NULL, pad, min_fixes)
  dfs <- list()
  geoms <- list()
  for (u in names(uds)) {
    ud <- uds[[u]]
    for (pc in percent) {
      poly <- .trk_ud_polygon(ud, pc, crs)
      if (is.null(poly)) {
        warning("'", u, "': no ", pc, "% isopleth could be built.", call. = FALSE)
        next
      }
      dfs[[length(dfs) + 1L]] <- data.frame(
        id = u, method = "kde", percent = pc, n_fixes = ud$n, h_m = ud$h,
        area_km2 = as.numeric(sf::st_area(poly)) / 1e6, stringsAsFactors = FALSE
      )
      geoms[[length(geoms) + 1L]] <- poly
    }
  }
  if (!length(dfs)) stop("No home range could be estimated.", call. = FALSE)
  res <- sf::st_sf(do.call(rbind, dfs), geometry = do.call(c, geoms))
  .trk_out(res, crs_out)
}

# ---- Overlap ----------------------------------------------------------------

#' Spatial overlap between animals or groups
#'
#' Computes pairwise overlap from kernel utilization distributions evaluated
#' on one shared grid. For every pair it returns:
#' * `prop_a_in_b`, `prop_b_in_a`: share of each home range (`percent`
#'   isopleth) that lies inside the other.
#' * `jaccard`: shared area / combined area of the two home ranges.
#' * `vi`: volume intersection, sum of the cell-wise minimum of the two UDs
#'   (0 = none, 1 = identical).
#' * `ba`: Bhattacharyya's affinity (0 = none, 1 = identical).
#' * `udoi`: utilization distribution overlap index (Fieberg & Kochanny 2005).
#'   It can exceed 1 when both animals concentrate use in the shared area.
#'
#' Areas come from grid cells, so they are approximations; raise `grid_size`
#' for finer results.
#'
#' @inheritParams track_utilization_distribution
#' @param group Optional name of a column defining groups (e.g. herd, sex).
#'   When given, fixes are pooled within each group and overlap is computed
#'   between groups instead of between animals.
#' @param percent Home range percentage used for the area-based metrics.
#' @return A data frame with one row per pair: `unit_a`, `unit_b`,
#'   `area_a_km2`, `area_b_km2`, `overlap_km2`, `prop_a_in_b`, `prop_b_in_a`,
#'   `jaccard`, `vi`, `ba`, `udoi`.
#' @examples
#' \dontrun{
#' track_overlap(trk, percent = 95)
#' }
#' @export
track_overlap <- function(x, id = NULL, time = NULL, lon = NULL, lat = NULL,
                          group = NULL, percent = 95, h = NULL, grid_size = 200L,
                          min_fixes = 5L, crs = NULL) {
  if (!is.numeric(percent) || length(percent) != 1L || percent <= 0 || percent >= 100) {
    stop("`percent` must be a single number in (0, 100).", call. = FALSE)
  }
  .trk_check_grid(grid_size)
  p <- .trk_prep(x, id, time, lon, lat, crs, group = group)
  d <- p$data
  d$unit <- if (is.null(group)) d$id else d$group
  
  counts <- table(d$unit)
  keep <- names(counts)[counts >= min_fixes]
  dropped <- setdiff(names(counts), keep)
  if (length(dropped)) {
    warning("Skipped (fewer than ", min_fixes, " fixes): ",
            paste(dropped, collapse = ", "), call. = FALSE)
  }
  if (length(keep) < 2L) {
    stop("Need at least two animals (or groups) with >= ", min_fixes, " fixes.", call. = FALSE)
  }
  d <- d[d$unit %in% keep, , drop = FALSE]
  
  xy <- lapply(keep, function(u) as.matrix(d[d$unit == u, c("x", "y")]))
  names(xy) <- keep
  hs <- vapply(xy, function(m) if (is.null(h)) .trk_href(m) else h, numeric(1))
  
  pad <- 3
  ext <- c(range(d$x) + c(-1, 1) * pad * max(hs),
           range(d$y) + c(-1, 1) * pad * max(hs))
  cell_size <- c(ext[2] - ext[1], ext[4] - ext[3]) / (grid_size - 1)
  if (max(cell_size) > min(hs) / 2) {
    warning("Grid cells (", round(max(cell_size)), " m) are coarse compared with the ",
            "smallest bandwidth (", round(min(hs)), " m); consider a larger `grid_size`.",
            call. = FALSE)
  }
  
  uds <- Map(function(m, hh) .trk_kde(m, hh, grid_size, ext, pad), xy, hs)
  lev <- vapply(uds, .trk_level, numeric(1), percent = percent)
  masks <- Map(function(u, l) u$z >= l, uds, lev)
  cell <- uds[[1]]$cell_area
  
  rows <- list()
  for (pr in utils::combn(names(uds), 2L, simplify = FALSE)) {
    a <- pr[1]
    b <- pr[2]
    za <- uds[[a]]$z
    zb <- uds[[b]]$z
    pa <- za * cell
    pb <- zb * cell
    ia <- masks[[a]]
    ib <- masks[[b]]
    inter <- ia & ib
    n_a <- sum(ia)
    n_b <- sum(ib)
    n_i <- sum(inter)
    n_u <- sum(ia | ib)
    rows[[length(rows) + 1L]] <- data.frame(
      unit_a = a, unit_b = b,
      area_a_km2 = n_a * cell / 1e6,
      area_b_km2 = n_b * cell / 1e6,
      overlap_km2 = n_i * cell / 1e6,
      prop_a_in_b = if (n_a > 0) n_i / n_a else NA_real_,
      prop_b_in_a = if (n_b > 0) n_i / n_b else NA_real_,
      jaccard = if (n_u > 0) n_i / n_u else NA_real_,
      vi = sum(pmin(pa, pb)),
      ba = sum(sqrt(pa * pb)),
      udoi = (n_i * cell) * sum(za[inter] * zb[inter]) * cell,
      stringsAsFactors = FALSE
    )
  }
  res <- do.call(rbind, rows)
  rownames(res) <- NULL
  attr(res, "percent") <- percent
  res
}

# ---- Residence time ---------------------------------------------------------

.trk_area <- function(area, crs, name_col = NULL) {
  lab <- NULL
  if (inherits(area, "sf")) {
    if (!is.null(name_col)) {
      if (!name_col %in% names(area)) {
        stop("`area_name` column '", name_col, "' not found in `area`.", call. = FALSE)
      }
      lab <- as.character(area[[name_col]])
    }
    geom <- sf::st_geometry(area)
  } else if (inherits(area, "sfc")) {
    geom <- area
  } else {
    stop("`area` must be an sf or sfc object with polygon geometry.", call. = FALSE)
  }
  if (is.na(sf::st_crs(geom))) {
    stop("`area` has no CRS; set one with sf::st_set_crs().", call. = FALSE)
  }
  types <- unique(as.character(sf::st_geometry_type(geom)))
  if (!all(types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("`area` must contain polygons only.", call. = FALSE)
  }
  geom <- sf::st_make_valid(sf::st_transform(geom, crs))
  if (is.null(lab)) lab <- paste0("area_", seq_along(geom))
  list(geom = geom, label = lab)
}

# Fraction of each segment lying inside each area, by sampling points along
# the segment (spacing ~ `resolution` metres, at most 200 samples per segment).
.trk_seg_frac_inside <- function(seg, geom, crs, resolution) {
  m <- nrow(seg)
  nf <- length(geom)
  out <- matrix(0, m, nf)
  bb <- sf::st_bbox(geom)
  cand <- which(pmax(seg$x1, seg$x2) >= bb[["xmin"]] & pmin(seg$x1, seg$x2) <= bb[["xmax"]] &
                  pmax(seg$y1, seg$y2) >= bb[["ymin"]] & pmin(seg$y1, seg$y2) <= bb[["ymax"]])
  if (!length(cand)) return(out)
  len <- sqrt((seg$x2[cand] - seg$x1[cand])^2 + (seg$y2[cand] - seg$y1[cand])^2)
  k <- as.integer(pmin(200L, pmax(1L, ceiling(len / resolution))))
  si <- rep(seq_along(cand), k)
  f <- (sequence(k) - 0.5) / k[si]
  ci <- cand[si]
  px <- seg$x1[ci] + f * (seg$x2[ci] - seg$x1[ci])
  py <- seg$y1[ci] + f * (seg$y2[ci] - seg$y1[ci])
  ins <- sf::st_intersects(.trk_points_sfc(px, py, crs), geom, sparse = FALSE)
  for (b in seq_len(nf)) {
    out[cand, b] <- as.numeric(rowsum(as.numeric(ins[, b]), si)) / k
  }
  out
}

#' Residence time inside defined areas
#'
#' Estimates how long each animal stays inside one or more polygons. Time is
#' allocated along each step assuming straight-line, constant-speed movement
#' between consecutive fixes, so steps that enter or leave an area are split
#' proportionally.
#'
#' A "visit" is a run of consecutive steps that touch the area; steps across a
#' gap longer than `max_gap` end a visit.
#'
#' @inheritParams track_hull
#' @param area An `sf`/`sfc` object with polygon geometry (any CRS).
#' @param area_name Optional column of `area` holding a label for each polygon.
#' @param max_gap Steps longer than this (in `units`) are ignored, because the
#'   path across a long gap is unknown. Default: no limit.
#' @param units One of `"hours"` (default), `"mins"`, `"secs"`, `"days"`.
#' @param resolution Approximate spacing (metres) of the sample points used to
#'   split steps that cross an area boundary.
#' @return A data frame with one row per animal and area: `id`, `area`,
#'   `n_fixes`, `n_fixes_inside`, `prop_fixes_inside`, `tracked_time`,
#'   `time_inside`, `prop_time_inside`, `n_visits`, `mean_visit`,
#'   `median_visit`, `max_visit` and `units`.
#' @examples
#' \dontrun{
#' park <- sf::st_sfc(sf::st_polygon(list(cbind(
#'   c(80.38, 80.395, 80.395, 80.38, 80.38),
#'   c(7.295, 7.295, 7.305, 7.305, 7.295)))), crs = 4326)
#' track_residence_time(trk, park, max_gap = 6)
#' }
#' @export
track_residence_time <- function(x, area, id = NULL, time = NULL, lon = NULL, lat = NULL,
                                 area_name = NULL, max_gap = NULL,
                                 units = c("hours", "mins", "secs", "days"),
                                 resolution = 25, crs = NULL) {
  units <- match.arg(units)
  if (!is.numeric(resolution) || length(resolution) != 1L || resolution <= 0) {
    stop("`resolution` must be a single positive number (metres).", call. = FALSE)
  }
  p <- .trk_prep(x, id, time, lon, lat, crs)
  d <- p$data
  crs <- p$crs
  ar <- .trk_area(area, crs, area_name)
  nf <- length(ar$geom)
  div <- .trk_unit_secs(units)
  
  seg <- .trk_segments(d, max_gap, units)
  fix_in <- sf::st_intersects(.trk_points_sfc(d$x, d$y, crs), ar$geom, sparse = FALSE)
  frac <- if (is.null(seg)) NULL else .trk_seg_frac_inside(seg, ar$geom, crs, resolution)
  
  rows <- list()
  for (u in unique(d$id)) {
    ix <- which(d$id == u)
    sg <- if (is.null(seg)) integer(0) else which(seg$id == u)
    valid <- if (length(sg)) seg$valid[sg] else logical(0)
    dt <- if (length(sg)) seg$dt[sg] else numeric(0)
    for (b in seq_len(nf)) {
      fr <- if (length(sg)) frac[sg, b] else numeric(0)
      t_in <- ifelse(valid, dt * fr, 0)
      t_trk <- sum(dt[valid])
      in_seg <- valid & fr > 0
      visits <- numeric(0)
      if (any(in_seg)) {
        r <- rle(in_seg)
        grp <- rep(seq_along(r$lengths), r$lengths)
        visits <- as.numeric(tapply(t_in[in_seg], grp[in_seg], sum))
      }
      rows[[length(rows) + 1L]] <- data.frame(
        id = u, area = ar$label[b],
        n_fixes = length(ix),
        n_fixes_inside = sum(fix_in[ix, b]),
        prop_fixes_inside = mean(fix_in[ix, b]),
        tracked_time = t_trk / div,
        time_inside = sum(t_in) / div,
        prop_time_inside = if (t_trk > 0) sum(t_in) / t_trk else NA_real_,
        n_visits = length(visits),
        mean_visit = if (length(visits)) mean(visits) / div else NA_real_,
        median_visit = if (length(visits)) stats::median(visits) / div else NA_real_,
        max_visit = if (length(visits)) max(visits) / div else NA_real_,
        units = units,
        stringsAsFactors = FALSE
      )
    }
  }
  res <- do.call(rbind, rows)
  rownames(res) <- NULL
  res
}

# ---- Crossings --------------------------------------------------------------

.trk_barrier <- function(barrier, crs, name_col = NULL) {
  lab <- NULL
  if (inherits(barrier, "sf")) {
    if (!is.null(name_col)) {
      if (!name_col %in% names(barrier)) {
        stop("`barrier_name` column '", name_col, "' not found in `barrier`.", call. = FALSE)
      }
      lab <- as.character(barrier[[name_col]])
    }
    geom <- sf::st_geometry(barrier)
  } else if (inherits(barrier, "sfc")) {
    geom <- barrier
  } else {
    stop("`barrier` must be an sf or sfc object (lines or polygons).", call. = FALSE)
  }
  if (is.na(sf::st_crs(geom))) {
    stop("`barrier` has no CRS; set one with sf::st_set_crs().", call. = FALSE)
  }
  types <- unique(as.character(sf::st_geometry_type(geom)))
  is_poly <- all(types %in% c("POLYGON", "MULTIPOLYGON"))
  is_line <- all(types %in% c("LINESTRING", "MULTILINESTRING"))
  if (!is_poly && !is_line) {
    stop("`barrier` must contain only lines or only polygons.", call. = FALSE)
  }
  geom <- sf::st_transform(geom, crs)
  if (is_poly) {
    geom <- sf::st_make_valid(geom)
    lines <- sf::st_boundary(geom)
  } else {
    lines <- geom
  }
  if (is.null(lab)) lab <- as.character(seq_along(geom))
  list(geom = geom, edges = .trk_edges(lines), is_poly = is_poly, label = lab)
}

# Barrier line work as a matrix of edges: x1, y1, x2, y2, bid (feature index).
.trk_edges <- function(lines) {
  out <- list()
  for (b in seq_along(lines)) {
    g <- lines[[b]]
    parts <- if (inherits(g, "LINESTRING")) {
      list(unclass(g))
    } else if (inherits(g, "MULTILINESTRING")) {
      unclass(g)
    } else {
      list()
    }
    for (m in parts) {
      m <- m[, 1:2, drop = FALSE]
      if (nrow(m) < 2L) next
      out[[length(out) + 1L]] <- cbind(m[-nrow(m), , drop = FALSE], m[-1, , drop = FALSE], b)
    }
  }
  if (!length(out)) stop("`barrier` contains no line segments.", call. = FALSE)
  e <- do.call(rbind, out)
  colnames(e) <- c("x1", "y1", "x2", "y2", "bid")
  e
}

# Intersections of step segments with barrier edges. Returns data frame with
# seg (row in the segment table), edge (row in `edges`) and t, the fraction
# (0-1] along the step where the crossing happens. Half-open intervals avoid
# double counting a crossing that falls exactly on a vertex.
.trk_seg_cross <- function(p1x, p1y, p2x, p2y, edges, chunk = 250L) {
  m <- length(p1x)
  empty <- data.frame(seg = integer(0), edge = integer(0), t = numeric(0))
  if (m == 0L) return(empty)
  ex1 <- edges[, "x1"]
  ey1 <- edges[, "y1"]
  ex2 <- edges[, "x2"]
  ey2 <- edges[, "y2"]
  emin_x <- pmin(ex1, ex2)
  emax_x <- pmax(ex1, ex2)
  emin_y <- pmin(ey1, ey2)
  emax_y <- pmax(ey1, ey2)
  res <- list()
  for (s in seq(1L, m, by = chunk)) {
    idx <- s:min(s + chunk - 1L, m)
    n <- length(idx)
    xmin <- min(p1x[idx], p2x[idx])
    xmax <- max(p1x[idx], p2x[idx])
    ymin <- min(p1y[idx], p2y[idx])
    ymax <- max(p1y[idx], p2y[idx])
    keep <- which(emax_x >= xmin & emin_x <= xmax & emax_y >= ymin & emin_y <= ymax)
    if (!length(keep)) next
    rx <- p2x[idx] - p1x[idx]
    ry <- p2y[idx] - p1y[idx]
    sx <- (ex2 - ex1)[keep]
    sy <- (ey2 - ey1)[keep]
    qpx <- outer(-p1x[idx], ex1[keep], "+")   # Q - P, rows = steps, cols = edges
    qpy <- outer(-p1y[idx], ey1[keep], "+")
    denom <- outer(rx, sy) - outer(ry, sx)
    tt <- (qpx * rep(sy, each = n) - qpy * rep(sx, each = n)) / denom
    uu <- (qpx * ry - qpy * rx) / denom
    hit <- denom != 0 & tt > 0 & tt <= 1 & uu >= 0 & uu < 1
    hit[is.na(hit)] <- FALSE
    if (!any(hit)) next
    w <- which(hit, arr.ind = TRUE)
    res[[length(res) + 1L]] <- data.frame(
      seg = idx[w[, 1]], edge = keep[w[, 2]], t = tt[w]
    )
  }
  if (!length(res)) return(empty)
  do.call(rbind, res)
}

#' Detect river, road or habitat-boundary crossings
#'
#' Finds every point where an animal's path (straight lines between
#' consecutive fixes) crosses a barrier. The barrier can be lines (rivers,
#' roads, fences) or polygons (habitat patches, protected areas, whose
#' boundaries are used). Crossing times are interpolated along each step.
#'
#' For polygon barriers the `direction` column says whether the animal was
#' entering or exiting the polygon. For line barriers it is `NA`.
#'
#' @inheritParams track_residence_time
#' @param barrier An `sf`/`sfc` object containing only lines or only polygons
#'   (any CRS).
#' @param barrier_name Optional column of `barrier` holding a label for each
#'   feature (e.g. a road name).
#' @return An `sf` data frame of crossing points with columns `id`,
#'   `barrier_id`, `time` (interpolated), `t_from`, `t_to` (the fixes either
#'   side), `direction`, `lon`, `lat`. Count crossings with, for example,
#'   `table(res$id, res$barrier_id)`.
#' @examples
#' \dontrun{
#' river <- sf::st_sfc(sf::st_linestring(cbind(c(80.380, 80.395),
#'                                              c(7.290, 7.310))), crs = 4326)
#' cr <- track_crossings(trk, river, max_gap = 6)
#' table(cr$id)
#' }
#' @export
track_crossings <- function(x, barrier, id = NULL, time = NULL, lon = NULL, lat = NULL,
                            barrier_name = NULL, max_gap = NULL,
                            units = c("hours", "mins", "secs", "days"),
                            crs = NULL, crs_out = NULL) {
  units <- match.arg(units)
  p <- .trk_prep(x, id, time, lon, lat, crs)
  d <- p$data
  crs <- p$crs
  bar <- .trk_barrier(barrier, crs, barrier_name)
  
  empty <- sf::st_sf(
    data.frame(id = character(0), barrier_id = character(0), time = d$time[0],
               t_from = d$time[0], t_to = d$time[0], direction = character(0),
               lon = numeric(0), lat = numeric(0), stringsAsFactors = FALSE),
    geometry = sf::st_sfc(crs = crs)
  )
  
  seg <- .trk_segments(d, max_gap, units)
  if (is.null(seg)) return(.trk_out(empty, crs_out))
  seg <- seg[seg$valid, , drop = FALSE]
  cr <- .trk_seg_cross(seg$x1, seg$y1, seg$x2, seg$y2, bar$edges)
  if (!nrow(cr)) {
    message("No crossings detected.")
    return(.trk_out(empty, crs_out))
  }
  
  bid <- bar$edges[cr$edge, "bid"]
  cr <- cr[order(cr$seg, bid, cr$t), , drop = FALSE]
  bid <- bar$edges[cr$edge, "bid"]
  s <- seg[cr$seg, , drop = FALSE]
  cx <- s$x1 + cr$t * (s$x2 - s$x1)
  cy <- s$y1 + cr$t * (s$y2 - s$y1)
  
  direction <- rep(NA_character_, nrow(cr))
  if (bar$is_poly) {
    inside0 <- sf::st_intersects(.trk_points_sfc(s$x1, s$y1, crs), bar$geom, sparse = FALSE)
    start_in <- inside0[cbind(seq_len(nrow(cr)), bid)]
    j <- stats::ave(seq_along(cr$seg), cr$seg, bid, FUN = seq_along)
    before_in <- ifelse(j %% 2L == 1L, start_in, !start_in)
    direction <- ifelse(before_in, "exit", "enter")
  }
  
  pts <- .trk_points_sfc(cx, cy, crs)
  ll <- sf::st_coordinates(sf::st_transform(pts, 4326))
  out <- data.frame(
    id = s$id,
    barrier_id = bar$label[bid],
    time = s$t1 + cr$t * s$dt,
    t_from = s$t1,
    t_to = s$t2,
    direction = direction,
    lon = ll[, 1],
    lat = ll[, 2],
    stringsAsFactors = FALSE
  )
  o <- order(out$id, out$time)
  res <- sf::st_sf(out[o, , drop = FALSE], geometry = pts[o])
  rownames(res) <- NULL
  .trk_out(res, crs_out)
}