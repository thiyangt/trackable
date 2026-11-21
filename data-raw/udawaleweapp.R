# =====================================================================
# Elephant movement data generator (1 year) - R Shiny app, version 2
#  * mean + SD for the annual metrics (drawn per simulated elephant)
#  * habitats are drawn as paths / points on a map
#  * behaviour-state correlated random walk (turning angle, speed by
#    time period) with foraging, travelling, resting, bathing, sleeping
# Packages: shiny, ggplot2, DT, leaflet, leaflet.extras, MASS
#   install.packages(c("shiny","ggplot2","DT","leaflet","leaflet.extras"))
# Run:  shiny::runApp("app.R")
# =====================================================================
library(shiny)
library(ggplot2)
library(DT)
library(leaflet)
library(leaflet.extras)

HABS   <- c("Dense forest", "Grassland", "Scrub", "Cropland edge", "Riverine corridor")
HABCOL <- c("Dense forest" = "#1b5e20", "Grassland" = "#c0ca33", "Scrub" = "#8d6e63",
            "Cropland edge" = "#f9a825", "Riverine corridor" = "#0288d1")
BEHS   <- c("Foraging", "Travelling", "Resting", "Bathing", "Sleeping")
BEHCOL <- c("Foraging" = "#2e7d32", "Travelling" = "#d84315", "Resting" = "#6d4c41",
            "Bathing" = "#0277bd", "Sleeping" = "#5e35b1")
MONTHS <- month.abb
NPER   <- 6

period_defaults <- data.frame(
  name = c("Night", "Pre-dawn", "Morning", "Midday", "Afternoon", "Evening"),
  from = c(22, 4, 7, 11, 15, 18), to = c(4, 7, 11, 15, 18, 22),
  v  = c(1.0, 1.5, 1.2, 0.8, 1.4, 1.8), vs = c(0.4, 0.5, 0.4, 0.3, 0.5, 0.6),
  tm = c(40, 15, 25, 35, 15, 20),       ts = c(60, 30, 40, 50, 30, 35),
  stringsAsFactors = FALSE)
beh_defaults <- rbind(c(10, 0, 15, 0, 75),   # Night
                      c(50, 35, 10, 5, 0),   # Pre-dawn
                      c(70, 15, 10, 5, 0),   # Morning
                      c(30, 5, 45, 15, 5),   # Midday
                      c(60, 25, 10, 5, 0),   # Afternoon
                      c(75, 20, 5, 0, 0))    # Evening
hr_defaults <- c(70, 65, 60, 55, 50, 45, 45, 50, 60, 85, 100, 90)

# ---------------------------- helpers --------------------------------
to_xy <- function(lon, lat, lon0, lat0) {
  cbind((lon - lon0) * 111.320 * cos(lat0 * pi / 180), (lat - lat0) * 110.574)
}
from_xy <- function(x, y, lon0, lat0) {
  cbind(lon0 + x / (111.320 * cos(lat0 * pi / 180)), lat0 + y / 110.574)
}
poly_area <- function(x, y) {
  n <- length(x); j <- c(2:n, 1)
  abs(sum(x * y[j] - x[j] * y)) / 2
}
mcp95 <- function(x, y, pct = 0.95) {
  if (length(x) < 5) return(NA_real_)
  d <- sqrt((x - mean(x))^2 + (y - mean(y))^2)
  keep <- d <= quantile(d, pct)
  xs <- x[keep]; ys <- y[keep]
  h <- chull(xs, ys)
  if (length(h) < 3) return(NA_real_)
  poly_area(xs[h], ys[h])
}
kde_core <- function(x, y, pct = 0.5) {
  if (length(x) < 10 || !requireNamespace("MASS", quietly = TRUE)) return(NA_real_)
  k  <- MASS::kde2d(x, y, n = 120)
  dx <- diff(k$x[1:2]); dy <- diff(k$y[1:2])
  z  <- sort(as.vector(k$z), decreasing = TRUE)
  sum(cumsum(z) / sum(z) <= pct) * dx * dy
}
rtn <- function(mean, sd, lo, hi) {            # truncated (clipped) normal draw
  if (is.na(sd) || sd <= 0) return(min(max(mean, lo), hi))
  min(max(rnorm(1, mean, sd), lo), hi)
}
densify <- function(m, sp = 0.1) {             # points every ~100 m along a path
  if (nrow(m) < 2) return(m)
  out <- m[1, , drop = FALSE]
  for (i in 2:nrow(m)) {
    a <- m[i - 1, ]; b <- m[i, ]
    n <- max(1, ceiling(sqrt(sum((b - a)^2)) / sp)); t <- (1:n) / n
    out <- rbind(out, cbind(a[1] + (b[1] - a[1]) * t, a[2] + (b[2] - a[2]) * t))
  }
  out
}
season_probs <- function(fo, gr, cr, rv) {
  p <- c(fo, gr, NA, cr, rv) / 100
  p[3] <- max(0, 1 - sum(p[-3]))               # scrub = remainder
  p / sum(p)
}
adjust_probs <- function(pw, pd, nw, nd, tc, tr) {
  a <- if (nw + nd > 0) (nw * pw + nd * pd) / (nw + nd) else pw
  f <- function(p) {
    if (a[4] > 0) p[4] <- p[4] * tc / a[4]
    if (a[5] > 0) p[5] <- p[5] * tr / a[5]
    s45 <- p[4] + p[5]
    if (s45 > 0.95) p[4:5] <- p[4:5] * 0.95 / s45
    o <- p[1:3]
    p[1:3] <- if (sum(o) > 0) o * (1 - p[4] - p[5]) / sum(o) else c(1, 1, 1) / 3 * (1 - p[4] - p[5])
    p / sum(p)
  }
  list(wet = f(pw), dry = f(pd))
}
slot_probs <- function(p, is_night, wn, wd) {  # push cropland use toward night/day
  w  <- if (is_night) wn else wd
  pc <- min(p[4] * w, 0.98)
  q  <- p; s <- sum(p[-4])
  q[4]  <- pc
  q[-4] <- if (s > 0) p[-4] * (1 - pc) / s else 0
  q / sum(q)
}
hour_lookup_fn <- function(from, to) {
  sapply(0:23, function(h) {
    for (i in seq_along(from)) {
      f <- from[i]; t <- to[i]
      inside <- if (f < t) (h >= f && h < t) else if (f > t) (h >= f || h < t) else TRUE
      if (inside) return(i)
    }
    1L
  })
}
norm_beh <- function(B) {
  B <- pmax(B, 0)
  z <- rowSums(B)
  B[z == 0, ] <- matrix(c(1, 0, 0, 0, 0), sum(z == 0), 5, byrow = TRUE)
  B / rowSums(B)
}
expected_dist <- function(v, B, hl, ffac) {    # km/day implied by the speeds
  sum(sapply(0:23, function(h) { p <- hl[h + 1]; (B[p, 1] * ffac + B[p, 2]) * v[p] }))
}
build_geometry <- function(paths, fb_lon, fb_lat) {
  raw <- lapply(1:5, function(h) {
    pm <- Filter(function(p) p$habitat == HABS[h], paths)
    if (length(pm)) lapply(pm, function(p) p$coords) else list(matrix(c(fb_lon[h], fb_lat[h]), 1))
  })
  used_fb <- sapply(1:5, function(h) !any(sapply(paths, function(p) p$habitat == HABS[h])))
  allpts <- do.call(rbind, unlist(raw, recursive = FALSE))
  lon0 <- mean(allpts[, 1]); lat0 <- mean(allpts[, 2])
  H <- lapply(raw, function(lst)
    do.call(rbind, lapply(lst, function(m) densify(to_xy(m[, 1], m[, 2], lon0, lat0)))))
  list(lon0 = lon0, lat0 = lat0, H = H, raw = raw, used_fb = used_fb)
}

# --------------------------- simulation ------------------------------
simulate_herd <- function(a, tick = function() NULL) {
  set.seed(a$seed)
  dt_min <- a$step_min; dt_h <- dt_min / 60
  spd    <- round(1440 / dt_min)
  dates  <- seq(as.Date(sprintf("%d-01-01", a$year)), as.Date(sprintf("%d-12-31", a$year)), by = "day")
  nday   <- length(dates)
  mon_all <- as.integer(format(dates, "%m"))
  wet_idx <- match(a$wet_months, MONTHS)
  nw <- sum(mon_all %in% wet_idx); nd <- nday - nw

  g <- build_geometry(a$paths, a$fb_lon, a$fb_lat)
  H <- g$H
  cent <- t(sapply(H, colMeans))

  B  <- norm_beh(a$B)
  hl <- hour_lookup_fn(a$per$from, a$per$to)
  vm <- a$per$v; vs <- a$per$vs
  if (a$calib) {
    E <- expected_dist(vm, B, hl, a$ffac)
    if (E > 0) { k <- a$dist_m / E; vm <- vm * k; vs <- vs * k }
  }
  pw0 <- season_probs(a$wet[1], a$wet[2], a$wet[3], a$wet[4])
  pd0 <- season_probs(a$dry[1], a$dry[2], a$dry[3], a$dry[4])

  all_fix <- list(); all_day <- list(); all_el <- list()
  for (e in seq_len(a$nele)) {
    eid <- sprintf("E%02d", e)
    tg <- list(mcp   = rtn(a$mcp_m,   a$mcp_s,   1, Inf),
               kde   = rtn(a$kde_m,   a$kde_s,   0.5, Inf),
               crop  = rtn(a$crop_m,  a$crop_s,  0, 95),
               noct  = rtn(a$noct_m,  a$noct_s,  0, 100),
               river = rtn(a$river_m, a$river_s, 0, 95))
    tg$kde <- min(tg$kde, tg$mcp)
    pw <- pw0; pd <- pd0
    if (a$force) {
      adj <- adjust_probs(pw0, pd0, nw, nd, tg$crop / 100, tg$river / 100)
      pw <- adj$wet; pd <- adj$dry
    }
    noct <- tg$noct / 100
    wn <- noct / 0.5; wd <- (1 - noct) / 0.5                # night = 18:00-06:00 = half of day
    Ra <- sqrt(tg$mcp / pi); Rc <- sqrt(tg$kde / pi)
    Rm_vec <- pmin(sqrt(a$hr / pi), Ra)

    pick_target <- function(h, pos, ctr, beh) {
      P <- H[[h]]; cand <- seq_len(nrow(P))
      if (runif(1) < 0.5) {                                  # half of bouts stay in the KDE core
        inc <- which((P[, 1] - ctr[1])^2 + (P[, 2] - ctr[2])^2 <= Rc^2)
        if (length(inc)) cand <- inc
      }
      if (beh == 1 && length(cand) > 1) {                    # foraging = local along the path
        d2 <- (P[cand, 1] - pos[1])^2 + (P[cand, 2] - pos[2])^2
        cand <- cand[order(d2)[seq_len(min(12, length(cand)))]]
      }
      j <- if (length(cand) == 1) cand else sample(cand, 1)
      P[j, ] + rnorm(2, 0, 0.03)
    }

    n <- nday * spd
    X <- numeric(n); Y <- numeric(n)
    BEH <- integer(n); HAB <- integer(n); PER <- integer(n)

    h0  <- sample.int(5, 1, prob = pd)
    pos <- H[[h0]][sample.int(nrow(H[[h0]]), 1), ]
    heading <- runif(1, 0, 2 * pi)
    beh <- sample.int(5, 1, prob = B[hl[1], ]); hab_cur <- h0; tgt <- pos; need <- TRUE
    cnt <- matrix(0, 2, 5)                                    # fixes per habitat: row 1 wet, row 2 dry

    for (d in seq_len(nday)) {
      m   <- mon_all[d]
      si  <- if (m %in% wet_idx) 1L else 2L
      p   <- if (si == 1L) pw else pd
      Rm  <- Rm_vec[m]
      ctr <- c(sum(p * cent[, 1]), sum(p * cent[, 2]))
      Dt  <- max(0.1, rnorm(1, a$disp_m, a$disp_s))          # this day's net-displacement cap
      i0  <- (d - 1) * spd
      for (s in seq_len(spd)) {
        i  <- i0 + s
        hr <- (s - 1) * dt_h
        per <- hl[floor(hr) + 1]
        is_night <- hr >= 18 || hr < 6
        bp <- B[per, ]
        if (bp[beh] == 0 || runif(1) > exp(-dt_min / a$bout[beh])) {   # behaviour bout ends
          beh <- sample.int(5, 1, prob = bp); need <- TRUE
        }
        if (beh <= 2) {
          if (need || sqrt(sum((tgt - pos)^2)) < c(0.1, 0.3)[beh]) {
            q <- slot_probs(p, is_night, wn, wd)
            tot <- sum(cnt[si, ])
            if (a$gain > 0 && tot > 50) {                     # steer realized share toward the entered share
              q <- pmax(q + a$gain * (p - cnt[si, ] / tot), 0)
              q <- if (sum(q) > 0) q / sum(q) else p
            }
            hab_cur <- sample.int(5, 1, prob = q)
            tgt <- pick_target(hab_cur, pos, ctr, beh); need <- FALSE
          }
        } else if (beh == 4) {
          if (need) {
            P <- H[[5]]; j <- which.min((P[, 1] - pos[1])^2 + (P[, 2] - pos[2])^2)
            hab_cur <- 5L; tgt <- P[j, ]; need <- FALSE
          }
        } else need <- FALSE

        d_t <- sqrt(sum((tgt - pos)^2))
        if (beh <= 2 || (beh == 4 && d_t > 0.08)) {
          sp <- max(0, rnorm(1, vm[per], vs[per]))
          if (beh == 1) sp <- sp * a$ffac
          tf <- if (beh == 1) 1 else if (beh == 2) a$tfac else 0
          sgn <- if (a$rsign) sample(c(-1, 1), 1) else 1
          heading <- heading + tf * (sgn * a$per$tm[per] + rnorm(1, 0, a$per$ts[per])) * pi / 180
          w <- if (beh == 1) a$bf else a$bt
          bearing <- atan2(tgt[2] - pos[2], tgt[1] - pos[1])
          heading <- atan2((1 - w) * sin(heading) + w * sin(bearing),
                           (1 - w) * cos(heading) + w * cos(bearing))
          st  <- min(sp * dt_h, d_t)
          pos <- pos + st * c(cos(heading), sin(heading))
          dd <- pos - ctr; r <- sqrt(sum(dd^2))
          if (r > Rm && r > 0) {
            pos <- ctr + dd / r * Rm
            heading <- atan2(ctr[2] - pos[2], ctr[1] - pos[1])
          }
          X[i] <- pos[1]; Y[i] <- pos[2]
        } else {                                              # stationary: tiny GPS jitter
          X[i] <- pos[1] + rnorm(1, 0, 0.003); Y[i] <- pos[2] + rnorm(1, 0, 0.003)
        }
        BEH[i] <- beh; HAB[i] <- hab_cur; PER[i] <- per
        cnt[si, hab_cur] <- cnt[si, hab_cur] + 1
      }
      i1 <- i0 + 1; i2 <- i0 + spd
      net <- sqrt((X[i2] - X[i1])^2 + (Y[i2] - Y[i1])^2)
      if (net > Dt) {                                         # enforce daily displacement
        k <- Dt / net; idx <- i1:i2
        X[idx] <- X[i1] + (X[idx] - X[i1]) * k
        Y[idx] <- Y[i1] + (Y[idx] - Y[i1]) * k
        pos <- c(X[i2], Y[i2])
      }
    }

    ll  <- from_xy(X, Y, g$lon0, g$lat0)
    hrs <- rep((0:(spd - 1)) * dt_h, nday)
    dts <- rep(dates, each = spd)
    mon <- rep(mon_all, each = spd)
    stp <- c(0, sqrt(diff(X)^2 + diff(Y)^2))
    df <- data.frame(
      elephant = eid, fix_id = seq_len(n),
      datetime = as.POSIXct(as.character(dts), tz = "UTC") + hrs * 3600,
      date = dts, month = mon, month_name = factor(MONTHS[mon], levels = MONTHS),
      season = ifelse(mon %in% wet_idx, "Wet", "Dry"), hour = hrs,
      period = factor(a$per$name[PER], levels = a$per$name),
      night = hrs >= 18 | hrs < 6,
      behaviour = factor(BEHS[BEH], levels = BEHS),
      habitat = factor(HABS[HAB], levels = HABS),
      lon = round(ll[, 1], 6), lat = round(ll[, 2], 6),
      x_km = X, y_km = Y, step_km = stp, speed_kmh = stp / dt_h)

    Xm <- matrix(X, nrow = spd); Ym <- matrix(Y, nrow = spd)
    dist_d <- colSums(matrix(stp, nrow = spd))
    disp_d <- sqrt((Xm[spd, ] - Xm[1, ])^2 + (Ym[spd, ] - Ym[1, ])^2)
    dy <- data.frame(elephant = eid, date = dates, month = mon_all,
                     season = ifelse(mon_all %in% wet_idx, "Wet", "Dry"),
                     distance_km = dist_d, displacement_km = disp_d)
    el <- data.frame(elephant = eid,
                     mcp_target = tg$mcp,   mcp_real = mcp95(X, Y),
                     kde_target = tg$kde,   kde_real = kde_core(X, Y),
                     crop_target = tg$crop, crop_real = 100 * mean(HAB == 4),
                     noct_target = tg$noct, noct_real = if (any(HAB == 4)) 100 * mean(df$night[HAB == 4]) else NA_real_,
                     river_target = tg$river, river_real = 100 * mean(HAB == 5),
                     disp_real = mean(disp_d), dist_real = mean(dist_d))
    all_fix[[e]] <- df; all_day[[e]] <- dy; all_el[[e]] <- el
    tick()
  }
  list(fixes = do.call(rbind, all_fix), daily = do.call(rbind, all_day),
       elephants = do.call(rbind, all_el), geom = g, dates = dates, args = a,
       speeds = list(v = vm, vs = vs))
}

# ------------------------------- UI ----------------------------------
coord_row <- function(id, label, lon, lat) {
  fluidRow(column(4, strong(label)),
           column(4, numericInput(paste0("lon_", id), NULL, lon, step = 0.001)),
           column(4, numericInput(paste0("lat_", id), NULL, lat, step = 0.001)))
}
ms_row <- function(id, label, m, s = NULL, step = 0.1) {
  fluidRow(column(6, strong(label)),
           column(3, numericInput(paste0(id, "_m"), NULL, m, step = step)),
           column(3, if (!is.null(s)) numericInput(paste0(id, "_s"), NULL, s, min = 0, step = step) else em("none")))
}
nin <- function(id, v, step = 0.1, min = NA) numericInput(id, NULL, v, step = step, min = min, width = "100%")

period_ui <- function() {
  hdr <- fluidRow(column(2, strong("Period")), column(1, strong("From h")), column(1, strong("To h")),
                  column(2, strong("Speed (km/h)")), column(2, strong("Speed SD")),
                  column(2, strong("Mean turn (\u00b0)")), column(2, strong("Turn SD (\u00b0)")))
  rows <- lapply(1:NPER, function(i) {
    d <- period_defaults[i, ]
    fluidRow(column(2, textInput(paste0("p_name_", i), NULL, d$name)),
             column(1, nin(paste0("p_from_", i), d$from, 1, 0)),
             column(1, nin(paste0("p_to_", i), d$to, 1, 0)),
             column(2, nin(paste0("p_v_", i), d$v, 0.1, 0)),
             column(2, nin(paste0("p_vs_", i), d$vs, 0.1, 0)),
             column(2, nin(paste0("p_tm_", i), d$tm, 5)),
             column(2, nin(paste0("p_ts_", i), d$ts, 5, 0)))
  })
  do.call(tagList, c(list(hdr), rows))
}
beh_ui <- function() {
  hdr <- do.call(fluidRow, c(list(column(2, strong("Period (%)"))),
                             lapply(BEHS, function(b) column(2, strong(b)))))
  rows <- lapply(1:NPER, function(i)
    do.call(fluidRow, c(list(column(2, textOutput(paste0("p_lab_", i)))),
                        lapply(1:5, function(j) column(2, nin(paste0("b_", i, "_", j), beh_defaults[i, j], 5, 0))))))
  do.call(tagList, c(list(hdr), rows))
}

ui <- fluidPage(
  titlePanel("Elephant movement data generator (1 year)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Sampling"),
      numericInput("year", "Year", 2026, 1990, 2100, 1),
      selectInput("step_min", "Fix interval (minutes)", c(15, 30, 60, 120), selected = 60),
      numericInput("nele", "Number of elephants", 1, 1, 10, 1),
      numericInput("seed", "Random seed", 42, 1),
      checkboxInput("force", "Rescale seasonal habitat use to the annual cropland-edge / riverine % (blurs wet vs dry)", FALSE),
      checkboxInput("rsign", "Randomise sign of mean turning angle (left/right)", TRUE),
      checkboxInput("calib", "Scale speeds so expected daily distance = mean daily distance", FALSE),
      actionButton("go", "Generate data", class = "btn-primary", width = "100%"),
      br(), br(),
      helpText("1) set metrics, 2) draw habitat paths on the map, 3) set movement / behaviour, then press Generate.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("1 Annual metrics",
          br(), fluidRow(column(6, ""), column(3, strong("Mean")), column(3, strong("SD"))),
          ms_row("disp",  "Daily displacement (km) - day to day", 3.0, 1.2),
          ms_row("dist",  "Mean daily distance (km)", 8, NULL),
          ms_row("mcp",   "Annual 95% MCP (km\u00b2) - between elephants", 400, 60, 5),
          ms_row("kde",   "Core area KDE 50% (km\u00b2) - between elephants", 60, 10, 1),
          ms_row("crop",  "Cropland-edge fixes (%) - between elephants", 18, 4, 1),
          ms_row("noct",  "Nocturnal cropland fixes (% of cropland fixes)", 75, 8, 1),
          ms_row("river", "Riverine corridor use (%) - between elephants", 20, 4, 1),
          helpText("Daily displacement SD varies from day to day. For the other SDs, each simulated elephant draws its own annual target from Normal(mean, SD)."),
          h4("Monthly home range (km\u00b2)"),
          fluidRow(lapply(1:12, function(i) column(2, numericInput(paste0("hr_", i), MONTHS[i], hr_defaults[i], min = 1))))
        ),
        tabPanel("2 Seasons & habitat use",
          br(),
          checkboxGroupInput("wet_months", "Wet months", choices = MONTHS,
                             selected = c("Oct", "Nov", "Dec", "Jan"), inline = TRUE),
          div(strong("Dry months: "), textOutput("dry_txt", inline = TRUE)),
          h4("Habitat use by season (% of foraging/travelling bouts)"),
          p(em("Scrub = remainder (100 minus the four below).")),
          fluidRow(
            column(4, strong("Wet season"),
                   numericInput("wet_forest", "Dense forest", 35, 0, 100),
                   numericInput("wet_grass",  "Grassland", 25, 0, 100),
                   numericInput("wet_crop",   "Cropland edge", 15, 0, 100),
                   numericInput("wet_river",  "Riverine corridor", 10, 0, 100)),
            column(4, strong("Dry season"),
                   numericInput("dry_forest", "Dense forest", 25, 0, 100),
                   numericInput("dry_grass",  "Grassland", 15, 0, 100),
                   numericInput("dry_crop",   "Cropland edge", 20, 0, 100),
                   numericInput("dry_river",  "Riverine corridor", 25, 0, 100))),
          textOutput("scrub_txt"),
          numericInput("gain", "Habitat-use feedback strength (0 = off, higher = realized shares follow the values above more tightly)", 4, 0, 20, 0.5),
          helpText("The wet and dry shares above are the targets for each season; the simulation steers toward them within each season. Bathing time counts as Riverine corridor, so keep bathing budgets below your riverine %. The annual cropland-edge / riverine inputs only take effect if the rescale box in the sidebar is ticked.")
        ),
        tabPanel("3 Habitat paths (map)",
          br(),
          fluidRow(
            column(4,
              radioButtons("active_hab", "Habitat to draw",
                           choiceNames = lapply(HABS, function(h) tags$span(style = paste0("color:", HABCOL[[h]], ";font-weight:bold"), h)),
                           choiceValues = HABS),
              helpText("Pick a habitat, then use the line tool (or marker) on the map. Every vertex and the points along each line become locations the elephant can use. Use the edit/trash tools on the map to change or delete paths."),
              actionButton("clear_all", "Clear all paths"),
              actionButton("recentre", "Centre on fallback points"),
              br(), br(), DTOutput("path_tab"),
              tags$details(tags$summary("Fallback point locations (used only for habitats with no drawn path)"),
                           fluidRow(column(4, ""), column(4, strong("Lon")), column(4, strong("Lat"))),
                           coord_row("forest", "Dense forest", 80.880, 8.050),
                           coord_row("grass",  "Grassland", 80.900, 8.020),
                           coord_row("scrub",  "Scrub", 80.920, 8.000),
                           coord_row("crop",   "Cropland edge", 80.870, 7.985),
                           coord_row("river",  "Riverine corridor", 80.940, 8.030))),
            column(8, leafletOutput("map", height = 620)))
        ),
        tabPanel("4 Movement & behaviour",
          br(), h4("Random-walk parameters by time period (moving speed, turning angle)"),
          period_ui(),
          helpText("Turning angle ~ Normal(mean, SD) added to the current heading. With 'randomise sign' on, the mean is applied as a left or right turn at random; a mean of 0 gives a plain correlated random walk."),
          textOutput("exp_dist"),
          h4("Behaviour budget by period (% of time, normalised per row)"),
          beh_ui(),
          h4("Mean bout duration (minutes)"),
          fluidRow(lapply(1:5, function(j) column(2, numericInput(paste0("bout_", j), BEHS[j], c(90, 60, 60, 45, 180)[j], min = 5, step = 5)))),
          h4("Movement modifiers"),
          fluidRow(
            column(3, numericInput("ffac", "Foraging speed factor (x speed)", 0.4, 0, 2, 0.05)),
            column(3, numericInput("tfac", "Travelling turn factor (x angles)", 0.4, 0, 2, 0.05)),
            column(3, numericInput("bf", "Pull toward target - foraging (0-1)", 0.3, 0, 1, 0.05)),
            column(3, numericInput("bt", "Pull toward target - travelling/bathing (0-1)", 0.7, 0, 1, 0.05))),
          helpText("Resting and sleeping are stationary; bathing walks to the nearest Riverine corridor point and stays there.")
        ),
        tabPanel("Track map",
          br(),
          fluidRow(
            column(3, selectInput("show_ele", "Elephant", choices = "E01")),
            column(3, selectInput("color_by", "Colour fixes by", c("Behaviour", "Habitat", "Season", "Month"))),
            column(6, sliderInput("days", "Day of year range", 1, 366, c(1, 30), step = 1))),
          plotOutput("track", height = 580)),
        tabPanel("Targets vs realized", br(), DTOutput("valid"), br(), h4("Per elephant"), DTOutput("ele_tab")),
        tabPanel("Monthly", br(), DTOutput("monthly"), plotOutput("hr_plot", height = 300)),
        tabPanel("Wet vs dry habitat use",
          plotOutput("hab_month", height = 400), br(),
          h4("Entered (target) vs realized % of fixes, by season"), DTOutput("hab_cmp"), br(),
          plotOutput("hab_plot", height = 320)),
        tabPanel("Behaviour",
          plotOutput("beh_plot", height = 340), DTOutput("beh_tab")),
        tabPanel("Daily movement", plotOutput("daily_plot", height = 460)),
        tabPanel("Data", br(),
          downloadButton("dl_fix", "Fixes (CSV)"), downloadButton("dl_day", "Daily summary (CSV)"),
          downloadButton("dl_paths", "Habitat paths (CSV)"), br(), br(), DTOutput("fix_tab"))
      )
    )
  )
)

# ----------------------------- server --------------------------------
add_toolbar <- function(map, hab) {
  addDrawToolbar(map, targetGroup = "drawn",
    polylineOptions = drawPolylineOptions(shapeOptions = drawShapeOptions(color = HABCOL[[hab]], weight = 5, opacity = 0.95)),
    markerOptions = drawMarkerOptions(),
    polygonOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE,
    circleMarkerOptions = FALSE, editOptions = editToolbarOptions())
}
parse_feat <- function(f) {
  g <- f$geometry; co <- g$coordinates
  m <- if (identical(g$type, "Point")) matrix(as.numeric(unlist(co))[1:2], ncol = 2, byrow = TRUE)
       else do.call(rbind, lapply(co, function(z) as.numeric(unlist(z))[1:2]))
  colnames(m) <- c("lon", "lat"); m
}
feat_id <- function(f) as.character(f$properties[["_leaflet_id"]])

server <- function(input, output, session) {
  rv <- reactiveValues(paths = list())

  # ---- map & drawing ----
  output$map <- renderLeaflet({
    lon <- isolate(mean(c(input$lon_forest, input$lon_grass, input$lon_scrub, input$lon_crop, input$lon_river)))
    lat <- isolate(mean(c(input$lat_forest, input$lat_grass, input$lat_scrub, input$lat_crop, input$lat_river)))
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      setView(lng = lon, lat = lat, zoom = 12) %>%
      addLayersControl(baseGroups = c("OpenStreetMap", "Satellite"), overlayGroups = "drawn",
                       options = layersControlOptions(collapsed = FALSE)) %>%
      add_toolbar(HABS[1])
  })
  observeEvent(input$active_hab, {
    leafletProxy("map") %>% removeDrawToolbar(clearFeatures = FALSE) %>% add_toolbar(input$active_hab)
  }, ignoreInit = TRUE)
  observeEvent(input$map_draw_new_feature, {
    f <- input$map_draw_new_feature
    id <- feat_id(f); if (length(id) == 0) id <- paste0("p", length(rv$paths) + 1)
    rv$paths[[id]] <- list(habitat = input$active_hab, coords = parse_feat(f))
  })
  observeEvent(input$map_draw_edited_features, {
    for (f in input$map_draw_edited_features$features) {
      id <- feat_id(f)
      if (length(id) && !is.null(rv$paths[[id]])) rv$paths[[id]]$coords <- parse_feat(f)
    }
  })
  observeEvent(input$map_draw_deleted_features, {
    for (f in input$map_draw_deleted_features$features) {
      id <- feat_id(f); if (length(id)) rv$paths[[id]] <- NULL
    }
  })
  observeEvent(input$clear_all, {
    rv$paths <- list(); leafletProxy("map") %>% clearGroup("drawn")
  })
  observeEvent(input$recentre, {
    leafletProxy("map") %>% setView(mean(c(input$lon_forest, input$lon_grass, input$lon_scrub, input$lon_crop, input$lon_river)),
                                    mean(c(input$lat_forest, input$lat_grass, input$lat_scrub, input$lat_crop, input$lat_river)), 12)
  })
  output$path_tab <- renderDT({
    ref <- c(80.9, 8.0)
    tab <- do.call(rbind, lapply(HABS, function(h) {
      pm <- Filter(function(p) p$habitat == h, rv$paths)
      len <- sum(sapply(pm, function(p) {
        if (nrow(p$coords) < 2) return(0)
        xy <- to_xy(p$coords[, 1], p$coords[, 2], ref[1], ref[2])
        sum(sqrt(rowSums(diff(xy)^2)))
      }))
      data.frame(Habitat = h, Features = length(pm), `Length (km)` = round(len, 2),
                 Source = if (length(pm)) "drawn" else "fallback point", check.names = FALSE)
    }))
    datatable(tab, rownames = FALSE, options = list(dom = "t"))
  })

  # ---- small UI helpers ----
  output$dry_txt <- renderText({
    d <- setdiff(MONTHS, input$wet_months); if (length(d)) paste(d, collapse = ", ") else "(none)"
  })
  output$scrub_txt <- renderText({
    w <- 100 - (input$wet_forest + input$wet_grass + input$wet_crop + input$wet_river)
    d <- 100 - (input$dry_forest + input$dry_grass + input$dry_crop + input$dry_river)
    paste0("Scrub remainder: wet ", round(w, 1), "%, dry ", round(d, 1), "%",
           if (w < 0 || d < 0) "  -- WARNING: habitat shares exceed 100%, they will be normalised." else "")
  })
  lapply(1:NPER, function(i) output[[paste0("p_lab_", i)]] <- renderText(input[[paste0("p_name_", i)]]))

  per_df <- reactive({
    data.frame(name = sapply(1:NPER, function(i) input[[paste0("p_name_", i)]]),
               from = sapply(1:NPER, function(i) input[[paste0("p_from_", i)]]),
               to   = sapply(1:NPER, function(i) input[[paste0("p_to_", i)]]),
               v    = sapply(1:NPER, function(i) input[[paste0("p_v_", i)]]),
               vs   = sapply(1:NPER, function(i) input[[paste0("p_vs_", i)]]),
               tm   = sapply(1:NPER, function(i) input[[paste0("p_tm_", i)]]),
               ts   = sapply(1:NPER, function(i) input[[paste0("p_ts_", i)]]),
               stringsAsFactors = FALSE)
  })
  beh_mat <- reactive(do.call(rbind, lapply(1:NPER, function(i) sapply(1:5, function(j) input[[paste0("b_", i, "_", j)]]))))
  output$exp_dist <- renderText({
    p <- per_df(); req(all(!is.na(p$from)), all(!is.na(p$to)), all(!is.na(p$v)), !is.na(input$ffac))
    paste0("Expected daily distance implied by these speeds and budgets: ",
           round(expected_dist(p$v, norm_beh(beh_mat()), hour_lookup_fn(p$from, p$to), input$ffac), 1),
           " km/day (before the daily-displacement cap; mean daily distance target = ", input$dist_m, " km).")
  })

  # ---- simulation ----
  sim <- eventReactive(input$go, {
    a <- list(
      disp_m = input$disp_m, disp_s = input$disp_s, dist_m = input$dist_m,
      mcp_m = input$mcp_m, mcp_s = input$mcp_s, kde_m = input$kde_m, kde_s = input$kde_s,
      crop_m = input$crop_m, crop_s = input$crop_s, noct_m = input$noct_m, noct_s = input$noct_s,
      river_m = input$river_m, river_s = input$river_s,
      hr = sapply(1:12, function(i) input[[paste0("hr_", i)]]),
      wet_months = input$wet_months,
      wet = c(input$wet_forest, input$wet_grass, input$wet_crop, input$wet_river),
      dry = c(input$dry_forest, input$dry_grass, input$dry_crop, input$dry_river),
      fb_lon = c(input$lon_forest, input$lon_grass, input$lon_scrub, input$lon_crop, input$lon_river),
      fb_lat = c(input$lat_forest, input$lat_grass, input$lat_scrub, input$lat_crop, input$lat_river),
      paths = reactiveValuesToList(rv)$paths,
      per = per_df(), B = beh_mat(), bout = sapply(1:5, function(j) input[[paste0("bout_", j)]]),
      ffac = input$ffac, tfac = input$tfac, bf = input$bf, bt = input$bt, gain = input$gain,
      rsign = input$rsign, calib = input$calib, force = input$force,
      year = input$year, step_min = as.numeric(input$step_min), nele = input$nele, seed = input$seed)
    shiny::validate(need(!anyNA(c(a$per$from, a$per$to, a$per$v, a$per$vs, a$per$tm, a$per$ts, a$B)),
                         "Fill in every movement / behaviour field."))
    withProgress(message = "Simulating...", value = 0,
                 simulate_herd(a, function() incProgress(1 / a$nele)))
  })
  observeEvent(sim(), {
    updateSelectInput(session, "show_ele", choices = unique(sim()$fixes$elephant))
  })

  # ---- track map ----
  output$track <- renderPlot({
    s <- sim(); req(input$show_ele)
    nd <- length(s$dates); r <- pmin(input$days, nd)
    f <- s$fixes
    f <- f[f$elephant == input$show_ele & f$date >= s$dates[r[1]] & f$date <= s$dates[r[2]], ]
    cv <- switch(input$color_by, Behaviour = "behaviour", Habitat = "habitat", Season = "season", Month = "month_name")
    p <- ggplot(f, aes(lon, lat))
    for (h in 1:5) for (m in s$geom$raw[[h]]) {
      d <- as.data.frame(m); names(d) <- c("lon", "lat")
      p <- p + (if (nrow(d) > 1) geom_path(data = d, aes(lon, lat), inherit.aes = FALSE, colour = HABCOL[[h]], linewidth = 1.6, alpha = 0.45)
                else geom_point(data = d, aes(lon, lat), inherit.aes = FALSE, colour = HABCOL[[h]], shape = 17, size = 4))
    }
    p <- p + geom_path(colour = "grey60", linewidth = 0.2, alpha = 0.6) +
      geom_point(aes(colour = .data[[cv]]), size = 1.1, alpha = 0.8)
    if (cv == "behaviour") p <- p + scale_colour_manual(values = BEHCOL, drop = FALSE)
    if (cv == "habitat")   p <- p + scale_colour_manual(values = HABCOL, drop = FALSE)
    p + coord_quickmap() + theme_minimal(base_size = 13) +
      labs(x = "Longitude", y = "Latitude", colour = input$color_by,
           title = paste0("Fixes for ", input$show_ele, " (thick lines/triangles = habitat paths)"))
  })

  # ---- targets vs realized ----
  output$valid <- renderDT({
    s <- sim(); a <- s$args; e <- s$elephants; d <- s$daily
    sdv <- function(x) if (sum(!is.na(x)) > 1) sd(x, na.rm = TRUE) else NA_real_
    tab <- data.frame(
      Metric = c("Daily displacement (km) [SD across days]", "Mean daily distance (km) [SD across days]",
                 "Annual 95% MCP (km\u00b2)", "Core area 50% KDE (km\u00b2)", "Cropland-edge fixes (%)",
                 "Nocturnal cropland fixes (%)", "Riverine corridor use (%)"),
      `Target mean` = c(a$disp_m, a$dist_m, a$mcp_m, a$kde_m, a$crop_m, a$noct_m, a$river_m),
      `Target SD` = c(a$disp_s, NA, a$mcp_s, a$kde_s, a$crop_s, a$noct_s, a$river_s),
      `Realized mean` = c(mean(d$displacement_km), mean(d$distance_km), mean(e$mcp_real, na.rm = TRUE),
                          mean(e$kde_real, na.rm = TRUE), mean(e$crop_real), mean(e$noct_real, na.rm = TRUE), mean(e$river_real)),
      `Realized SD` = c(sd(d$displacement_km), sd(d$distance_km), sdv(e$mcp_real), sdv(e$kde_real),
                        sdv(e$crop_real), sdv(e$noct_real), sdv(e$river_real)),
      check.names = FALSE)
    num <- sapply(tab, is.numeric); tab[num] <- lapply(tab[num], round, 2)
    datatable(tab, rownames = FALSE, options = list(dom = "t", pageLength = 10))
  })
  output$ele_tab <- renderDT({
    e <- sim()$elephants; num <- sapply(e, is.numeric); e[num] <- lapply(e[num], round, 2)
    datatable(e, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  # ---- monthly ----
  output$monthly <- renderDT({
    s <- sim(); f <- s$fixes; d <- s$daily
    tab <- do.call(rbind, lapply(1:12, function(m) {
      g <- f[f$month == m, ]; dd <- d[d$month == m, ]
      mc <- mean(sapply(split(g, g$elephant), function(z) mcp95(z$x_km, z$y_km)), na.rm = TRUE)
      data.frame(Month = MONTHS[m], Season = g$season[1], `Target HR (km2)` = s$args$hr[m],
                 `Realized MCP95 (km2)` = round(mc, 1),
                 `Mean daily distance (km)` = round(mean(dd$distance_km), 2),
                 `Mean displacement (km)` = round(mean(dd$displacement_km), 2), check.names = FALSE)
    }))
    datatable(tab, rownames = FALSE, options = list(dom = "t", pageLength = 12))
  })
  output$hr_plot <- renderPlot({
    s <- sim(); f <- s$fixes
    rl <- sapply(1:12, function(m) { g <- f[f$month == m, ]
      mean(sapply(split(g, g$elephant), function(z) mcp95(z$x_km, z$y_km)), na.rm = TRUE) })
    long <- rbind(data.frame(Month = factor(MONTHS, levels = MONTHS), Type = "Target", HR = s$args$hr),
                  data.frame(Month = factor(MONTHS, levels = MONTHS), Type = "Realized", HR = rl))
    ggplot(long, aes(Month, HR, fill = Type)) + geom_col(position = "dodge") +
      theme_minimal(base_size = 13) + labs(y = "Home range (km\u00b2)", title = "Monthly home range")
  })

  # ---- behaviour & habitat ----
  output$beh_plot <- renderPlot({
    f <- sim()$fixes
    ggplot(f, aes(floor(hour), fill = behaviour)) + geom_bar(position = "fill") +
      scale_fill_manual(values = BEHCOL) + scale_x_continuous(breaks = seq(0, 23, 3)) +
      theme_minimal(base_size = 13) + labs(x = "Hour of day", y = "Share of fixes", title = "Activity budget through the day")
  })
  output$beh_tab <- renderDT({
    f <- sim()$fixes
    tab <- do.call(rbind, lapply(levels(f$period), function(p) {
      g <- f[f$period == p, ]
      mv <- g$behaviour %in% c("Foraging", "Travelling")
      data.frame(Period = p, `Realized speed, all fixes (km/h)` = round(mean(g$speed_kmh), 3),
                 `Realized speed, moving fixes (km/h)` = round(mean(g$speed_kmh[mv]), 3),
                 `Foraging %` = round(100 * mean(g$behaviour == "Foraging"), 1),
                 `Travelling %` = round(100 * mean(g$behaviour == "Travelling"), 1),
                 `Resting %` = round(100 * mean(g$behaviour == "Resting"), 1),
                 `Bathing %` = round(100 * mean(g$behaviour == "Bathing"), 1),
                 `Sleeping %` = round(100 * mean(g$behaviour == "Sleeping"), 1), check.names = FALSE)
    }))
    datatable(tab, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  output$hab_plot <- renderPlot({
    f <- sim()$fixes
    t <- as.data.frame(prop.table(table(f$season, f$habitat), 1) * 100)
    names(t) <- c("Season", "Habitat", "Percent")
    ggplot(t, aes(Season, Percent, fill = Habitat)) + geom_col() + scale_fill_manual(values = HABCOL) +
      theme_minimal(base_size = 13) + labs(y = "% of fixes", title = "Realized habitat use by season")
  })
  output$hab_month <- renderPlot({
    s <- sim(); f <- s$fixes
    wet <- match(s$args$wet_months, MONTHS)
    t <- as.data.frame(prop.table(table(f$month, f$habitat), 1) * 100)
    names(t) <- c("month", "Habitat", "Percent")
    t$month <- as.integer(as.character(t$month))
    t$Season <- factor(ifelse(t$month %in% wet, "Wet months", "Dry months"), levels = c("Wet months", "Dry months"))
    t$Month <- factor(MONTHS[t$month], levels = MONTHS)
    ggplot(t, aes(Month, Percent, fill = Habitat)) + geom_col(width = 0.85) +
      facet_grid(~Season, scales = "free_x", space = "free_x") +
      scale_fill_manual(values = HABCOL) + theme_minimal(base_size = 13) +
      labs(y = "% of fixes", x = NULL, title = "Habitat use in each month, wet vs dry")
  })
  output$hab_cmp <- renderDT({
    s <- sim(); a <- s$args; f <- s$fixes
    pw <- season_probs(a$wet[1], a$wet[2], a$wet[3], a$wet[4]) * 100
    pd <- season_probs(a$dry[1], a$dry[2], a$dry[3], a$dry[4]) * 100
    rw <- 100 * prop.table(table(factor(f$habitat[f$season == "Wet"], levels = HABS)))
    rd <- 100 * prop.table(table(factor(f$habitat[f$season == "Dry"], levels = HABS)))
    tab <- data.frame(Habitat = HABS,
                      `Wet target %` = round(pw, 1), `Wet realized %` = round(as.numeric(rw), 1),
                      `Dry target %` = round(pd, 1), `Dry realized %` = round(as.numeric(rd), 1),
                      check.names = FALSE)
    datatable(tab, rownames = FALSE, options = list(dom = "t"))
  })

  # ---- daily ----
  output$daily_plot <- renderPlot({
    d <- sim()$daily
    long <- rbind(data.frame(elephant = d$elephant, date = d$date, Metric = "Daily distance", km = d$distance_km),
                  data.frame(elephant = d$elephant, date = d$date, Metric = "Daily displacement", km = d$displacement_km))
    ggplot(long, aes(date, km, colour = Metric)) + geom_line(alpha = 0.4) +
      geom_smooth(se = FALSE, method = "loess", formula = y ~ x) + facet_wrap(~elephant) +
      theme_minimal(base_size = 13) + labs(x = NULL, y = "km", title = "Daily movement over the year")
  })

  # ---- data ----
  fix_cols <- c("elephant", "fix_id", "datetime", "date", "month", "season", "period", "night",
                "behaviour", "habitat", "lon", "lat", "x_km", "y_km", "step_km", "speed_kmh")
  output$fix_tab <- renderDT({
    f <- sim()$fixes[, fix_cols]
    f$step_km <- round(f$step_km, 3); f$speed_kmh <- round(f$speed_kmh, 3)
    datatable(f, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  })
  output$dl_fix <- downloadHandler(
    filename = function() paste0("elephant_fixes_", input$year, ".csv"),
    content = function(file) write.csv(sim()$fixes[, fix_cols], file, row.names = FALSE))
  output$dl_day <- downloadHandler(
    filename = function() paste0("elephant_daily_", input$year, ".csv"),
    content = function(file) write.csv(sim()$daily, file, row.names = FALSE))
  output$dl_paths <- downloadHandler(
    filename = function() "habitat_paths.csv",
    content = function(file) {
      ps <- reactiveValuesToList(rv)$paths
      out <- do.call(rbind, lapply(names(ps), function(id)
        data.frame(path_id = id, habitat = ps[[id]]$habitat, vertex = seq_len(nrow(ps[[id]]$coords)),
                   lon = ps[[id]]$coords[, 1], lat = ps[[id]]$coords[, 2])))
      if (is.null(out)) out <- data.frame(path_id = character(), habitat = character(), vertex = integer(), lon = numeric(), lat = numeric())
      write.csv(out, file, row.names = FALSE)
    })
}

shinyApp(ui, server)
