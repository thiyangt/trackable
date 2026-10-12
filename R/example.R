library(dplyr)
library(sf)
library(tibble)

set.seed(123)

dat <- tibble(
  animal_id = rep(c("E001", "E002", "E003"), each = 4),
  time = rep(
    as.POSIXct("2022-01-01 08:00:00", tz = "UTC") +
      86400 * 0:3,
    times = 3
  ),
  x = runif(12, 80.70, 80.90),
  y = runif(12, 7.80, 8.00),
  Length = rep(c(250, 270, 250), each = 4),
  Sex = rep(c("Female", "Male", "Female"), each = 4)
)

dat
trk <- trackable(
  data = dat,
  time = time,
  x = x,
  y = y,
  key = c("animal_id", "Length", "Sex"),
  crs = 4326
)

trk
