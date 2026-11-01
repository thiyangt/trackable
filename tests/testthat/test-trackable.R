test_that("trackable creates a valid tracking object", {
  
  set.seed(123)
  
  # Create example tracking data
  dat <- tibble::tibble(
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
  
  # Create trackable object
  trk <- trackable(
    data = dat,
    time = time,
    x = x,
    y = y,
    key = c("animal_id", "Length", "Sex"),
    crs = 4326
  )
  
  # 1. Check object class
  expect_s3_class(trk, "trackable")
  
  # 2. Check object creation
  expect_no_error(print(trk))
  
  # 3. Check dimensions
  expect_equal(nrow(trk), 12)
  
  # 4. Check number of animals
  expect_equal(dplyr::n_distinct(dat$animal_id), 3)
  
  # 5. Check number of observations per animal
  expect_equal(
    as.integer(table(dat$animal_id)),
    c(4L, 4L, 4L)
  )
  
  # 6. Check key variables exist in input data
  expect_true(
    all(c("animal_id", "Length", "Sex") %in% names(dat))
  )
  
  # 7. Check time variable
  expect_s3_class(dat$time, "POSIXct")
  
  # 8. Check coordinates are numeric
  expect_type(dat$x, "double")
  expect_type(dat$y, "double")
  
  # 9. Check coordinates are within expected ranges
  expect_true(all(dat$x >= 80.70 & dat$x <= 80.90))
  expect_true(all(dat$y >= 7.80 & dat$y <= 8.00))
  
})
