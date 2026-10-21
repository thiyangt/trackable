
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

# --------------------------------------------------
# 1. Reference locations
# Coordinate order: latitude, longitude
# --------------------------------------------------

core_points <- tibble(
  latitude = c(
    8.149198559216039,
    8.111079123080923,
    8.19560967405455
  ),
  longitude = c(
    80.89827497204487,
    80.90529673594875,
    80.92840060427767
  )
)

other_points <- tibble(
  latitude = c(
    7.995824880951906,
    8.093491,
    8.09141197975179,
    8.142471864004476,
    8.142471864004476,
    8.219373658937203,
    8.206146711164457,
    8.157270445040954
  ),
  longitude = c(
    80.85939301792968,
    80.876235,
    80.89636272524989,
    80.89986053156116,
    80.89986053156116,
    80.99159648374021,
    80.87630365060869,
    80.84663103107437
  )
) |>
  distinct()

# Optional: include the earlier GPS reference coordinates.
# Save them in a CSV with latitude and longitude columns.
#
# previous_points <- read_csv(
#   "elephant_reference_locations.csv",
#   show_col_types = FALSE
# ) |>
#   select(latitude, longitude)
#
# all_reference_points <- bind_rows(
#   core_points, other_points, previous_points
# ) |> distinct()

# --------------------------------------------------
# 2. Simulate correlated hourly movement
# --------------------------------------------------

simulate_elephant <- function(
    start_time = "2024-01-01 00:00:00",
    end_time = "2025-12-31 23:00:00",
    core_points,
    other_points,
    animal_id = "ELE001",
    sex = "female",
    seed = 2024,
    core_step_sd = 180,
    excursion_probability = 0.015,
    rest_probability = 0.15
) {
  
  set.seed(seed)
  
  times <- seq(
    as.POSIXct(start_time, tz = "Asia/Colombo"),
    as.POSIXct(end_time, tz = "Asia/Colombo"),
    by = "hour"
  )
  
  n <- length(times)
  
  # Local coordinate conversion, in metres.
  # Suitable for this relatively small geographic region.
  lat0 <- mean(core_points$latitude)
  metres_per_lon <- 111320 * cos(lat0 * pi / 180)
  metres_per_lat <- 111320
  
  to_xy <- function(points) {
    cbind(
      x = (points$longitude - mean(core_points$longitude)) *
        metres_per_lon,
      y = (points$latitude - mean(core_points$latitude)) *
        metres_per_lat
    )
  }
  
  to_lonlat <- function(x, y) {
    tibble(
      latitude = lat0 + y / metres_per_lat,
      longitude = mean(core_points$longitude) +
        x / metres_per_lon
    )
  }
  
  core_xy <- to_xy(core_points)
  other_xy <- to_xy(other_points)
  
  # Start near one of the core reference points.
  start <- sample(seq_len(nrow(core_xy)), 1)
  position <- core_xy[start, ]
  
  # Persistent movement direction creates a correlated walk.
  velocity <- c(0, 0)
  
  # Excursion state
  excursion_left <- 0
  excursion_target <- position
  
  # Select a core target
  core_target <- core_xy[
    sample(seq_len(nrow(core_xy)), 1),
  ]
  
  xy <- matrix(NA_real_, nrow = n, ncol = 2)
  xy[1, ] <- position
  
  for (i in 2:n) {
    
    # Start an occasional excursion outside the core area.
    if (
      excursion_left == 0 &&
      runif(1) < excursion_probability
    ) {
      excursion_left <- sample(3:18, 1)
      
      excursion_target <- other_xy[
        sample(seq_len(nrow(other_xy)), 1),
      ]
    }
    
    # Select whether the elephant is on an excursion.
    if (excursion_left > 0) {
      target <- excursion_target
      excursion_left <- excursion_left - 1
    } else {
      # Occasionally switch between core locations.
      if (runif(1) < 0.03) {
        core_target <- core_xy[
          sample(seq_len(nrow(core_xy)), 1),
        ]
      }
      
      target <- core_target
    }
    
    if (runif(1) < rest_probability) {
      # Stationary hour
      velocity <- velocity * 0.3
    } else {
      # Attraction towards the target plus correlated noise.
      noise <- rnorm(2, mean = 0, sd = core_step_sd)
      
      velocity <- 0.55 * velocity +
        0.25 * (target - position) +
        noise
      
      # Limit hourly displacement to 1.5 km.
      speed <- sqrt(sum(velocity^2))
      
      if (speed > 1500) {
        velocity <- velocity / speed * 1500
      }
      
      position <- position + velocity
    }
    
    xy[i, ] <- position
  }
  
  locations <- to_lonlat(xy[, 1], xy[, 2])
  
  tibble(
    animal_id = animal_id,
    timestamp = times,
    latitude = locations$latitude,
    longitude = locations$longitude,
    sex = sex,
    year = year(times)
  )
}

# --------------------------------------------------
# 3. Generate two years of hourly data
# --------------------------------------------------

elephant_tracking <- simulate_elephant(
  core_points = core_points,
  other_points = other_points,
  seed = 2024
)

# Check number of records and timestamps
nrow(elephant_tracking)
range(elephant_tracking$timestamp)

# Save the dataset
write_csv(
  elephant_tracking,
  "male_elephant_tracking_2024_2025.csv"
)

# Inspect records
print(elephant_tracking, n = 10)
##############################################################


ggplot() +
  geom_path(
    data = elephant_tracking,
    aes(longitude, latitude, colour = factor(year)),
    linewidth = 0.35,
    alpha = 0.7
  ) +
  geom_point(
    data = core_points,
    aes(longitude, latitude),
    shape = 4,
    size = 4,
    stroke = 1.2
  ) +
  geom_point(
    data = other_points,
    aes(longitude, latitude),
    shape = 2,
    size = 3
  ) +
  coord_equal() +
  scale_colour_viridis_d(name = "Year") +
  labs(
    title = "Simulated Male Elephant Movement",
    subtitle = "Sri Lanka | 2024–2025",
    x = "Longitude",
    y = "Latitude",
    caption = paste(
      "Crosses: core reference points;",
      "triangles: additional reference points.",
      "Synthetic data, not observed GPS records."
    )
  ) +
  theme_minimal()
