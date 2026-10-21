
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(sf)

set.seed(2024)

# --------------------------------------------------
# 1. Define the full movement boundary
# Coordinates are longitude, latitude for sf
# --------------------------------------------------

boundary_points <- matrix(
  c(
    80.870414, 8.200766,
    80.82871833885383, 8.13134879546795,
    80.8984491803632, 8.108166476428762,
    80.99074808675074, 8.360908595021215,
    80.870414, 8.200766
  ),
  ncol = 2,
  byrow = TRUE
)

boundary <- st_sfc(
  st_polygon(list(boundary_points)),
  crs = 4326
)

# Project to metres for movement calculations
# UTM zone 44N is appropriate for this region.
boundary_m <- st_transform(boundary, 32644)

# --------------------------------------------------
# 2. Define preferred core locations
# --------------------------------------------------

core_locations <- data.frame(
  latitude = c(
    8.14156475703087,
    8.164207281365373,
    8.164207281365373,
    8.19855900377645,
    8.147811098654111,
    8.10876986686702
  ),
  longitude = c(
    80.90556326000684,
    80.90240826642375,
    80.90240826642375,
    80.92685946669283,
    80.90240826642375,
    80.90556326000684
  )
)

core_sf <- st_as_sf(
  core_locations,
  coords = c("longitude", "latitude"),
  crs = 4326
) |>
  st_transform(32644)

core_xy <- st_coordinates(core_sf)

# --------------------------------------------------
# 3. Helper functions
# --------------------------------------------------

inside_boundary <- function(xy) {
  p <- st_sfc(
    st_point(as.numeric(xy)),
    crs = 32644
  )
  
  lengths(st_intersects(p, boundary_m)) > 0
}

sample_inside <- function() {
  # Generate a random point inside the boundary.
  bb <- st_bbox(boundary_m)
  
  repeat {
    candidate <- c(
      runif(1, bb["xmin"], bb["xmax"]),
      runif(1, bb["ymin"], bb["ymax"])
    )
    
    if (inside_boundary(candidate)) {
      return(candidate)
    }
  }
}

# --------------------------------------------------
# 4. Simulate hourly tracking data
# --------------------------------------------------

simulate_elephant <- function(
    start = "2024-01-01 00:00:00",
    end = "2025-12-31 23:00:00",
    animal_id = "ELE001",
    sex = "Male",
    resting_probability = 0.12,
    min_rest_hours = 2,
    max_rest_hours = 8,
    core_probability = 0.85,
    seed = 2024
) {
  
  set.seed(seed)
  
  timestamps <- seq(
    as.POSIXct(start, tz = "Asia/Colombo"),
    as.POSIXct(end, tz = "Asia/Colombo"),
    by = "hour"
  )
  
  n <- length(timestamps)
  
  # Select a starting position close to a core location.
  current <- core_xy[
    sample(seq_len(nrow(core_xy)), 1),
  ]
  
  positions <- matrix(NA_real_, nrow = n, ncol = 2)
  positions[1, ] <- current
  
  # Movement state
  velocity <- c(0, 0)
  target <- current
  
  # Resting state
  resting_remaining <- 0
  
  for (i in 2:n) {
    
    # Continue resting at exactly the same position.
    if (resting_remaining > 0) {
      positions[i, ] <- current
      resting_remaining <- resting_remaining - 1
      next
    }
    
    # Begin a resting episode. The elephant stays in place
    # for at least two consecutive hourly intervals.
    if (runif(1) < resting_probability) {
      
      resting_remaining <- sample(
        min_rest_hours:max_rest_hours,
        1
      )
      
      positions[i, ] <- current
      resting_remaining <- resting_remaining - 1
      velocity <- c(0, 0)
      next
    }
    
    # Select a new target occasionally.
    if (runif(1) < 0.08) {
      
      if (runif(1) < core_probability) {
        
        # Most targets are in the core area.
        target <- core_xy[
          sample(seq_len(nrow(core_xy)), 1),
        ]
        
      } else {
        
        # Occasional excursion to another location,
        # still constrained to the full boundary.
        target <- sample_inside()
      }
    }
    
    # Correlated movement with attraction to target.
    noise <- rnorm(2, mean = 0, sd = 180)
    
    proposed_velocity <-
      0.65 * velocity +
      0.20 * (target - current) +
      noise
    
    # Limit displacement per hour to 1 km.
    speed <- sqrt(sum(proposed_velocity^2))
    
    if (speed > 1000) {
      proposed_velocity <-
        proposed_velocity / speed * 1000
    }
    
    proposed_position <- current + proposed_velocity
    
    # Do not allow movement outside the boundary.
    if (!inside_boundary(proposed_position)) {
      
      # Try a shorter step toward the proposed position.
      fractions <- c(0.5, 0.25, 0.1, 0)
      
      accepted <- FALSE
      
      for (f in fractions) {
        candidate <- current +
          f * (proposed_position - current)
        
        if (inside_boundary(candidate)) {
          proposed_position <- candidate
          proposed_velocity <-
            proposed_position - current
          accepted <- TRUE
          break
        }
      }
      
      if (!accepted) {
        proposed_position <- current
        proposed_velocity <- c(0, 0)
      }
    }
    
    current <- proposed_position
    velocity <- proposed_velocity
    positions[i, ] <- current
  }
  
  # Convert projected positions to longitude and latitude.
  movement_sf <- st_as_sf(
    data.frame(
      x = positions[, 1],
      y = positions[, 2]
    ),
    coords = c("x", "y"),
    crs = 32644
  ) |>
    st_transform(4326)
  
  lonlat <- st_coordinates(movement_sf)
  
  tibble(
    animal_id = animal_id,
    timestamp = timestamps,
    latitude = lonlat[, 2],
    longitude = lonlat[, 1],
    sex = sex,
    year = year(timestamps)
  ) |>
    mutate(
      resting = duplicated(
        paste(latitude, longitude)
      )
    )
}

# --------------------------------------------------
# 5. Generate 2024–2025 data
# --------------------------------------------------

elephant_tracking <- simulate_elephant(
  seed = 2024
)



# Check number of hourly records
nrow(elephant_tracking)

# Expected: 17,544
count(elephant_tracking, year)

# Preview
print(elephant_tracking, n = 20)


library(ggplot2)
library(sf)
library(dplyr)

# Convert boundary to sf in longitude/latitude
boundary_plot <- st_transform(boundary_m, 4326)

ggplot() +
  # Full movement boundary
  geom_sf(
    data = boundary_plot,
    fill = "grey95",
    colour = "black",
    linewidth = 0.8
  ) +
  # Elephant movement path
  geom_path(
    data = elephant_tracking,
    aes(
      x = longitude,
      y = latitude,
      colour = factor(year)
    ),
    linewidth = 0.35,
    alpha = 0.7
  ) +
  # Hourly GPS observations
  geom_point(
    data = elephant_tracking,
    aes(
      x = longitude,
      y = latitude,
      colour = factor(year)
    ),
    size = 0.25,
    alpha = 0.5
  ) +
  # Preferred core locations
  geom_point(
    data = core_locations,
    aes(
      x = longitude,
      y = latitude
    ),
    shape = 4,
    colour = "red",
    size = 4,
    stroke = 1.2
  ) +
  scale_colour_viridis_d(name = "Year") +
  coord_sf(expand = FALSE) +
  labs(
    title = "Simulated Male Elephant Movement",
    subtitle = "Sri Lanka | 2024–2025",
    x = "Longitude",
    y = "Latitude",
    caption = paste(
      "Red crosses indicate preferred core locations.",
      "Trajectory represents synthetic data."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

