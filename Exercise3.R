library("readr")
library("sf")
library("dplyr")

posmo <- read_delim("data/posmo_e3.csv")
posmo <- select(posmo, datetime, lon_x, lat_y)

#Transform the data to a projected coordinate reference system (e.g., EPSG 2056)
posmo <- st_as_sf(posmo, coords = c("lon_x","lat_y"), crs = 4326) |>  st_transform(2056)

# Extract the coordinates and store them in separate columns.
posmo_coordinates <- st_coordinates(posmo)
posmo <- cbind(posmo, posmo_coordinates)

# check best suitable date with most data points
table(as.Date(posmo$datetime))

#Filter your data to choose a single day for analysis.

posmo_filter <- posmo |>
  filter(as.Date(datetime) == "2023-05-10")

#Implement steps a) to c) from the example:
#  a. Specify a temporal window for measuring Euclidean distances.
#  b. Measure the distance from every point to every other point within the temporal window.
#  c. Remove "static points" based on a given threshold.

library("dplyr")
temporal_window = 4

posmo_filter <- posmo_filter |>
  mutate(
    nMinus2 = sqrt((lag(X, 2) - X)^2 + (lag(Y, 2) - Y)^2),
    nMinus1 = sqrt((lag(X, 1) - X)^2 + (lag(Y, 1) - Y)^2),
    nPlus1  = sqrt((X - lead(X, 1))^2 + (Y - lead(Y, 1))^2),
    nPlus2  = sqrt((X - lead(X, 2))^2 + (Y - lead(Y, 2))^2)
  ) |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2), na.rm = TRUE)
  ) |>
  ungroup()

posmo_filter <- posmo_filter |>
  ungroup() |>
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

posmo_filter <- posmo_filter |>
    filter(!static)


# Task 2: 
# Explore the stepMean values using summary statistics.

summary(posmo_filter$stepMean)
hist(posmo_filter$stepMean)
boxplot(posmo_filter$stepMean)

#  Calculate the mean of all stepMean values and use it as the threshold value d.
d <- mean(posmo_filter$stepMean, na.rm = TRUE)

# Store the new information in a new column named static.

posmo_filter <- posmo_filter |>
  mutate(static = stepMean < d)

# Task 3:

# Create a ggplot

library(ggplot2)

ggplot(posmo_filter, aes(x = X, y = Y, colour = static)) +
    geom_path() +
    geom_point() +
    coord_equal() +
    theme(legend.position = "bottom") +
    labs(title = "Segmented Trajectory",
              x = "Easting",
              y = "Northing",
              colour = "Static")









