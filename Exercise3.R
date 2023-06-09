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


#task 4:

rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

posmo_filter <- posmo_filter |>
  mutate(segment_ID = rle_id(static))

# Visualize the moving segments by colorizing them based on segment_ID.

ggplot(posmo_filter, aes(x = X, y = Y, colour = segment_ID)) +
  geom_path() +
  geom_point() +
  coord_equal() +
  theme(legend.position = "bottom") +
  labs(title = "Segmented Trajectory with Unique Segment IDs",
       x = "Easting",
       y = "Northing",
       colour = "Segment ID")

# Determine the duration of each segment and remove short segments

posmo_filter <- posmo_filter |>
  group_by(segment_ID) |>
  mutate(duration = max(datetime) - min(datetime)) |>
  ungroup() |>
  filter(duration >= as.difftime(5, units = "mins"))


# Task 5
# load and transform the dataset
pedestrian <- read_csv("data/pedestrian.csv")
pedestrian <- st_as_sf(pedestrian, coords = c("E", "N"), crs = 2056)

# Explore the trajectories and get an idea of how the pedestrians moved. Plot the trajectories using ggplot.

ggplot() +
  geom_sf(data = pedestrian, aes(colour = as.factor(TrajID))) +
  labs(title = "Pedestrian Trajectories",
       x = "Easting",
       y = "Northing",
       colour = "Pedestrian ID") +
  theme(legend.position = "bottom") +
  coord_sf()



ggplot() +
  geom_sf(data = pedestrian, aes(colour = as.factor(TrajID))) +
  labs(title = "Pedestrian Trajectories",
       x = "Easting",
       y = "Northing",
       colour = "Pedestrian ID") +
  theme(legend.position = "bottom") +
  coord_sf() +
  facet_wrap(~ TrajID, ncol = 2)


# Task 6
library(SimilarityMeasures)
pedestrian <- read_csv("data/pedestrian.csv")
trajectories <- split(pedestrian, pedestrian$TrajID)
trajectory_matrices <- lapply(trajectories, function(traj) {
  cbind(traj$E, traj$N)
})

similarity_results <- list()

for (i in 2:length(trajectory_matrices)) {
  similarity_results[[i - 1]] <- list(
  DTW = DTW(trajectory_matrices[[1]], trajectory_matrices[[i]]),
  EditDist = EditDist(trajectory_matrices[[1]], trajectory_matrices[[i]]),
  Frechet = Frechet(trajectory_matrices[[1]], trajectory_matrices[[i]]),
  LCSS = LCSS(trajectory_matrices[[1]], trajectory_matrices[[i]],
  pointSpacing = 1, pointDistance = 1, errorMarg = 0.1)
  )
}

names(similarity_results) <- paste("Traj1_vs_Traj", 2:6, sep = "")

similarity_results_df <- do.call(rbind, lapply(similarity_results, as.data.frame))
similarity_results_df <- cbind(Comparison = rownames(similarity_results_df), similarity_results_df)
rownames(similarity_results_df) <- NULL
print(similarity_results_df)



library(tidyverse)

similarity_results_long <- similarity_results_df %>%
  gather(key = "Similarity_Measure", value = "Value", -Comparison)

ggplot(similarity_results_long, aes(x = Comparison, y = Value, fill = Similarity_Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Similarity Measures for Pedestrian Trajectories",
       x = "Trajectory Comparisons",
       y = "Similarity Measure Value",
       fill = "Similarity Measure") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ Similarity_Measure, scales = "free_y")


# Expectations based on chart: most similar - 1 to 6
#                             least similar - 1 to 4

# Results from analysis: most similar: 1 to 6 based on DTW, EditDist and LCSS, while Frechet 1 to 2 (expected based on the measure)
#                       least similar: 1 to3 for all measures.




