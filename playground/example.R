#### simple example ####

library(ggplot2)

n <- 3000
my_data <- tibble::tibble(
  x = runif(n, -5, 5),
  y = runif(n, -5, 5),
  value = sample(c("A", "B"), n, replace = T),
  id = 1:n
)

ggplot(my_data, aes(x = x, y = y, color = value)) +
  geom_point() +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5))

ggplot(my_data, aes(x = x, y = y, color = value)) +
  ggpointgrid::geom_pointgrid(grid_x = 80, grid_y = 80) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5))

axes <- ggpointgrid:::make_grid_axes_2D(my_data, grid_x = 80, grid_y = 80)
system.time(res1 <- arrange_points_on_grid_df(my_data, axis_x = axes[[1]], axis_y = axes[[2]]))
system.time(res2 <- arrange_points_on_grid_legacy(my_data, axis_x = axes[[1]], axis_y = axes[[2]]))
apply(as.matrix(res1 != res2), 2, any)

tu <- paste(
  "[", paste(axes[[1]], collapse = ","), "]",
  "[", paste(axes[[2]], collapse = ","), "]",
  "[", paste(my_data$x, collapse = ","), "]",
  "[", paste(my_data$y, collapse = ","), "]"
)
zu <- paste("echo", tu, "| ./src/arrange")
system(paste("time", zu))
hu <- system(zu, intern = T)

writeLines(tu, "futhark_test.txt")

#### algorithm test ####

# generate test data
regular_grid <- expand.grid(1:100, 1:100)
points_to_plot <- data.frame(
  x = sample(1:100, 4000, replace = T),
  y = sample(1:100, 4000, replace = T)
)

# calculate pairwise distances of grid and points
distance_matrix <- fields::rdist(regular_grid, points_to_plot)
distance_long <- stats::setNames(
  reshape2::melt(distance_matrix),
  c("grid_id", "point_id", "distance")
)

# run algorithm
core_algorithm(distance_long)

