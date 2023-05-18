#### simple example ####

library(ggplot2)

my_data <- tibble::tibble(
  x = rnorm(500),
  y = rnorm(500),
  value = sample(c("A", "B"), 500, replace = T)
)

p1 <- ggplot(my_data, aes(x = x, y = y, color = value)) +
  geom_point() +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5))

p2 <- ggplot(my_data, aes(x = x, y = y, color = value)) +
  ggpointgrid::geom_pointgrid(grid_x = 50, grid_y = 50) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5))

cowplot::plot_grid(p1, p2)


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

