library(ggplot2)

#### generate test data ####

n <- 3000
my_data <- tibble::tibble(
  x = runif(n, -5, 5),
  y = runif(n, -5, 5),
  value = sample(c("A", "B"), n, replace = T),
  id = 1:n
)

#### normal plotting test ####

ggplot(my_data, aes(x = x, y = y, color = value)) +
  geom_point() +
  coord_fixed(xlim = c(-5, 5), ylim = c(-5, 5))

ggplot(my_data, aes(x = x, y = y, color = value)) +
  ggpointgrid::geom_pointgrid(grid_x = 80, grid_y = 80) +
  coord_fixed(xlim = c(-5, 5), ylim = c(-5, 5))

#### algorithm test ####

axis_x <- make_grid_sequence(80L, my_data$x)
axis_y <- make_grid_sequence(80L, my_data$y)
axes <- expand.grid(axis_x, axis_y)

system.time(
  res1 <- arrange_points_on_grid(
      as.matrix(axes),
      as.matrix(my_data[c("x", "y")])
    )
)

#### futhark debugging ####

test_data_string <- paste(
  "[", paste(axes[[1]], collapse = ","), "]",
  "[", paste(axes[[2]], collapse = ","), "]",
  "[", paste(my_data$x, collapse = ","), "]",
  "[", paste(my_data$y, collapse = ","), "]"
)
cli_cmd <- paste("echo", test_data_string, "| ./src/arrange")

# run arrange on command line - needs to be compiled as an executable first
result <- system(cli_cmd, intern = T)
system(paste("time", cli_cmd))

# write test data string to file to copy it into the profiling setup list
# in arrange.fut
writeLines(test_data_string, "futhark_test.txt")
