#### labels on diagonal lines ####
library(magrittr)
library(ggplot2)

# prepare mtcars dataset with id column
dat <- mtcars %>% tibble::rownames_to_column("id")

# fit linear trend line to
# hp = Gross horsepower and mpg = Miles/(US) gallon
fit <- lm(hp~mpg, data = dat)
pred <- tibble::tibble(
  mpg = seq(8, 35, length.out = 2 * nrow(dat)),
  hp = predict(fit, newdata = data.frame(mpg))
)

# shift trend line to work as a target grid for the label arrangement
pred_shifted <- pred
pred_shifted$mpg <- pred_shifted$mpg - 5
pred_shifted$hp <- pred_shifted$hp - 40

# compute data range and orthogonal angle to trend line
# to set the plot ratio and label angle
b <- coef(fit)[["mpg"]]
x_range <- diff(range(c(dat$mpg, pred$mpg, pred_shifted$mpg)))
y_range <- diff(range(c(dat$hp, pred$hp, pred_shifted$hp)))
orth_angle <- atan(b * x_range / y_range) * 180 / pi + 90

# assemble plot
p <- ggplot(data = dat, aes(x = mpg, y = hp)) +
  geom_point(data = pred_shifted, colour = "grey", size = 1) +
  geom_line(data = pred, colour = "red") +
  geom_segmentgrid(
    # the grid defines where labels could be placed
    grid_xy = as.matrix(pred_shifted),
    # hp and mpg are scaled very differently
    # scale_xy unifies that for point-grid distance calculation
    scale_xy = TRUE,
    colour = "grey", linetype = "dashed"
  ) +
  geom_point() +
  geom_textgrid(
    aes(label = id),
    # the same arguments as for the segments to get the same label arrangement
    grid_xy = as.matrix(pred_shifted),
    scale_xy = TRUE,
    hjust = 1.05, angle = orth_angle, size = 3
  ) +
  coord_cartesian(clip = FALSE, ratio = x_range/y_range) +
  theme_bw()

ggsave(
  "blog/2026_05_labelling/labels_on_diagonal_lines.png",
  plot = p,
  units = "px",
  scale = 2,
  width = 1000,
  height = 1000,
  bg = "#FFFFFF"
)

#### labels around polygon ####
library(magrittr)
library(ggplot2)

# download spatial data
# germany border
temp <- tempfile()
download.file("https://daten.gdz.bkg.bund.de/produkte/vg/nuts5000/aktuell/nuts5000_12-31.utm32s.gpkg.zip", temp)
germany <- sf::st_read(grepv(".gpkg$", unzip(temp, exdir = tempdir()))) %>% sf::st_union()
crs <- sf::st_crs(germany)
# airports around the world
temp <- tempfile()
download.file("https://naciscdn.org/naturalearth/10m/cultural/ne_10m_airports.zip", temp)
airports_world <- sf::st_read(grepv(".shp", unzip(temp, exdir = tempdir())))
airports_germany <- airports_world %>%
  sf::st_transform(crs) %>%
  sf::st_intersection(germany)
airports_df <- airports_germany %>%
  dplyr::mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
  ) %>% sf::st_drop_geometry()

# prepare label grid on a buffer line around the germany polygon
germany_buffer_points <- germany %>%
  # line in 60km distance to the polygon
  sf::st_buffer(dist = 60000) %>%
  sf::st_boundary() %>%
  # 20 equidistant grid points along this line
  sf::st_line_sample(n = 20)

# assemble plot
p <- ggplot() +
  geom_sf(data = germany, fill = "white", colour = "black") +
  geom_sf(data = germany_buffer_points, colour = "grey", size = 1) +
  ggpointgrid::geom_segmentgrid(
    data = airports_df, aes(x, y),
    grid_xy = sf::st_coordinates(germany_buffer_points),
  ) +
  geom_point(data = airports_df, aes(x, y)) +
  ggpointgrid::geom_labelgrid(
    data = airports_df, aes(x, y, label = abbrev),
    grid_xy = sf::st_coordinates(germany_buffer_points)) +
  coord_sf(clip = "off") +
  theme_bw() +
  theme(axis.title = element_blank())

ggsave(
  "blog/2026_05_labelling/labels_around_polygon.png",
  plot = p,
  units = "px",
  scale = 2,
  width = 1000,
  height = 1000,
  bg = "#FFFFFF"
)

#### labels on a grid ####
library(magrittr)
library(ggplot2)

# make random, annulus-shaped point cloud
set.seed(124)
n <- 1000
r1 <- 1
r2 <- 2
theta <- runif(n, 0, 2*pi)
r <- sqrt(runif(n, r1^2, r2^2))
annulus <- tibble::tibble(
  x = r * cos(theta), y = r * sin(theta),
  label = 1:n
)

# define polygons around the point cloud using sf
points <- sf::st_multipoint(as.matrix(annulus[,c(1,2)]))
outline <- points %>% sf::st_convex_hull() %>% sf::st_buffer(dist = 1)
annulus_polygon <- points %>% sf::st_buffer(dist = 0.2)
# these are the polygons around the point cloud, excluding the area
# immediately around the points themselves
annulus_polygon_inv <- sf::st_difference(outline, annulus_polygon)

# select 100 random samples for labelling
annulus_sample <- annulus %>% dplyr::slice_sample(n = 100)

# assemble plot
p <- ggplot() +
  #geom_sf(data = annulus_polygon_inv) +
  ggpointgrid::geom_segmentgrid(
    data = annulus_sample, aes(x, y),
    # define grid resolution
    grid_x = 25L, grid_y = 25L,
    # filter grid to only points within the polygons around the point cloud
    polygons_sf = annulus_polygon_inv,
    colour = "grey"
  ) +
  geom_point(data = annulus, aes(x, y)) +
  ggpointgrid::geom_labelgrid(
    data = annulus_sample, aes(x, y, label = label),
    grid_x = 25L, grid_y = 25L,
    polygons_sf = annulus_polygon_inv,
    size = 3
  ) +
  theme_bw()

ggsave(
  "blog/2026_05_labelling/labels_on_grid.png",
  plot = p,
  units = "px",
  scale = 2,
  width = 1000,
  height = 1000,
  bg = "#FFFFFF"
)
