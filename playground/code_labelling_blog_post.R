library(magrittr)
library(ggplot2)

#### labels on diagonal lines ####

dat <- mtcars
dat$car <- rownames(dat)

fit <- lm(hp~mpg, data = dat)
pred <- tibble::tibble(
  mpg = seq(0, 40, length.out = 2.5 * nrow(dat)),
  hp = predict(fit, newdata = data.frame(mpg))
)
pred_shifted <- pred
pred_shifted$hp <- pred_shifted$hp - 70

# orthogonal angle
b <- coef(fit)[["mpg"]]
x_range <- diff(range(c(dat$mpg, pred$mpg)))
y_range <- diff(range(c(dat$hp, pred$hp)))
orth_angle <- atan(b * x_range / y_range) * 180 / pi + 90

ggplot(data = dat, aes(x = mpg, y = hp)) +
  #geom_point(data = pred_shifted, aes(x = mpg, y = hp), colour = "red") +
  geom_line(data = pred) +
  geom_segmentgrid(
    grid_xy = as.matrix(pred_shifted), scale_xy = TRUE,
    colour = "grey", linetype = "dashed"
  ) +
  geom_point() +
  geom_textgrid(
    aes(label = car),
    grid_xy = as.matrix(pred_shifted), scale_xy = TRUE,
    hjust = 1.05, angle = orth_angle
  ) +
  coord_cartesian(clip = FALSE, ratio = x_range/y_range) +
  theme_bw()

#### labels around a polygon ####

library(magrittr)
library(sf)

# get germany polygon
germany <- rnaturalearthdata::countries50 %>%
  dplyr::filter(adm0_a3 == "DEU")
# get airports
temp <- tempfile()
download.file("https://naciscdn.org/naturalearth/10m/cultural/ne_10m_airports.zip", temp)
airports_world <- sf::st_read(grepv(".shp", unzip(temp, exdir = tempdir())))
airports_germany <- airports_world %>%
  sf::st_intersection(germany)

# prepare spatial data
germany_4647 <- germany %>% sf::st_transform(4647)
germany_buffer_points_4647 <- germany_4647 %>%
  sf::st_buffer(dist = 60000) %>%
  sf::st_boundary() %>%
  sf::st_line_sample(n = 20)
airports_df <- airports_germany %>%
  sf::st_transform(4647) %>%
  dplyr::mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
  ) %>% sf::st_drop_geometry()

# label plot
ggplot() +
  geom_sf(data = germany_4647) +
  geom_sf(data = germany_buffer_points_4647) +
  geom_point(data = airports_df, aes(x, y)) +
  ggpointgrid::geom_segmentgrid(
    data = airports_df, aes(x, y),
    grid_xy = sf::st_coordinates(germany_buffer_points_4647)) +
  ggpointgrid::geom_labelgrid(
    data = airports_df, aes(x, y, label = abbrev),
    grid_xy = sf::st_coordinates(germany_buffer_points_4647)) +
  coord_sf(clip = "off")

#### labels on a grid ####

set.seed(1)

n <- 2000
r1 <- 1
r2 <- 2

theta <- runif(n, 0, 2*pi)
r <- sqrt(runif(n, r1^2, r2^2))

x <- r * cos(theta)
y <- r * sin(theta)

dat <- data.frame(x = x, y = y)

plot(dat$x, dat$y, asp = 1, pch = 16, cex = 0.4)
