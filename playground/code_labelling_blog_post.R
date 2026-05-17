library(magrittr)
library(ggplot2)

#### labels on orthogonal lines ####

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

