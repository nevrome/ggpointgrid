#' geom_pointgrid
#' 
#' \code{geom_pointgrid} plots points not to their exact coordinates, but 
#' on a regular grid. This rearrangement avoids any overplotting by
#' attributing each input point its own grid position. The grid properties are 
#' controlled with the parameters \code{grid_x} and \code{grid_y}.
#'
#' @inheritParams ggplot2::geom_point
#' @param grid_x Single integer or numeric vector. If a single integer is supplied, 
#' then the grid's x-axis coordinates are determined as a regular sequence from
#' \code{min(x)} to \code{max(x)}. If a numeric vector is supplied, then this 
#' vector is directly used for the grid's x-axis coordinates.
#' @param grid_y Single integer or numeric vector. Like \code{grid_x}, but for the 
#' y-axis.
#' 
#' @examples
#' library(ggplot2)
#' 
#' testdata <- data.frame(
#'   x = c(1, 2, 1.95, 2, 3, 3, 3, 3, 3, 4, 4.02, 4, 4.01, 5),
#'   y = c(1, 2, 1.95, 4, 3, 3, 3, 3, 3, 2, 2.02, 4, 3.97, 5)
#' )
#' 
#' ggplot(testdata, aes(x, y)) +
#' geom_pointgrid(color = "red", grid_x = 40, grid_y = 40)
#' 
#' @export
geom_pointgrid <- function(
  mapping = NULL,
  data = NULL,
  grid_x = 20,
  grid_y = 20,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  # call layer function
  ggplot2::layer(
    mapping = mapping,
    data = data,
    stat = stat,
    geom = GeomPointGrid,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      grid_x = grid_x,
      grid_y = grid_y,
      ...
    )
  )
}

#' geom object for use in geom_pointgrid
#' @export
GeomPointGrid <- ggplot2::ggproto(
  "GeomPointGrid", ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = ggplot2::aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_params, coord, grid_x, grid_y) {
    
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
    
    # these two lines are the main difference to geom_point!
    # the point coordinates are manipulated to map to a grid layout
    axes <- make_grid_axes_2D(data, grid_x, grid_y)
    data <- arrange_points_on_grid_df(data, axes[["x"]], axes[["y"]])
    
    coords <- coord$transform(data, panel_params)
    ggname(
      "geom_pointgrid",
      grid::pointsGrob(
        coords$x, coords$y,
        pch = coords$shape,
        gp = grid::gpar(
          col = ggplot2::alpha(coords$colour, coords$alpha),
          fill = ggplot2::alpha(coords$fill, coords$alpha),
          fontsize = coords$size * ggplot2::.pt + coords$stroke * ggplot2::.stroke / 2,
          lwd = coords$stroke * ggplot2::.stroke / 2
        )
      )
    )
  }
)

# prepare the axes coordinates from the grid input values
make_grid_axes_2D <- function(tab, grid_x, grid_y) {
  # input checks
  checkmate::assert_data_frame(tab)
  checkmate::assert_numeric(tab[["x"]])
  checkmate::assert_numeric(tab[["y"]])
  checkmate::assert_numeric(grid_x, any.missing = FALSE)
  checkmate::assert_numeric(grid_y, any.missing = FALSE)
  # compile axes
  if (length(grid_x) == 1) {
    if ("mapped_discrete" %in% class(tab[["x"]])) {
      unique_x <- length(unique(tab[["x"]]))
      axis_x <- seq(-0.5, unique_x+0.5, length.out = grid_x)
    } else {
      axis_x <- seq(min(tab[["x"]]), max(tab[["x"]]), length.out = grid_x)
    }
  } else {
    axis_x <- grid_x
  }
  if (length(grid_y) == 1) {
    if ("mapped_discrete" %in% class(tab[["y"]])) {
      unique_y <- length(unique(tab[["y"]]))
      axis_y <- seq(-0.5, unique_y+0.5, length.out = grid_y)
    } else {
      axis_y <- seq(min(tab[["y"]]), max(tab[["y"]]), length.out = grid_y)
    }
  } else {
    axis_y <- grid_y
  }
  return(list(x = axis_x, y = axis_y))
}