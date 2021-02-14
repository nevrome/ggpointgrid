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
  # input check
  checkmate::assert_numeric(grid_x, any.missing = FALSE)
  checkmate::assert_numeric(grid_y, any.missing = FALSE)
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

#' geom object for use in geom_beeswarm
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
    #huhu1 <<- data
    # this line is the main difference to geom_point!
    # the point coordinates are manipulated to map to a grid
    # layout
    data <- arrange_points_on_grid(data, grid_x, grid_y)
    #huhu2 <<- data
    coords <- coord$transform(data, panel_params)
    #huhu3 <<- coords
    ggname("geom_pointgrid",
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

arrange_points_on_grid <- function(tab, grid_x, grid_y) {
  # define grid the input points should be mapped to
  if (length(grid_x) == 1) {
    if ("mapped_discrete" %in% class(tab[["x"]])) {
      unique_x <- length(unique(tab[["x"]]))
      x_grid <- seq(-0.5, unique_x+0.5, length.out = grid_x)
    } else {
      x_grid <- seq(min(tab[["x"]]), max(tab[["x"]]), length.out = grid_x)
    }
  } else {
    x_grid <- grid_x
  }
  if (length(grid_y) == 1) {
    if ("mapped_discrete" %in% class(tab[["y"]])) {
      unique_y <- length(unique(tab[["y"]]))
      y_grid <- seq(-0.5, unique_y+0.5, length.out = grid_y)
    } else {
      y_grid <- seq(min(tab[["y"]]), max(tab[["y"]]), length.out = grid_y)
    }
  } else {
    y_grid <- grid_y
  }
  xy_grid <- expand.grid(x_grid, y_grid)
  # size check
  if (nrow(xy_grid) < nrow(tab)) {
    stop("The grid is not big enough to accommodate all input points.")
  }
  # calculate distance between input points and grid
  distance_matrix <- fields::rdist(xy_grid, tab[c("x", "y")])
  distance_long <- stats::setNames(
    reshape2::melt(distance_matrix), 
    c('grid_id', 'mean_point_id', 'distance')
  )
  # determine the optimal grid arrangement coordinates
  grid_df <- core_arrange_algorithm(distance_long)
  # replace input point coordinates with grid coordinates
  tab[grid_df$mean_point_id, c("x", "y")] <- xy_grid[grid_df$grid_id,]
  return(tab)
}

# tail-recursive implementation of the rearrangement algorithm
core_arrange_algorithm <- function(distance_long, grid_df = data.frame()) {
  # sort table by distance
  distance_long_sorted <- distance_long[order(distance_long$distance),]
  # get smallest distance grid point by input point
  closest_grid_points <- distance_long_sorted[!duplicated(distance_long_sorted$mean_point_id),]
  # find grid point duplicates and create subsets with already uniquely claimed positions
  dups <- unique(closest_grid_points$grid_id[duplicated(closest_grid_points$grid_id)])
  without_dups <- closest_grid_points[!(closest_grid_points$grid_id %in% dups),]
  with_dups <- closest_grid_points[closest_grid_points$grid_id %in% dups,]
  # find best input point matches according to distance for duplicated grid points
  with_dups_better <- with_dups[!duplicated(with_dups$grid_id),]
  # construct current version of the output grid
  grid_df <- unique(rbind(grid_df, without_dups, with_dups_better))
  # construct distance table with yet unattributed input and grid points
  distance_long <- distance_long[
    !(distance_long$grid_id %in% grid_df$grid_id) &
      !(distance_long$mean_point_id %in% grid_df$mean_point_id),
  ]
  # make decision whether the attribution is ready or not
  if (nrow(distance_long) == 0) {
    return(grid_df)
  } else {
    core_arrange_algorithm(distance_long, grid_df)
  }
}
