#' @title Grid arrange algorithm
#' 
#' @description \code{arrange_points_on_grid} is an interface to the grid arrange
#' algorithm used for \link{geom_pointgrid}. \code{make_grid_sequence} is a little
#' helper function to prepare regular sequences of coordinates along one dimension.
#'
#' @param grid_xy Numeric matrix. Grid coordinates to which the points should be
#' mapped. 2-column matrix with x-axis coordinates in the first, and y-axis
#' coordinates in the second column.
#' @param pts_xy Numeric matrix. Point (observation) coordinates that should be
#' mapped to the grid. 2-column matrix with x-axis coordinates in the first, and
#' y-axis coordinates in the second column.
#' @param grid_length Integer. Length of the output grid along one axis. Note
#' that integers in R are marked with a trailing L, so e.g. grid_length = 40L.
#' @param data_axis Numeric vector. Coordinates of the input data for plotting
#' along the respective axis. Is used to inform the sequence range.
#' @param mode String option. How the data \code{data_axis} should be interpreted.
#' Either "discrete" or "continuous".
#' 
#' @return \code{arrange_points_on_grid} returns a 2-column numeric matrix with
#' the same number and order of rows as \code{pts_xy}. It contains the grid-mapped
#' x-axis coordinates in the first, and y-axis coordinates in the second column.
#' \code{make_grid_sequence} returns a numeric vector of grid coordinates.
#' 
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   id = 1:200,
#'   x = runif(200, -5, 5),
#'   y = runif(200, -5, 5)
#' )
#' plot(df$x, df$y, pch = 20, cex = 0.7, asp = 1)
#'
#' axis_x <- make_grid_sequence(30L, df$x)
#' axis_y <- make_grid_sequence(30L, df$y)
#' segments(axis_x, min(axis_y), axis_x, max(axis_y), col = "grey")
#' segments(min(axis_x), axis_y, max(axis_x), axis_y, col = "grey")
#'
#' res <- arrange_points_on_grid(
#'  as.matrix(expand.grid(axis_x, axis_y)),
#'  as.matrix(df[c("x", "y")])
#' )
#' segments(df$x, df$y, res[,"x"], res[,"y"])
#' points(res[,"x"], res[,"y"], pch = 18, cex = 1, col = "red")
#'
#' @name grid_arrange_algorithm
NULL

#' @rdname grid_arrange_algorithm
#' @export
arrange_points_on_grid <- function(grid_xy, pts_xy) {
  # input checks
  checkmate::assert_matrix(grid_xy, any.missing = FALSE, ncols = 2)
  checkmate::assert_matrix(pts_xy, any.missing = FALSE, ncols = 2)
  # creating grid
  if (nrow(grid_xy) < nrow(pts_xy)) {
    stop("The grid is not big enough to accommodate all input points.")
  }
  # run arrange algorithm
  res <- futhark_entry_arrange_from_coordinates_cpp(
    grid_xy[,1], grid_xy[,2], pts_xy[,1], pts_xy[,2]
  )
  # compile output
  m <- matrix(c(res[[1]], res[[2]]), ncol = 2)
  colnames(m) <- c("x", "y")
  return(m)
}

#' @rdname grid_arrange_algorithm
#' @export
make_grid_sequence <- function(grid_length, data_axis, mode = "continuous") {
  # input checks
  checkmate::assert_integer(grid_length, lower = 1, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(data_axis)
  # discrete data
  if (mode == "discrete") {
    unique_x <- length(unique(data_axis))
    seq(-0.5, unique_x+0.5, length.out = grid_length)
  # continuous data
  } else if (mode == "continuous") {
    seq(min(data_axis, na.rm = TRUE), max(data_axis, na.rm = TRUE), length.out = grid_length)
  } else {
    stop("Unkown mode: ", mode)
  }
}
