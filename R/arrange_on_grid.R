#' @title ...
#' 
#' @description ...
#'
#' @param axes Numeric matrix.
#' @param pts Numeric matrix.
#' 
#' @return ...
#'
#' @name core_algorithm
NULL

#' @rdname core_algorithm
#' @export
arrange_points_on_grid <- function(axes, pts) {
  # input checks
  checkmate::assert_matrix(axes, any.missing = FALSE, ncols = 2)
  checkmate::assert_matrix(pts, any.missing = FALSE, ncols = 2)
  # creating grid
  xy_grid <- expand.grid(axes[,1], axes[,2])
  if (nrow(xy_grid) < nrow(pts)) {
    stop("The grid is not big enough to accommodate all input points.")
  }
  # run arrange algorithm
  res <- futhark_entry_arrange_from_coordinates_cpp(
    xy_grid[,1], xy_grid[,2], pts[,1], pts[,2]
  )
  # compile output
  return(matrix(c(x = res[[1]], y = res[[2]]), ncol = 2))
}

#' @rdname core_algorithm
#' @export
make_grid_axis <- function(grid_axis, data_axis, mode = "continous") {
  # input checks
  checkmate::assert_numeric(grid_axis, any.missing = FALSE)
  checkmate::assert_numeric(data_axis)
  # cover options
  if (length(grid_axis) == 1) {
    if (mode == "discrete") {
      unique_x <- length(unique(data_axis))
      seq(-0.5, unique_x+0.5, length.out = grid_axis)
    } else {
      seq(min(data_axis), max(data_axis), length.out = grid_axis)
    }
  } else if (mode == "continous") {
    grid_x
  }
}
