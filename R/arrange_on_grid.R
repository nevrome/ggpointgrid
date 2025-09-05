
# tab <- dplyr::transmute(iris, x = Sepal.Length, y = Sepal.Width)
# grid_x <- 20
# grid_y <- 20
# axes <- ggpointgrid:::make_grid_axes_2D(tab, grid_x, grid_y)
# axis_x <- axes[["x"]]
# axis_y <- axes[["y"]]
# pts_x <- tab[["x"]]
# pts_y <- tab[["y"]]
# arrange_points_on_grid(axis_x, axis_y, pts_x, pts_y)

arrange_points_on_grid_df <- function(tab, axis_x, axis_y) {
  # input checks
  checkmate::assert_data_frame(tab)
  checkmate::assert_numeric(tab[["x"]])
  checkmate::assert_numeric(tab[["y"]])
  # call arrange algorithm
  res <- arrange_points_on_grid(axis_x, axis_y, tab[["x"]], tab[["y"]])
  # overwrite table
  tab[["x"]] <- res[[1]]
  tab[["y"]] <- res[[2]]
  return(tab)
}

#' futhark_test
#' @export
arrange_points_on_grid <- function(axis_x, axis_y, pts_x, pts_y) {
  # input checks
  checkmate::assert_numeric(axis_x, any.missing = FALSE)
  checkmate::assert_numeric(axis_y, any.missing = FALSE)
  checkmate::assert_numeric(pts_x, any.missing = FALSE)
  checkmate::assert_numeric(pts_y, any.missing = FALSE)
  checkmate::assert_true(length(pts_x) == length(pts_y))
  # creating grid
  xy_grid <- expand.grid(axis_x, axis_y)
  if (nrow(xy_grid) < length(pts_x)) {
    stop("The grid is not big enough to accommodate all input points.")
  }
  # run arrange algorithm
  futhark_entry_arrange_from_coordinates_cpp(
    xy_grid[["Var1"]], xy_grid[["Var2"]],
    pts_x, pts_y
  )
}
