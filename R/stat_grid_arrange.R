#' stat_grid_arrange
#' 
#' ggplot2 \link[ggplot2]{stat} to perform the grid arrangement.
#' \code{compute_grid_arrangement} allows to perform the arrangement directly,
#' to avoid recomputation when *grid geoms are combined.
#' 
#' @param grid_x Single integer or numeric vector. If a single integer is supplied, 
#' then the grid's x-axis coordinates are determined as a regular sequence from
#' \code{min(x)} to \code{max(x)}, so in relation to the input data points.
#' If a numeric vector is supplied, then this vector is directly used for the
#' grid's x-axis coordinates.
#' Note that integers in R are marked with a trailing L, so e.g. grid_x = 40L.
#' @param grid_y Single integer or numeric vector. Like \code{grid_x}, but for the 
#' y-axis.
#' @param polygons_sf An \code{sf} or \code{sfc} object of type
#' \code{POLYGON} or \code{MULTIPOLYGON}. These geometries define regions
#' inside which grid points are retained. See \link[sf]{st_polygon} for more on
#' how to create the region definitions. Polygon holes are supported automatically.
#' When this is given, then the ranges for \code{grid_x} and \code{grid_y} with
#' single integer input are derived from the bounding box of \code{polygons_sf}.
#' 
#' @examples
#' library(ggplot2)
#' testdata <- data.frame(
#'   x = c(1, 2, 1.95, 2, 3, 3, 3, 3, 3, 4, 4.02, 4, 4.01, 5),
#'   y = c(1, 2, 1.95, 4, 3, 3, 3, 3, 3, 2, 2.02, 4, 3.97, 5)
#' )
#' laid_out <- compute_grid_arrangement(testdata, grid_x = 20L, grid_y = 20L)
#' ggplot(laid_out) +
#'   geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_point(aes(x = xend, y = yend)) +
#'   geom_point(aes(x = x, y = y), colour = "red")
#' 
#' @name stat_grid_arrange
NULL

#' @rdname stat_grid_arrange
#' @export
stat_grid_arrange <- function(
    mapping = NULL,
    data = NULL,
    geom = "point",
    position = "identity",
    ...,
    grid_x = 20L,
    grid_y = 20L,
    polygons_sf = NULL,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  ggplot2::layer(
    stat = StatGridArrange,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      grid_x = grid_x,
      grid_y = grid_y,
      polygons_sf = polygons_sf,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname stat_grid_arrange
#' @export
StatGridArrange <- ggplot2::ggproto(
  "StatGridArrange", ggplot2::Stat,
  required_aes = c("x", "y"),
  
  compute_panel = function(data, scales, grid_x = 20L, grid_y = 20L, polygons_sf = NULL) {
    compute_grid_arrangement(
      data = data,
      grid_x = grid_x,
      grid_y = grid_y,
      polygons_sf = polygons_sf
    )
  }
)

#' @param data Data.frame. Positions of the input data points. Must have columns
#' named "x" and "y" with the coordinates on these axes.
#' 
#' @rdname stat_grid_arrange
#' @export
compute_grid_arrangement <- function(data, grid_x = 20L, grid_y = 20L, polygons_sf = NULL) {
  # input checks
  checkmate::assert_data_frame(data)
  checkmate::assert_names(c("x", "y"), subset.of = names(data))
  # perform grid arrangement
  axes <- make_grid_axes_in_geom(data, grid_x, grid_y, polygons_sf)
  paog <- arrange_points_on_grid(axes, as.matrix(data[c("x", "y")]))
  data$xend <- data$x
  data$yend <- data$y
  data$x <- paog[,1]
  data$y <- paog[,2]
  return(data)
}

# prepare the axes coordinates from the grid input values
make_grid_axes_in_geom <- function(tab, grid_x, grid_y, polygons_sf) {
  # input checks
  checkmate::assert_data_frame(tab)
  # prepare reference range: either input points or polygons bounding box
  ref_range <- if (is.null(polygons_sf)) {
    tab
  } else {
    rlang::check_installed("sf")
    bb <- sf::st_bbox(polygons_sf)
    data.frame(
      x = bb[c(1,3)],
      y = bb[c(2,4)]
    )
  }
  # compile axes
  if (length(grid_x) == 1 & is.integer(grid_x)) {
    axis_x <- make_grid_sequence(
      grid_x, ref_range[["x"]],
      ifelse(
        "mapped_discrete" %in% class(ref_range[["x"]]),
        "discrete",
        "continuous"
      )
    )
  } else {
    axis_x <- grid_x
  }
  if (length(grid_y) == 1 & is.integer(grid_y)) {
    axis_y <- make_grid_sequence(
      grid_y, ref_range[["y"]],
      ifelse(
        "mapped_discrete" %in% class(ref_range[["y"]]),
        "discrete",
        "continuous"
      )
    )
  } else {
    axis_y <- grid_y
  }
  # expand
  axes_df <- expand.grid(axis_x, axis_y)
  # filter by polygons if given
  axes_matrix <- if (is.null(polygons_sf)) {
    as.matrix(axes_df)
  } else {
    filter_grid_in_polygons(as.matrix(axes_df), polygons_sf)
  }
  # return axes matrix
  return(axes_matrix)
}