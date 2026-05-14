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
#' ggplot(testdata, aes(x, y)) +
#'   geom_pointgrid(color = "red", grid_x = 40L, grid_y = 40L)
#' 
#' # with polygon constraint
#' outer = matrix(c(1,1,5,1,5,5,1,5,1,1), ncol=2, byrow=TRUE)
#' hole = matrix(c(2,2,2,4,4,4,4,2,2,2), ncol=2, byrow=TRUE)
#' poly = sf::st_polygon(list(outer, hole))
#' ggplot() +
#'   geom_sf(data = sf::st_sfc(poly)) +
#'   geom_pointgrid(
#'     data = testdata, mapping = aes(x, y),
#'     color = "red", grid_x = 40L, grid_y = 40L, polygons_sf = poly
#'   )
#' 
#' @export
geom_pointgrid <- function(
  mapping = NULL,
  data = NULL,
  grid_x = 20L,
  grid_y = 20L,
  polygons_sf = NULL,
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
      polygons_sf = polygons_sf,
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
  setup_data = function(data, params) {
    # these lines are the main difference to geom_point!
    # the point coordinates are manipulated to map to a grid layout
    axes <- make_grid_axes_in_geom(data, params$grid_x, params$grid_y, params$polygons_sf)
    paog <- arrange_points_on_grid(axes, as.matrix(data[c("x", "y")]))
    data$x <- paog[,1]
    data$y <- paog[,2]
    return(data)
  },
  draw_panel = function(data, panel_params, coord, grid_x, grid_y, polygons_sf) {
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
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
