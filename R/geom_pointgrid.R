#' geom_pointgrid
#' 
#' \code{geom_pointgrid} plots points not to their exact coordinates, but 
#' on a regular grid. This rearrangement avoids any overplotting by
#' attributing each input point its own grid position. The grid properties are 
#' controlled with the parameters \code{grid_x} and \code{grid_y}.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams grid_arrange_params
#' 
#' @examples
#' library(ggplot2)
#' testdata <- data.frame(
#'   x = c(1, 1, 2, 1.95, 2, 3, 3, 3, 3, 3, 4, 4.02, 4, 4.01, 5, 5),
#'   y = c(5, 1, 2, 1.95, 4, 3, 3, 3, 3, 3, 2, 2.02, 4, 3.97, 5, 1)
#' )
#' ggplot(testdata, aes(x, y)) +
#'   geom_pointgrid(color = "red", grid_x = 20L, grid_y = 20L)
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
#' @family ggpointgrid geoms
#' @export
geom_pointgrid <- function(
    mapping = NULL,
    data = NULL,
    grid_x = 20L,
    grid_y = 20L,
    polygons_sf = NULL,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  stat_grid_arrange(
    mapping = mapping,
    data = data,
    geom = "point",
    position = position,
    grid_x = grid_x,
    grid_y = grid_y,
    polygons_sf = polygons_sf,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    ...
  )
}
