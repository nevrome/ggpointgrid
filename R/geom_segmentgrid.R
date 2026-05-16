#' geom_segmentgrid
#' 
#' \code{geom_segmentgrid} plots connecting lines between points and their
#' grid-arranged position.
#'
#' @inheritParams ggplot2::geom_segment
#' @inheritParams grid_arrange_params
#' 
#' @examples
#' library(ggplot2)
#' testdata <- data.frame(
#'   x = c(1, 2, 1.95, 2, 3, 3, 3, 3, 3, 4, 4.02, 4, 4.01, 5),
#'   y = c(1, 2, 1.95, 4, 3, 3, 3, 3, 3, 2, 2.02, 4, 3.97, 5)
#' )
#' ggplot(testdata, aes(x, y)) +
#'   geom_segmentgrid(grid_x = 20L, grid_y = 20L) +
#'   geom_point() +
#'   geom_pointgrid(color = "red", grid_x = 20L, grid_y = 20L)
#' 
#' @family ggpointgrid geoms
#' @export
geom_segmentgrid <- function(
    mapping = NULL,
    data = NULL,
    grid_x = 20L,
    grid_y = 20L,
    grid_xy = NULL,
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
    geom = "segment",
    position = position,
    grid_x = grid_x,
    grid_y = grid_y,
    grid_xy = grid_xy,
    polygons_sf = polygons_sf,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    ...
  )
}
