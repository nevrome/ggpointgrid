#' geom_textgrid
#' 
#' \code{geom_textgrid} is for \link[ggplot2]{geom_text}
#' what \link{geom_pointgrid} is for \link[ggplot2]{geom_point}.
#'
#' @inheritParams ggplot2::geom_text
#' @inheritParams stat_grid_arrange
#' 
#' @examples
#' library(ggplot2)
#' testdata <- data.frame(
#'   x = c(1, 2, 1.95, 2, 3, 3, 3, 3, 3, 4, 4.02, 4, 4.01, 5),
#'   y = c(1, 2, 1.95, 4, 3, 3, 3, 3, 3, 2, 2.02, 4, 3.97, 5),
#'   l = LETTERS[1:14]
#' )
#' ggplot(testdata, aes(x, y, label = l)) +
#'   geom_textgrid(color = "red", grid_x = 20L, grid_y = 20L)
#' 
#' @export
geom_textgrid <- function(
    mapping = NULL,
    data = NULL,
    grid_x = 20L,
    grid_y = 20L,
    polygons_sf = NULL,
    position = "identity",
    ...,
    parse = FALSE,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  stat_grid_arrange(
    mapping = mapping,
    data = data,
    geom = "text",
    position = position,
    grid_x = grid_x,
    grid_y = grid_y,
    polygons_sf = polygons_sf,
    parse = parse,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    ...
  )
}
