#' Title
#'
#' @param mapping test
#' @param data test
#' @param stat test
#' @param position test
#' @param na.rm test
#' @param show.legend test
#' @param inherit.aes test
#' @param ... test
#'
#' @return test
#'
#' @examples
#' 
#' library(ggplot2)
#' ggplot(mtcars) +
#' geom_pointgrid(aes(hp, mpg))
#' 
#' @export
geom_pointgrid <- function(
  mapping = NULL, 
  data = NULL, 
  stat = "identity",
  position = "identity", 
  na.rm = FALSE, 
  show.legend = NA,
  inherit.aes = TRUE, 
  ...
) {
  ggplot2::layer(
    geom = GeomPointGrid, 
    mapping = mapping,  
    data = data, 
    stat = stat, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, 
      ...
    )
  )
}

#' geom object for use in geom_beeswarm
#' @export
GeomPointGrid <- ggplot2::ggproto(
  "GeomPointGrid", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(shape = 19, colour = "black"),
  draw_key = ggplot2::draw_key_point,
  
  draw_panel = function(data, panel_params, coord) {
    coords <- coord$transform(data, panel_params)
    grid::pointsGrob(
      coords$x, coords$y,
      pch = coords$shape,
      gp = grid::gpar(col = coords$colour)
    )
  }
)
