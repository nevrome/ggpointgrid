#' geom_textgrid
#' 
#' \code{geom_textgrid} is for \link[ggplot2]{geom_text}
#' what \link{geom_pointgrid} is for \link[ggplot2]{geom_point}.
#'
#' @inheritParams ggplot2::geom_text
#' @inheritParams geom_pointgrid
#' 
#' @examples
#' library(ggplot2)
#' 
#' testdata <- data.frame(
#'   x = c(1, 2, 1.95, 2, 3, 3, 3, 3, 3, 4, 4.02, 4, 4.01, 5),
#'   y = c(1, 2, 1.95, 4, 3, 3, 3, 3, 3, 2, 2.02, 4, 3.97, 5),
#'   l = LETTERS[1:14]
#' )
#' 
#' ggplot(testdata, aes(x, y, label = l)) +
#' geom_textgrid(color = "red", grid_x = 40, grid_y = 40)
#' 
#' @export
geom_textgrid <- function(
  mapping = NULL,
  data = NULL,
  grid_x = 20,
  grid_y = 20,
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  # call layer function
  ggplot2::layer(
    mapping = mapping,
    data = data,
    stat = stat,
    geom = GeomTextGrid,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      grid_x = grid_x,
      grid_y = grid_y,
      ...
    )
  )
}

#' geom object for use in geom_textgrid
#' @export
GeomTextGrid <- ggplot2::ggproto(
  "GeomTextGrid", ggplot2::Geom,
  required_aes = c("x", "y", "label"),

  default_aes = ggplot2::aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_params, coord, 
                        parse = FALSE, na.rm = FALSE,
                        grid_x, grid_y) {
    
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }
    
    # these two lines are the main difference to geom_point!
    # the point coordinates are manipulated to map to a grid layout
    axes <- make_grid_axes_in_geom(data, grid_x, grid_y)
    paog <- arrange_points_on_grid(axes, as.matrix(data[c("x", "y")]))
    data[["x"]] <- paog[,1]
    data[["y"]] <- paog[,2]
    
    coords <- coord$transform(data, panel_params)

    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }
        
    ggname(
      "geom_textgrid",
       grid::textGrob(
         lab,
         coords$x, coords$y, default.units = "native",
         hjust = coords$hjust, vjust = coords$vjust,
         rot = coords$angle,
         gp = grid::gpar(
           col = ggplot2::alpha(coords$colour, coords$alpha),
           fontsize = coords$size * ggplot2::.pt,
           fontfamily = coords$family,
           fontface = coords$fontface,
           lineheight = coords$lineheight
         )
       )
    )
  }
)
