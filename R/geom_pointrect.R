#' geom_pointrect
#' 
#' \code{geom_pointrect} plots points not to their exact coordinates, but
#' rearranges them in rectangular boxes around the actual position. 
#' This rearrangement avoids overplotting.
#'
#' @inheritParams ggplot2::geom_point
#' @param scale_x Double. Scaling factor for the point grid box x-axis.
#' @param scale_y Double. Like \code{scale_x}, but for the y-axis.
#' @param round_x Integer. Decimal place where x-axis coordinates should be 
#' rounded to attribute them to the same position. This is only relevant
#' for continuously scaled variables.
#' @param round_y Integer. Like \code{round_x}, but for the y-axis.
#' 
#' @examples
#' library(ggplot2)
#' 
#' testdata <- data.frame(
#'   x = c(1, 2, 1.95, 2, 3, 3, 3, 3, 3, 4, 4.02, 4, 4.01, 5),
#'   y = c(1, 2, 1.95, 4, 3, 3, 3, 3, 3, 2, 2.02, 4, 3.97, 5)
#' )
#' 
#' ggplot(testdata, aes(x, y)) +
#' geom_pointrect(color = "red")
#' 
#' @export
geom_pointrect <- function(
  mapping = NULL,
  data = NULL,
  scale_x = 0.1,
  scale_y = 0.1,
  round_x = 1,
  round_y = 1,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  # input check
  checkmate::assert_numeric(scale_x, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(scale_y, any.missing = FALSE, len = 1)
  checkmate::assert_int(round_x, na.ok = FALSE)
  checkmate::assert_int(round_y, na.ok = FALSE)
  # call layer function
  ggplot2::layer(
    mapping = mapping,
    data = data,
    stat = stat,
    geom = GeomPointRect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      scale_x = scale_x,
      scale_y = scale_y,
      round_x = round_x,
      round_y = round_y,
      ...
    )
  )
}

#' geom object for use in geom_beeswarm
#' @export
GeomPointRect <- ggplot2::ggproto(
  "GeomPointRect", ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = ggplot2::aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_params, coord, scale_x, scale_y, round_x, round_y) {
    
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
    #huhu1 <<- data
    # this line is the main difference to geom_point!
    # the point coordinates are manipulated to map to a grid
    # layout
    data <- arrange_points_in_boxes(data, scale_x, scale_y, round_x, round_y)
    #huhu2 <<- data
    coords <- coord$transform(data, panel_params)
    #huhu3 <<- coords
    ggname("geom_pointrect",
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

arrange_points_in_boxes <- function(tab, scale_x, scale_y, round_x, round_y) {
  # sort 
  sorted_tab <- tab[order(tab$group, tab$colour, tab$fill, tab$shape, tab$size, tab$alpha, tab$stroke), ]
  # round
  sorted_tab[["x"]] <- round(sorted_tab[["x"]], round_x)
  sorted_tab[["y"]] <- round(sorted_tab[["y"]], round_y)
  # arrange in rect
  points_per_position <- split(sorted_tab, interaction(sorted_tab[["x"]], sorted_tab[["y"]]))
  do.call(rbind, lapply(points_per_position, function(x) {
    number_of_points <- nrow(x)
    offsets <- make_box_offsets(number_of_points)
    x[["x"]] <- x[["x"]] + offsets[["x"]] * scale_x
    x[["y"]] <- x[["y"]] + offsets[["y"]] * scale_y
    return(x)
  }))
}

make_box_offsets <- function(x) {
  fact <- sqrt(x)
  ncols <- ceiling(fact)
  if (ncols * floor(fact) >= x) {
    nrows <- floor(fact)
  } else {
    nrows <- ceiling(fact)
  }
  new_grid <- expand.grid(1:ncols, rev(1:nrows))
  grid_center <- c((ncols + 1)/2, (nrows + 1)/2)
  data.frame(
    x = new_grid[[1]] - grid_center[1],
    y = new_grid[[2]] - grid_center[2]
  )[1:x,]
}
