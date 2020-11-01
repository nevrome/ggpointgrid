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
#' geom_point(aes(hp, mpg)) +
#' geom_pointgrid(aes(hp, mpg), color = "red")
#' 
#' @export
geom_pointgrid <- function(
  mapping = NULL, 
  data = NULL, 
  grid_x = 20,
  grid_y = 20,
  stat = "identity",
  position = "identity", 
  ...,
  na.rm = FALSE, 
  show.legend = NA,
  inherit.aes = TRUE
) {
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
      ...
    )
  )
}

#' geom object for use in geom_beeswarm
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
  draw_panel = function(data, panel_params, coord, grid_x, grid_y) {
    
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
    
    # this line is the main difference to geom_point!
    # the point coordinates are manipulated to map to a grid
    # layout
    data <- arrange_data(data, grid_x, grid_y)
    
    coords <- coord$transform(data, panel_params)
    ggname("geom_pointgrid",
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

arrange_data <- function(tab, grid_x, grid_y) {
  
  # grid arrangement for mean points
  x_grid <- seq(min(tab[["x"]]), max(tab[["x"]]), length.out = grid_x)
  y_grid <- seq(min(tab[["y"]]), max(tab[["y"]]), length.out = grid_y)
  xy_grid <- expand.grid(x_grid, y_grid)
  
  distance_matrix <- fields::rdist(xy_grid, tab[c("x", "y")])
  distance_long <- setNames(reshape2::melt(distance_matrix), c('grid_id', 'mean_point_id', 'distance'))
  
  grid_df <- arrange_on_grid(distance_long)
  
  tab[grid_df$mean_point_id,c("x", "y")] <- xy_grid[grid_df$grid_id,]
  
  return(tab)

}

arrange_on_grid <- function(distance_long, grid_df = data.frame()) {
  # sort table by distance
  distance_long_sorted <- distance_long[order(distance_long$distance),]
  # get smallest distance grid point by input point
  closest_grid_points <- distance_long_sorted[!duplicated(distance_long_sorted$mean_point_id),]
  # find grid point duplicates and create subsets with already uniquely claimed positions
  dups <- unique(closest_grid_points$grid_id[duplicated(closest_grid_points$grid_id)])
  without_dups <- closest_grid_points[!(closest_grid_points$grid_id %in% dups),]
  with_dups <- closest_grid_points[closest_grid_points$grid_id %in% dups,]
  # find best input point matches according to distance for duplicated grid points
  with_dups_better <- with_dups[!duplicated(with_dups$grid_id),]
  # construct current version of the output grid
  grid_df <- unique(rbind(grid_df, without_dups, with_dups_better))
  # construct distance table with yet unattributed input and grid points
  distance_long <- distance_long[
    !(distance_long$grid_id %in% grid_df$grid_id) &
      !(distance_long$mean_point_id %in% grid_df$mean_point_id),
  ]
  # make decision whether the attribution is ready or not
  if (nrow(distance_long) == 0) {
    return(grid_df)
  } else {
    arrange_on_grid(distance_long, grid_df)
  }
}


# copied from https://github.com/tidyverse/ggplot2/blob/v3.3.2/R/geom-point.r
translate_shape_string <- function(shape_string) {
  # strings of length 0 or 1 are interpreted as symbols by grid
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }
  
  pch_table <- c(
    "square open"           = 0,
    "circle open"           = 1,
    "triangle open"         = 2,
    "plus"                  = 3,
    "cross"                 = 4,
    "diamond open"          = 5,
    "triangle down open"    = 6,
    "square cross"          = 7,
    "asterisk"              = 8,
    "diamond plus"          = 9,
    "circle plus"           = 10,
    "star"                  = 11,
    "square plus"           = 12,
    "circle cross"          = 13,
    "square triangle"       = 14,
    "triangle square"       = 14,
    "square"                = 15,
    "circle small"          = 16,
    "triangle"              = 17,
    "diamond"               = 18,
    "circle"                = 19,
    "bullet"                = 20,
    "circle filled"         = 21,
    "square filled"         = 22,
    "diamond filled"        = 23,
    "triangle filled"       = 24,
    "triangle down filled"  = 25
  )
  
  shape_match <- charmatch(shape_string, names(pch_table))
  
  invalid_strings <- is.na(shape_match)
  nonunique_strings <- shape_match == 0
  
  if (any(invalid_strings)) {
    bad_string <- unique(shape_string[invalid_strings])
    n_bad <- length(bad_string)
    
    collapsed_names <- sprintf("\n* '%s'", bad_string[1:min(5, n_bad)])
    
    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }
    
    abort(glue("Can't find shape name:", collapsed_names, more_problems))
  }
  
  if (any(nonunique_strings)) {
    bad_string <- unique(shape_string[nonunique_strings])
    n_bad <- length(bad_string)
    
    n_matches <- vapply(
      bad_string[1:min(5, n_bad)],
      function(shape_string) sum(grepl(paste0("^", shape_string), names(pch_table))),
      integer(1)
    )
    
    collapsed_names <- sprintf(
      "\n* '%s' partially matches %d shape names",
      bad_string[1:min(5, n_bad)], n_matches
    )
    
    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }
    
    abort(glue("Shape names must be unambiguous:", collapsed_names, more_problems))
  }
  
  unname(pch_table[shape_match])
}

# https://github.com/tidyverse/ggplot2/blob/v3.3.2/R/utilities-grid.r
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}
