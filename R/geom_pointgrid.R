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
#' geom_pointgrid(aes(hp, mpg))
#' 
#' @export
geom_pointgrid <- function(
  mapping = NULL, 
  data = NULL, 
  grid_dim = 20,
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
      grid_dim = grid_dim,
      ...
    )
  )
}

#' geom object for use in geom_beeswarm
#' @export
GeomPointGrid <- ggplot2::ggproto(
  "GeomPointGrid", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(size = 1, shape = 19, colour = "red"),
  draw_key = ggplot2::draw_key_point,
  
  draw_panel = function(data, panel_params, coord, grid_dim) {
    
    data <- arrange_data(data, grid_dim)
    
    coords <- coord$transform(data, panel_params)
    grid::pointsGrob(
      coords$x, coords$y,
      pch = coords$shape,
      size = unit(coords$size, "char"),
      gp = grid::gpar(col = coords$colour)
    )
  }
)

arrange_data <- function(tab, grid_dim) {
  
  # grid arrangement for mean points
  x_grid <- seq(min(tab[["x"]]), max(tab[["x"]]), length.out = grid_dim)
  y_grid <- seq(min(tab[["y"]]), max(tab[["y"]]), length.out = grid_dim)
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

