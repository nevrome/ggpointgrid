#### legacy R implementation ####
# this is the origional implementation of the algorithm in R,
# before the futhark implementation was added

arrange_points_on_grid_legacy <- function(tab, axis_x, axis_y) {
  xy_grid <- expand.grid(axis_x, axis_y)
  # size check
  if (nrow(xy_grid) < nrow(tab)) {
    stop("The grid is not big enough to accommodate all input points.")
  }
  # calculate distance between input points and grid
  distance_matrix <- fields::rdist(xy_grid, tab[c("x", "y")])
  distance_long_raw <- stats::setNames(
    reshape2::melt(distance_matrix), 
    c('grid_id', 'point_id', 'distance')
  )
  distance_long <- distance_long_raw
  # determine the optimal grid arrangement coordinates
  grid_df <- core_arrange_loop(distance_long)
  # replace input point coordinates with grid coordinates
  tab[grid_df$point_id, c("x", "y")] <- xy_grid[grid_df$grid_id,]
  return(tab)
}

# test code for the core algorithm
# distance_matrix <- fields::rdist(expand.grid(1:5, 1:5), data.frame(x = c(2,2,3,3), y = c(2,2,3,3)))
# distance_long <- stats::setNames(reshape2::melt(distance_matrix), c('grid_id', 'point_id', 'distance'))
# core_arrange_loop(distance_long)
# larger test
# distance_matrix <- fields::rdist(expand.grid(1:100, 1:100), data.frame(x = sample(1:100, 4000, replace = T), y = sample(1:100, 4000, replace = T)))
# distance_long <- stats::setNames(reshape2::melt(distance_matrix), c('grid_id', 'point_id', 'distance'))
# core_arrange_loop(distance_long)

core_arrange_loop <- function(distance_long) {
  distance_long_sorted <- dplyr::arrange(distance_long, .data[["distance"]])
  res_total <- list()
  repeat {
    res <- arrange_iteration(distance_long_sorted)
    res_total <- append(res_total, list(res$solved))
    if (nrow(res$todo) > 0) {
      distance_long_sorted <- res$todo
    } else {
      break
    }
  }
  return(dplyr::bind_rows(res_total))
}

arrange_iteration <- function(distance_long_sorted) {
  # get smallest distance grid point for each input point
  closest_grid_points <- distance_long_sorted[!duplicated(distance_long_sorted$point_id),]
  # find grid point duplicates and create subsets with already uniquely claimed positions
  grid_points_dups <- unique(closest_grid_points$grid_id[duplicated(closest_grid_points$grid_id)])
  has_grid_point_dup <- closest_grid_points$grid_id %in% grid_points_dups
  without_dups <- closest_grid_points[!has_grid_point_dup,]
  with_dups <- closest_grid_points[has_grid_point_dup,]
  # find best input point matches according to distance for duplicated grid points
  with_dups_better <- with_dups[!duplicated(with_dups$grid_id),]
  # combine newly solved point-grid pairs for the output grid
  newly_solved <- dplyr::bind_rows(without_dups, with_dups_better)
  # construct distance table with yet unattributed input and grid points
  new_distance_long <- dplyr::anti_join(
    dplyr::anti_join(distance_long_sorted, newly_solved, by = "grid_id"),
    newly_solved, by = "point_id"
  )
  # output
  return(list(solved = newly_solved, todo = new_distance_long))
}