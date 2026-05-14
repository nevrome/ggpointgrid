#' @title Grid arrange algorithm
#' 
#' @description \code{arrange_points_on_grid} is an interface to the grid arrange
#' algorithm used for \link{geom_pointgrid}. \code{make_grid_sequence} is a little
#' helper function to prepare regular sequences of coordinates along one dimension.
#' \code{filter_grid_in_polygons} filters a grid of candidate points to those
#' falling inside polygonal regions supplied as \code{sf} geometries.
#'
#' @param grid_xy Numeric matrix. Grid coordinates to which the points should be
#' mapped. 2-column matrix with x-axis coordinates in the first, and y-axis
#' coordinates in the second column.
#' @param pts_xy Numeric matrix. Point (observation) coordinates that should be
#' mapped to the grid. 2-column matrix with x-axis coordinates in the first, and
#' y-axis coordinates in the second column.
#' @param grid_length Integer. Length of the output grid along one axis. Note
#' that integers in R are marked with a trailing L, so e.g. grid_length = 40L.
#' @param data_axis Numeric vector. Coordinates of the input data for plotting
#' along the respective axis. Is used to inform the sequence range.
#' @param mode String option. How the data \code{data_axis} should be interpreted.
#' Either "discrete" or "continuous".
#' 
#' @return \code{arrange_points_on_grid} returns a 2-column numeric matrix with
#' the same number and order of rows as \code{pts_xy}. It contains the grid-mapped
#' x-axis coordinates in the first, and y-axis coordinates in the second column.
#' \code{make_grid_sequence} returns a numeric vector of grid coordinates.
#' \code{filter_grid_in_polygons} returns a 2-column numeric matrix containing
#' the subset of points from \code{grid_xy} that fall inside the supplied
#' polygon geometry.
#' 
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   id = 1:200,
#'   x = runif(200, -5, 5),
#'   y = runif(200, -5, 5)
#' )
#' plot(df$x, df$y, pch = 20, cex = 0.7, asp = 1)
#'
#' axis_x <- make_grid_sequence(30L, df$x)
#' axis_y <- make_grid_sequence(30L, df$y)
#' segments(axis_x, min(axis_y), axis_x, max(axis_y), col = "grey")
#' segments(min(axis_x), axis_y, max(axis_x), axis_y, col = "grey")
#'
#' res <- arrange_points_on_grid(
#'  as.matrix(expand.grid(axis_x, axis_y)),
#'  as.matrix(df[c("x", "y")])
#' )
#' segments(df$x, df$y, res[,"x"], res[,"y"])
#' points(res[,"x"], res[,"y"], pch = 18, cex = 1, col = "red")
#' 
#' # limit the grid to a certain polygon
#' theta <- seq(0, 2*pi, length.out = 99)
#' circle_xy <- cbind(x = 3 * cos(theta), y = 3 * sin(theta))
#' circle_xy <- rbind(circle_xy, circle_xy[1, , drop = FALSE]) # close polygon
#' circle_poly <- sf::st_sfc(sf::st_polygon(list(circle_xy)))
#' 
#' small_grid <- filter_grid_in_polygons(
#'   as.matrix(expand.grid(axis_x, axis_y)),
#'   circle_poly
#' )
#' res <- arrange_points_on_grid(
#'  small_grid,
#'  as.matrix(df[c("x", "y")])
#' )
#' plot(df$x, df$y, pch = 20, cex = 0.7, asp = 1)
#' polygon(circle_xy[,1], circle_xy[,2], border = "blue", lwd = 2)
#' segments(df$x, df$y, res[,"x"], res[,"y"])
#' points(res[,"x"], res[,"y"], pch = 18, cex = 1, col = "red")
#' 
#' @name grid_arrange_algorithm
NULL

#' @rdname grid_arrange_algorithm
#' @export
arrange_points_on_grid <- function(grid_xy, pts_xy) {
  # input checks
  checkmate::assert_matrix(grid_xy, any.missing = FALSE, ncols = 2)
  checkmate::assert_matrix(pts_xy, any.missing = FALSE, ncols = 2)
  # creating grid
  if (nrow(grid_xy) < nrow(pts_xy)) {
    stop("The grid is not big enough to accommodate all input points.")
  }
  # run arrange algorithm
  res <- futhark_entry_arrange_from_coordinates_cpp(
    grid_xy[,1], grid_xy[,2], pts_xy[,1], pts_xy[,2]
  )
  # compile output
  m <- matrix(c(res[[1]], res[[2]]), ncol = 2)
  colnames(m) <- c("x", "y")
  return(m)
}

#' @rdname grid_arrange_algorithm
#' @export
make_grid_sequence <- function(grid_length, data_axis, mode = "continuous") {
  # input checks
  checkmate::assert_integer(grid_length, lower = 1, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(data_axis)
  # discrete data
  if (mode == "discrete") {
    unique_x <- length(unique(data_axis))
    seq(-0.5, unique_x+0.5, length.out = grid_length)
  # continuous data
  } else if (mode == "continuous") {
    seq(min(data_axis, na.rm = TRUE), max(data_axis, na.rm = TRUE), length.out = grid_length)
  } else {
    stop("Unkown mode: ", mode)
  }
}

#' @param polygons_sf An \code{sf} or \code{sfc} object of type
#' \code{POLYGON} or \code{MULTIPOLYGON}. These geometries define regions
#' inside which grid points are retained. See \link[sf]{st_polygon} for more on
#' how to create the region definitions. Polygon holes are supported automatically.
#'   
#' @rdname grid_arrange_algorithm
#' @export
filter_grid_in_polygons <- function(
  grid_xy,
  polygons_sf
  ) {
  # input checks
  rlang::check_installed("sf")
  checkmate::assert_matrix(grid_xy, any.missing = FALSE, ncols = 2)
  # polygon format transformation
  poly_fmt <- as_futhark_polygon_format(polygons_sf)
  polygons_xy <- poly_fmt$polygons_xy
  ring_offsets <- poly_fmt$ring_offsets
  polygon_ring_counts <- poly_fmt$polygon_ring_counts
  # run grid creation algorithm
  res <- futhark_entry_grid_in_polygons_cpp(
    xs = polygons_xy[,1],
    ys = polygons_xy[,2],
    ring_offsets = ring_offsets,
    polygon_ring_counts = polygon_ring_counts,
    gx = grid_xy[,1],
    gy = grid_xy[,2]
  )
  # compile output
  m <- matrix(c(res[[1]], res[[2]]), ncol = 2)
  colnames(m) <- c("x", "y")
  return(m)
}

as_futhark_polygon_format <- function(polygons) {
  if (inherits(polygons, "sf")) {
    return(as_futhark_polygon_format_sf(sf::st_geometry(polygons)))
  }
  if (inherits(polygons, "sfc")) {
    return(as_futhark_polygon_format_sf(polygons))
  }
  if (inherits(polygons, "sfg")) {
    return(as_futhark_polygon_format_sf(sf::st_sfc(polygons)))
  }
  stop("Unsupported sf polygon format.")
}

as_futhark_polygon_format_sf <- function(geom) {
  # input checks
  geom_types <- unique(as.character(sf::st_geometry_type(geom, by_geometry = TRUE)))
  checkmate::assert_true(
    all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))
  )
  checkmate::assert_true(all(sf::st_is_valid(geom)))
  # format transformation
  all_polygons <- list()
  for (i in seq_along(geom)) {
    g <- geom[[i]]
    g_type <- as.character(sf::st_geometry_type(geom[i], by_geometry = TRUE))
    if (g_type == "POLYGON") {
      # g is list of rings
      poly_rings <- purrr::map(g, \(ring) {as.matrix(ring)[, 1:2, drop = FALSE]})
      all_polygons[[length(all_polygons) + 1]] <- poly_rings
    } else if (g_type == "MULTIPOLYGON") {
      # g is list of polygons; each polygon is list of rings
      for (j in seq_along(g)) {
        poly_rings <- purrr::map(g, \(ring) {as.matrix(ring)[, 1:2, drop = FALSE]})
        all_polygons[[length(all_polygons) + 1]] <- poly_rings
      }
    }
  }
  polygon_ring_counts <- purrr::map_int(all_polygons, length)
  all_rings <- unlist(all_polygons, recursive = FALSE)
  ring_sizes <- purrr::map_int(all_rings, nrow)
  ring_offsets <- c(0, cumsum(ring_sizes))
  polygons_xy <- do.call(rbind, all_rings)
  # assemble result
  return(list(
    polygons_xy = polygons_xy,
    ring_offsets = as.numeric(ring_offsets),
    polygon_ring_counts = as.numeric(polygon_ring_counts)
  ))
}
