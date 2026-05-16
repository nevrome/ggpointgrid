### 2.0.0

- implemented a type-based distinction of the two modes of `grid_x` and `grid_y`: Integer grid length mode has to be triggered explicitly now by giving an integer with a trailing L, e.g. `10L` (this is a breaking change!)
- added two new geoms `geom_labelgrid()` and `geom_segmentgrid()`
- defined a custom ggplot2 stat `stat_grid_arrange()` that does the hard work of the grid arrangement for the geoms
- implemented an interface for `stat_grid_arrange()` and therefore the geoms, which allows to set grid parameters per axis, or directly provide the grid, and set polygons to which the grid is filtered
- added a function `filter_grid_in_polygons()` to filter a grid to those points falling inside polygonal regions supplied as sf geometries (sf is a suggested dependency now)
- remodelled the calling of the futhark code to support a second module underpinning `filter_grid_in_polygons()`
- switched to the latest futhark version 0.26.1, which required minor adjustments of the Rcpp bridge code
- fixed a bug that caused the coord limits not to be adjusted after the grid arrangement, which could lead to silently hidden grid-arranged points
- added more documentation and examples to demonstrate the new functionality

### 1.4.0

- switched to a futhark implementation of the arrangement algorithm behind `geom_pointgrid()`
- added a direct interface for this algorithm: `arrange_points_on_grid()`
- added the helper function `make_grid_sequence()` for coordinate sequence preparation

### 1.3.0

- made the core arrangement algorithm for `geom_pointgrid()` faster, by replacing some inefficient base R code
- made it also more memory efficient for large input datasets and grids, by switching from a recursive to an iterative implementation
- reworked the README to describe more precisely what ggpointgrid can be used for

### 1.2.0

- added `geom_textgrid()`