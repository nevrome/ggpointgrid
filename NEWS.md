### 1.4.0

- switched to a futhark (https://futhark-lang.org) implementation of the arrangement algorithm behind `geom_pointgrid()`
- added a direct interface for this algorithm: `arrange_points_on_grid()`
- added the helper function `make_grid_sequence()` for coordinate sequence preparation

### 1.3.0

- made the core arrangement algorithm for `geom_pointgrid()` faster, by replacing some inefficient base R code
- made it also more memory efficient for large input datasets and grids, by switching from a recursive to an iterative implementation
- reworked the README to describe more precisely what ggpointgrid can be used for

### 1.2.0

- added `geom_textgrid()`