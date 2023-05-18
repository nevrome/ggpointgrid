### 1.3.0

- made the core arrangement algorithm for `geom_pointgrid()` faster, by replacing some inefficient base R code
- made it also more memory efficient for large input datasets and grids, by switching from a recursive to an iterative implementation
- reworked the README to describe more precisely what ggpointgrid can be used for

### 1.2.0

- added `geom_textgrid()`