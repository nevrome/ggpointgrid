
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpointgrid

This package provides some simple geoms (`geom_pointgrid`,
`geom_pointrect`) derived from geom\_point to rearrange scatterplot
coordinates on regular grids while strictly avoiding over-plotting. The
applications might be similar to `geom_jitter`.

### Installation

You can install the development version from github with the following
command (in your R console):

    if(!require('remotes')) install.packages('remotes')
    remotes::install_github("nevrome/ggpointgrid")

### Example

``` r
library(ggplot2)
library(magrittr)
library(palmerpenguins)
```

``` r
palmerpenguins::penguins %>%
  ggplot() +
  ggpointgrid::geom_pointgrid(
    aes(x = body_mass_g, y = bill_length_mm, shape = species, color = sex),
    grid_x = 50,
    grid_y = 50
  )
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
palmerpenguins::penguins %>%
  dplyr::arrange(island) %>%
  ggplot() +
  ggpointgrid::geom_pointrect(
    aes(x = sex, y = species, color = island), 
    scale_x = 0.05, 
    scale_y = 0.05
  )
```

![](README_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->
