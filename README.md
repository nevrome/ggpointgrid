
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpointgrid

This package provides some simple geoms derived to rearrange scatterplot
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

#### geom\_pointgrid

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

#### geom\_textgrid

``` r
palmerpenguins::penguins %>%
  ggplot() +
  ggpointgrid::geom_textgrid(
    aes(
      x = body_mass_g, y = bill_length_mm, 
      label = as.character(as.hexmode(1:nrow(palmerpenguins::penguins))), 
      color = sex
    ),
    size = 2,
    grid_x = 30,
    grid_y = 50
  )
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#### geom\_pointrect

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

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
