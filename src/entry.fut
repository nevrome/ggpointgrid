import "arrange_hungarian"
import "arrange_greedy"
import "grid"

entry hungarian
  (grid_xs: []f64) (grid_ys: []f64)
  (pts_x: []f64)  (pts_y: []f64)
  : ([]f64, []f64) =
  hungarian grid_xs grid_ys pts_x pts_y

entry greedy
  (grid_xs: []f64) (grid_ys: []f64)
  (pts_x: []f64)  (pts_y: []f64)
  : ([]f64, []f64) =
  greedy grid_xs grid_ys pts_x pts_y

entry grid_in_polygons
  (xs: []f64)
  (ys: []f64)
  (ring_offsets: []i64)
  (polygon_ring_counts: []i64)
  (gx: []f64)
  (gy: []f64)
  : ([]f64, []f64) =
  grid_in_polygons_ xs ys ring_offsets polygon_ring_counts gx gy