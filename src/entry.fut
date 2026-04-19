import "arrange"
import "grid"

entry arrange_from_coordinates
  (grid_xs: []f64) (grid_ys: []f64)
  (pts_x: []f64)  (pts_y: []f64)
  : ([]f64, []f64) =
  arrange_from_coordinates_ grid_xs grid_ys pts_x pts_y

entry grid_in_polygons
  (xs: []f64)
  (ys: []f64)
  (ring_offsets: []i64)
  (polygon_ring_counts: []i64)
  (nx: i64)
  (ny: i64)
  : ([]f64, []f64) =
  grid_in_polygons_ xs ys ring_offsets polygon_ring_counts nx ny