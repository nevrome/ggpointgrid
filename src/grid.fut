type point = (f64, f64)

def min_f64 (a: f64) (b: f64) : f64 = if a < b then a else b
def max_f64 (a: f64) (b: f64) : f64 = if a > b then a else b

-- check whether a horizontal ray from (px,py) crosses edge (x1,y1)-(x2,y2)
def ray_crosses_edge
  (px: f64) (py: f64)
  (x1: f64) (y1: f64)
  (x2: f64) (y2: f64)
  : bool =
  if (y1 > py) != (y2 > py)
  then
    let xint = x1 + (py - y1) * (x2 - x1) / (y2 - y1)
    in xint > px
  else false

-- check if a point is in a ring of a polygon
def point_in_ring [n]
  (xs: [n]f64) (ys: [n]f64)
  (ring_offsets: []i64)
  (r: i64)
  (px: f64) (py: f64)
  : bool =
  let s = ring_offsets[r]
  let e = ring_offsets[r + 1]
  let m = e - s
  let idxs = iota m
  let crossings =
    map (\i ->
      let j = s + i
      let k = if j + 1 < e then j + 1 else s
      let x1 = xs[j]
      let y1 = ys[j]
      let x2 = xs[k]
      let y2 = ys[k]
      in if ray_crosses_edge px py x1 y1 x2 y2 then 1i32 else 0i32
    ) idxs
  let total = reduce (+) 0i32 crossings
  in (total % 2i32) == 1i32

-- check if a point is in a single polygon,
-- inside its outer ring and not inside any hole.
def point_in_polygon [n][k][m]
  (xs: [n]f64) (ys: [n]f64)
  (ring_offsets: [k]i64)
  (polygon_offsets: [m]i64)
  (p: i64)
  (px: f64) (py: f64)
  : bool =
  let rs = polygon_offsets[p]
  let re = polygon_offsets[p + 1]
  in if rs >= re
     then false
     else
       let outer_inside = point_in_ring xs ys ring_offsets rs px py
       let hole_ids = iota (re - rs - 1)
       let in_hole =
         reduce (||) false
           (map (\i ->
              let r = rs + 1 + i
              in point_in_ring xs ys ring_offsets r px py
            ) hole_ids)
       in outer_inside && not in_hole

-- ceck if a point is in any polygons
def point_in_any_polygon [n][k][m]
  (xs: [n]f64) (ys: [n]f64)
  (ring_offsets: [k]i64)
  (polygon_offsets: [m]i64)
  (px: f64) (py: f64)
  : bool =
  let poly_ids = iota (length polygon_offsets - 1)
  in reduce (||) false
       (map (\p -> point_in_polygon xs ys ring_offsets polygon_offsets p px py)
            poly_ids)

-- create a regular grid inside polygons
entry grid_in_polygons_
  -- xs + ys: all polygon/ring coordinates in a flat format
  (xs: []f64)
  (ys: []f64)
  -- ring_offsets: ring offset index lists
  -- rings in polygons are differentiated with ring_offsets
  -- these document when one ring ends and the next begins
  -- in the coordinate table
  -- the first ring of a polygon is the outer boundary,
  -- subsequent rings are holes
  (ring_offsets: []i64)
  -- polygon_ring_counts: list with number of rings per polygon
  (polygon_ring_counts: []i64)
  -- gx + gy: Grid coordinates in x and y direction
  (gx: []f64)
  (gy: []f64)
  -- return good grid points as separate x/y arrays
  : ([]f64, []f64) =
  let polygon_offsets = [0] ++ scan (+) 0 polygon_ring_counts
  let flat_ids = indices gx
  let inside =
    map2 (\px py -> point_in_any_polygon xs ys ring_offsets polygon_offsets px py)
         gx gy
  let idx = filter (\i -> inside[i]) flat_ids
  let out_x = map (\i -> gx[i]) idx
  let out_y = map (\i -> gy[i]) idx
  in (out_x, out_y)

------ for testing/debugging/profiling ------

-- direct test on the command line
-- futhark c grid.fut
-- echo [0,10,10,0, 3,7,7,3] [0,0,10,10, 3,3,7,7] [0,4,8] [2] [0,2,4,6,8,10] [0,2,4,6,8,10] | ./grid -e grid_in_polygons_

-- get outer edges of bounding box to populate initial grid
-- def bbox_points [n] (xs: [n]f64) (ys: [n]f64) : (f64, f64, f64, f64) =
--   let xmin = reduce min_f64 xs[0] xs
--   let xmax = reduce max_f64 xs[0] xs
--   let ymin = reduce min_f64 ys[0] ys
--   let ymax = reduce max_f64 ys[0] ys
--   in (xmin, ymin, xmax, ymax)

-- expand two 1D axes into a flattened grid (optional helper)
-- def expand_grid [nx] [ny] (xs: [nx]f64) (ys: [ny]f64)
--     : ([ny * nx]f64, [ny * nx]f64) =
--   let grid_xs = flatten (map (\_ -> xs) ys)
--   let grid_ys = flatten (map (\y -> replicate nx y) ys)
--   in (grid_xs, grid_ys)
