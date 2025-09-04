import "lib/github.com/diku-dk/sorts/radix_sort"

-- sort indices by f64 key (ascending)
def sortIndicesf64 [n] (xs: [n]f64): [n]i64 =
  zip xs (iota n)
  |> radix_sort_float_by_key (.0) f64.num_bits f64.get_bit
  |> map (.1)

-- build all pairwise squared distances between grid cells and points
def pairwise_squared_distances
  (grid_xs: []f64) (grid_ys: []f64)
  (pts_x: []f64) (pts_y: []f64)
  (m: i64) (n: i64)
  : ([]i64, []i64, []f64) =
  let total = m * n
  let gridIds_flat = map (\gi -> replicate n gi) (iota m) |> flatten
  let pointIds_flat = replicate m (iota n) |> flatten
  let dists = map (\idx ->
    let gi = idx / n
    let pj = idx % n
    let dx = grid_xs[gi] - pts_x[pj]
    let dy = grid_ys[gi] - pts_y[pj]
    in dx*dx + dy*dy
  ) (iota total)
  in (gridIds_flat, pointIds_flat, dists)

-- returns exactly n matches when m >= n
def greedy_match_sorted
  (gridIds: []i64) (pointIds: []i64) (distances: []f64)
  (m: i64) (n: i64)
  : ([]i64, []i64) =
  let L = length distances
  let idx = sortIndicesf64 distances
  let out_g0 = replicate n 0i64
  let out_p0 = replicate n 0i64
  let grid_taken0 = replicate m false
  let point_taken0 = replicate n false
  let (_, _, _, og, op, _) =
    loop (i, grid_taken, point_taken, out_g, out_p, cnt) =
      (0i64, grid_taken0, point_taken0, out_g0, out_p0, 0i64)
    while i < L && cnt < n do
      let j = idx[i]
      let g = gridIds[j]
      let p = pointIds[j]
      in  if not grid_taken[g] && not point_taken[p] then
            let out_g' = out_g with [cnt] = g
            let out_p' = out_p with [cnt] = p
            let grid_taken' = grid_taken with [g] = true
            let point_taken' = point_taken with [p] = true
            in (i+1, grid_taken', point_taken', out_g', out_p', cnt+1)
          else
            (i+1, grid_taken, point_taken, out_g, out_p, cnt)
  in (og, op)

-- returns per-point assigned grid coordinates (same order as input points)
entry arrange_from_coordinates
  (grid_xs: []f64) (grid_ys: []f64)
  (pts_x: []f64)  (pts_y: []f64)
  : ([]f64, []f64) =
  let m = length grid_xs
  let n = length pts_x
  let (gridIds0, pointIds0, distances0) =
    pairwise_squared_distances grid_xs grid_ys pts_x pts_y m n
  let (gs, ps) = greedy_match_sorted gridIds0 pointIds0 distances0 m n
  let xs_assign = map (\g -> grid_xs[g]) gs
  let ys_assign = map (\g -> grid_ys[g]) gs
  let out_x0 = replicate n 0.0
  let out_y0 = replicate n 0.0
  let out_x = scatter out_x0 ps xs_assign
  let out_y = scatter out_y0 ps ys_assign
  in (out_x, out_y)

-- expand two 1D axes into a flattened grid (optional helper)
def expand_grid (xs: []f64) (ys: []f64): ([]f64, []f64) =
  let gx = length xs
  let grid_xs_2d = map (\_y -> xs) ys
  let grid_ys_2d = map (\y  -> replicate gx y) ys
  in (flatten grid_xs_2d, flatten grid_ys_2d)
  
-- alternativ interface that also does the grid expansion
def arrange_points_on_grid_from_gridvectors
  (grid_x: []f64) (grid_y: []f64)
  (pts_x: []f64) (pts_y: []f64)
  : ([]f64, []f64) =
  let (gx, gy) = expand_grid grid_x grid_y
  in arrange_from_coordinates gx gy pts_x pts_y

-- only there for testing purposes
def main 
   (grid_xs: []f64) (grid_ys: []f64)
   (pts_x: []f64)  (pts_y: []f64)
   : ([]f64, []f64) =
     arrange_points_on_grid_from_gridvectors grid_xs grid_ys pts_x pts_y

-- running on the command line
-- futhark c arrange.fut
-- echo [1,2,3,4] [1,2,3,4] [1,1,1,1] [1,1,1,1] | ./arrange

-- profiling
-- see https://futhark.readthedocs.io/en/latest/man/futhark-profile.html
-- futhark bench -v --backend=multicore --profile --json result.json arrange.fut
-- futhark profile result.json

-- profiling runs
-- https://futhark-lang.org/examples/benchmarking.html
-- ==
-- compiled input { [1.0,2.0,3.0,4.0] [1.0,2.0,3.0,4.0] [1.0,1.0,1.0,1.0] [1.0,1.0,1.0,1.0] }

