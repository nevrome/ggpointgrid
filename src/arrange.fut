import "lib/github.com/diku-dk/sorts/radix_sort"

def i32iota n = map i32.i64 (iota (i64.i32 n))
def i32replicate n x = replicate (i64.i32 n) x
def i32length xs = length xs |> i32.i64

-- sort indices by f32 key (ascending)
def sortIndicesf32 [n] (xs: [n]f32): [n]i32 =
  zip xs (iota n |> map i32.i64)
  |> radix_sort_float_by_key (.0) f32.num_bits f32.get_bit
  |> map (.1)

-- build all pairwise squared distances between grid cells and points
def pairwise_squared_distances
  (grid_xs: []f32) (grid_ys: []f32)
  (pts_x: []f32) (pts_y: []f32)
  (m: i32) (n: i32)
  : ([]i32, []i32, []f32) =
  let total: i32 = m * n
  let gridIds_flat: []i32 =
    map (\(gi: i32) -> i32replicate n gi) (i32iota m) |> flatten
  let pointIds_flat: []i32 =
    i32replicate m (i32iota n) |> flatten
  let dists: []f32 = map (\(idx: i32) ->
    let gi: i32 = idx / n
    let pj: i32 = idx % n
    let dx = grid_xs[gi] - pts_x[pj]
    let dy = grid_ys[gi] - pts_y[pj]
    in dx*dx + dy*dy
  ) (i32iota total)
  in (gridIds_flat, pointIds_flat, dists)

-- returns exactly n matches when m >= n
def greedy_match_sorted
  (gridIds: []i32) (pointIds: []i32) (distances: []f32)
  (m: i32) (n: i32)
  : ([]i32, []i32) =
  let L: i32 = i32length distances
  let idx: []i32 = sortIndicesf32 distances
  let out_g0: []i32 = i32replicate n 0i32
  let out_p0: []i32 = i32replicate n 0i32
  let grid_taken0: []bool = i32replicate m false
  let point_taken0: []bool = i32replicate n false
  let (_, _, _, og, op, _) =
    loop (i: i32, grid_taken: *[]bool, point_taken: *[]bool,
          out_g: *[]i32, out_p: *[]i32, cnt: i32) =
      (0i32, grid_taken0, point_taken0, out_g0, out_p0, 0i32)
    while i < L && cnt < n do
      let j: i32 = idx[i]
      let g: i32 = gridIds[j]
      let p: i32 = pointIds[j]
      in if not grid_taken[g] && not point_taken[p] then
           let out_g' = out_g with [cnt] = g
           let out_p' = out_p with [cnt] = p
           let grid_taken' = grid_taken with [g] = true
           let point_taken' = point_taken with [p] = true
           in (i+1i32, grid_taken', point_taken', out_g', out_p', cnt+1i32)
         else
           (i+1i32, grid_taken, point_taken, out_g, out_p, cnt)
  in (og, op)

-- returns per-point assigned grid coordinates (same order as input points)
entry arrange_from_coordinates
  (grid_xs: []f32) (grid_ys: []f32)
  (pts_x: []f32)  (pts_y: []f32)
  : ([]f32, []f32) =
  let m: i32 = i32length grid_xs
  let n: i32 = i32length pts_x
  let (gridIds0, pointIds0, distances0) =
    pairwise_squared_distances grid_xs grid_ys pts_x pts_y m n
  let (gs, ps) = greedy_match_sorted gridIds0 pointIds0 distances0 m n
  let xs_assign: []f32 = map (\(g: i32) -> grid_xs[g]) gs
  let ys_assign: []f32 = map (\(g: i32) -> grid_ys[g]) gs
  let out_x0: []f32 = i32replicate n 0.0f32
  let out_y0: []f32 = i32replicate n 0.0f32
  let out_x: []f32 = scatter out_x0 (map i64.i32 ps) xs_assign
  let out_y: []f32 = scatter out_y0 (map i64.i32 ps) ys_assign
  in (out_x, out_y)

-- expand two 1D axes into a flattened grid (optional helper)
def expand_grid (xs: []f32) (ys: []f32): ([]f32, []f32) =
  let gx: i32 = i32length xs
  let grid_xs_2d = map (\_y -> xs) ys
  let grid_ys_2d = map (\(y: f32) -> i32replicate gx y) ys
  in (flatten grid_xs_2d, flatten grid_ys_2d)
  
-- alternativ interface that also does the grid expansion
def arrange_points_on_grid_from_gridvectors
  (grid_x: []f32) (grid_y: []f32)
  (pts_x: []f32) (pts_y: []f32)
  : ([]f32, []f32) =
  let (gx, gy) = expand_grid grid_x grid_y
  in arrange_from_coordinates gx gy pts_x pts_y

-- only there for testing purposes
def main 
   (grid_xs: []f32) (grid_ys: []f32)
   (pts_x: []f32)  (pts_y: []f32)
   : ([]f32, []f32) =
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

