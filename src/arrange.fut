import "lib/github.com/diku-dk/sorts/radix_sort"

-- sort indices by f64 key (ascending)
def sortIndicesf64 [n] (xs: [n]f64): [n]i64 =
  zip xs (iota n)
  |> radix_sort_float_by_key (.0) f64.num_bits f64.get_bit
  |> map (.1)

-- expand two 1D axes into a flattened grid (optional helper)
def expand_grid (xs: []f64) (ys: []f64): ([]f64, []f64) =
  let gx = length xs
  let grid_xs_2d = map (\_y -> xs) ys
  let grid_ys_2d = map (\y  -> replicate gx y) ys
  in (flatten grid_xs_2d, flatten grid_ys_2d)

-- build all pairwise squared distances between grid cells and points
def pairwise_squared_distances
  (grid_xs: []f64) (grid_ys: []f64)
  (pts_x: []f64)  (pts_y: []f64)
  : ([]i32, []i32, []f64) =
  let m = length grid_xs
  let n = length pts_x
  let dmat: [][]f64 =
    map (\gi ->
      map (\pj ->
        let dx = grid_xs[gi] - pts_x[pj]
        let dy = grid_ys[gi] - pts_y[pj]
        in dx*dx + dy*dy
      ) (iota n)
    ) (iota m)
  let grid_ids_row: []i32 = map i32.i64 (iota m)
  let grid_ids_2d: [][]i32 = map (\gi -> replicate n gi) grid_ids_row
  let point_ids_col: []i32 = map i32.i64 (iota n)
  let point_ids_2d: [][]i32 = replicate m point_ids_col
  in (flatten grid_ids_2d, flatten point_ids_2d, flatten dmat)

-- returns exactly n matches when m >= n
def greedy_match_sorted
  (gridIds: []i32) (pointIds: []i32) (distances: []f64)
  (m: i64) (n: i64):
  ([]i32, []i32, []f64) =
  let L = length distances
  let idx = sortIndicesf64 distances
  let out_g0 = replicate n 0i32
  let out_p0 = replicate n 0i32
  let out_d0 = replicate n 0.0f64
  let grid_taken0 = replicate m false
  let point_taken0 = replicate n false
  let (_, _, _, og, op, od, _) =
    loop (i, grid_taken, point_taken, out_g, out_p, out_d, cnt) =
      (0i64, grid_taken0, point_taken0, out_g0, out_p0, out_d0, 0i64)
    while i < L && cnt < n do
      let j = idx[i]
      let g = gridIds[j]
      let p = pointIds[j]
      let gi = i64.i32 g
      let pi = i64.i32 p
      in  if not grid_taken[gi] && not point_taken[pi] then
            let out_g' = out_g with [cnt] = g
            let out_p' = out_p with [cnt] = p
            let out_d' = out_d with [cnt] = distances[j]
            let grid_taken' = grid_taken with [gi] = true
            let point_taken' = point_taken with [pi] = true
            in (i+1, grid_taken', point_taken', out_g', out_p', out_d', cnt+1)
          else
            (i+1, grid_taken, point_taken, out_g, out_p, out_d, cnt)
  in (og, op, od)

-- returns per-point assigned grid coordinates (same order as input points)
entry arrange_from_coordinates
  (grid_xs: []f64) (grid_ys: []f64)
  (pts_x: []f64)  (pts_y: []f64)
  : ([]f64, []f64) =
  let m = length grid_xs
  let n = length pts_x
  let (gridIds0, pointIds0, distances0) =
    pairwise_squared_distances grid_xs grid_ys pts_x pts_y
  let (gs, ps, _) = greedy_match_sorted gridIds0 pointIds0 distances0 m n
  let ps_i64 = map i64.i32 ps
  let xs_assign = map (\g -> grid_xs[i64.i32 g]) gs
  let ys_assign = map (\g -> grid_ys[i64.i32 g]) gs
  let out_x = scatter (copy pts_x) ps_i64 xs_assign
  let out_y = scatter (copy pts_y) ps_i64 ys_assign
  in (out_x, out_y)

-- alternativ interface that also does the grid expansion
def arrange_points_on_grid_from_gridvectors
  (grid_x: []f64) (grid_y: []f64)
  (pts_x: []f64) (pts_y: []f64):
  ([]f64, []f64) =
  let (gx, gy) = expand_grid grid_x grid_y
  in arrange_from_coordinates gx gy pts_x pts_y

--def main : ([]f64, []f64) =
--    arrange_points_on_grid_from_axisvectors [1,2,3,4] [1,2,3,4] [1,1,1,1] [1,1,1,1]

-- futhark c arrange.fut
-- echo | ./arrange
