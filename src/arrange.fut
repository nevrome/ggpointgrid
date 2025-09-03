import "lib/github.com/diku-dk/sorts/radix_sort"

-- General helper functions

def sortIndicesf64 [n] (xs: [n]f64): [n]i32 =
  zip xs (iota n)
  |> radix_sort_float_by_key (.0) f64.num_bits f64.get_bit
  |> map ((.1) >-> i32.i64)

def nubi32 [n] (bs: [n]i32): []i32 =
  loop acc = [] for i < length bs do (filter (!= bs[i]) acc) ++ [bs[i]]

def searchIndexesi32 [n] (s: i32) (xs: [n]i32): []i32 =
  zip xs (iota n)
  |> filter (\(x,_) -> x == s)
  |> map ((.1) >-> i32.i64)

def isIni32 (x: i32) (xs: []i32) =
  or (map (== x) xs)

def isDuplii32 (x: i32) (xs: []i32): bool =
  (reduce (\acc y -> if x == y then acc + 1 else acc) 0 xs) > 1

def getFirstIndexi32 (x: i32) (xs: []i32): i32 =
  searchIndexesi32 x xs |> head

def getDuplii32 (xs: []i32): []i32 =
  filter (\x -> isDuplii32 x xs) xs

def expand_grid (xs: []f64) (ys: []f64): ([]f64, []f64) =
  let gx = length xs
  let grid_xs_2d = map (\_y -> xs) ys
  let grid_ys_2d = map (\y  -> replicate gx y) ys
  in (flatten grid_xs_2d, flatten grid_ys_2d)

def pairwise_squared_distances
  (grid_xs: []f64) (grid_ys: []f64)
  (pts_x: []f64) (pts_y: []f64):
  ([]i32, []i32, []f64) =
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

-- Core greedy arrange

def getBestGridPointForOneInputPoint [n]
  (gridIds: [n]i32) (pointIds: [n]i32) (distances: [n]f64)
  (distanceIndexes: [n]i32) (pid: i32):
  (i32, i32, f64) =
  let curPointIndexes = searchIndexesi32 pid pointIds
  let minDistIndex = filter (\x -> isIni32 x curPointIndexes) distanceIndexes |> head
  in (gridIds[minDistIndex], pid, distances[minDistIndex])

def getBestGridPointForEachInputPoint [n]
  (gridIds: [n]i32) (pointIds: [n]i32) (distances: [n]f64):
  [](i32, i32, f64) =
  let distanceIndexes = sortIndicesf64 distances
  let uniquePointIds = nubi32 pointIds
  let bests = map (\pid -> getBestGridPointForOneInputPoint gridIds pointIds distances distanceIndexes pid) uniquePointIds
  let (_, _, ds) = unzip3 bests
  let distanceIndexesAmongBests = sortIndicesf64 ds
  in map (\i -> bests[i]) distanceIndexesAmongBests

def arrange_with_distances
  (input: ([](i32, i32, f64), [](i32, i32, f64))):
  [](i32, i32, f64) =
  let (_, output) =
    loop (toHandle, result) = input
    while (length toHandle > 0) do
      let (gridIds, pointIds, distances) = unzip3 toHandle
      let bestCombs = getBestGridPointForEachInputPoint gridIds pointIds distances
      let (gs, _, _) = bestCombs |> unzip3
      let overloadedGridPoints = getDuplii32 gs |> nubi32
      let withoutMultipleCandidates = filter (\(x,_,_) -> not (isIni32 x overloadedGridPoints)) bestCombs
      let withMultipleCandidates = filter (\(x,_,_) -> isIni32 x overloadedGridPoints) bestCombs
      let withMultipleCandidatesDecision =
        map (\x -> withMultipleCandidates[getFirstIndexi32 x (map (.0) withMultipleCandidates)]) overloadedGridPoints
      let good = result ++ withoutMultipleCandidates ++ withMultipleCandidatesDecision
      let (gg, pg, _) = good |> unzip3
      let leftOvers = filter (\(gi,pi,_) -> (not (isIni32 gi gg)) && (not (isIni32 pi pg))) toHandle
      in (leftOvers, good)
  in output

-- returns per-point assigned grid coordinates (same order as input points).
def arrange_from_coordinates
  (grid_xs: []f64) (grid_ys: []f64)
  (pts_x: []f64) (pts_y: []f64)
  : ([]f64, []f64) =
  let m = length grid_xs
  let n = length pts_x
  let _ = assert (m >= n) "The grid is not big enough to accommodate all input points."
  let (gridIds0, pointIds0, distances0) = pairwise_squared_distances grid_xs grid_ys pts_x pts_y
  let solved = arrange_with_distances (zip3 gridIds0 pointIds0 distances0, [])
  let (gs, ps, _) = unzip3 solved
  let out_x = scatter (copy pts_x) (map i64.i32 ps) (map (\gid -> grid_xs[i64.i32 gid]) gs)
  let out_y = scatter (copy pts_y) (map i64.i32 ps) (map (\gid -> grid_ys[i64.i32 gid]) gs)
  in (out_x, out_y)

def arrange_points_on_grid_from_gridvectors
  (grid_x: []f64) (grid_y: []f64)
  (pts_x: []f64) (pts_y: []f64):
  ([]f64, []f64) =
  let (gx, gy) = expand_grid grid_x grid_y
  in arrange_from_coordinates gx gy pts_x pts_y


-- futhark c arrange.fut
-- echo [1,2,3,4] [1,1,1,2] [0.1,0,0.2,0.1] | ./arrange

def main : ([]f64, []f64) =
    arrange_points_on_grid_from_gridvectors [1,2,3,4] [1,2,3,4] [1,1,1,1] [1,1,1,1]
