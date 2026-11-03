-- build all pairwise squared distances between grid cells and points
def pairwise_squared_distances [m][n]
  (grid_xs: [m]f64) (grid_ys: [m]f64)
  (pts_x: [n]f64) (pts_y: [n]f64)
  : [n][m]f64 =
  map (\p ->
    map (\g ->
      let dx = grid_xs[g] - pts_x[p]
      let dy = grid_ys[g] - pts_y[p]
      in dx*dx + dy*dy
    ) (iota m)
  ) (iota n)

-- solve a rectangular assignment problem with the Hungarian algorith
-- cost has shape [number_of_points][number_of_grid_cells]
-- the result has length n: result[p] = grid cell assigned to point p
-- every point is assigned, and every selected grid cell is distinct
def rectangular_assignment [n][m] (cost: [n][m]f64) : [n]i64 =
  -- the implementation uses 1-based indexing internally.
  -- u: potentials for points/rows.
  -- v: potentials for grid cells/columns.
  -- p[g]: point currently assigned to grid cell g.
  let u0 = replicate (n + 1) 0.0
  let v0 = replicate (m + 1) 0.0
  let p0 = replicate (m + 1) 0i64
  let (_, _, p_final) =
    loop (u, v, p) = (u0, v0, p0)
    for ii < n do
      let i = ii + 1
      -- column zero is a dummy column containing the point
      -- whose augmenting path is currently being constructed
      let p_start = p with [0] = i
      let minv0 = replicate (m + 1) f64.inf
      let used0 = replicate (m + 1) false
      let way0 = replicate (m + 1) 0i64
      -- find an augmenting path ending in an unassigned grid cell
      let (u1, v1, _, _, way1, free_grid) =
        loop (u_s, v_s, minv_s, used_s, way_s, j0) =
          (u, v, minv0, used0, way0, 0i64)
        while p_start[j0] != 0 do
          let used1 = used_s with [j0] = true
          let i0 = p_start[j0]
          -- update reduced costs and find the closest unvisited
          -- grid cell
          let (minv1, way1, delta, j1) =
            loop (minv_a, way_a, delta_a, j1_a) =
              (minv_s, way_s, f64.inf, 0i64)
            for jj < m do
              let j = jj + 1
              in if not used1[j] then
                   let cur = cost[i0 - 1][j - 1] - u_s[i0] - v_s[j]
                   let old = minv_a[j]
                   let better = cur < old
                   let new_min = if better then cur else old
                   let way_b = if better then way_a with [j] = j0 else way_a
                   let (delta_b, j1_b) =
                     if new_min < delta_a
                     then (new_min, j)
                     else (delta_a, j1_a)
                   in (minv_a with [j] = new_min, way_b, delta_b, j1_b)
                 else (minv_a, way_a, delta_a, j1_a)
          -- update the dual potentials
          let (u2, v2, minv2) =
            loop (u_a, v_a, minv_a) = (u_s, v_s, minv1)
            for j < m + 1 do
              if used1[j] then
                let row = p_start[j]
                let u_b = u_a with [row] = u_a[row] + delta
                let v_b = v_a with [j] = v_a[j] - delta
                in (u_b, v_b, minv_a)
              else if j > 0 then
                let minv_b = minv_a with [j] = minv_a[j] - delta
                in (u_a, v_a, minv_b)
              else (u_a, v_a, minv_a)
          in (u2, v2, minv2, used1, way1, j1)
      -- reverse the augmenting path, thereby assigning the new point
      let (p2, _) =
        loop (p_a, j) = (p_start, free_grid)
        while j != 0 do
          let previous = way1[j]
          let p_b = p_a with [j] = p_a[previous]
          in (p_b, previous)
      in (u1, v1, p2)
  -- p_final maps grid cells to points. Invert this to obtain point -> grid cell
  let point_to_grid0 = replicate n (-1i64)
  let point_to_grid =
    loop result = point_to_grid0
    for jj < m do
      let j = jj + 1
      let point = p_final[j]
      in if point != 0
         then result with [point - 1] = j - 1
         else result
  in point_to_grid

entry arrange_from_coordinates2
  (grid_xs: []f64) (grid_ys: []f64)
  (pts_x: []f64) (pts_y: []f64)
  : ([]f64, []f64) =
  let m = length grid_xs
  let n = length pts_x
  let dists = pairwise_squared_distances grid_xs grid_ys pts_x pts_y
  let grid_for_point =
    assert (n <= m)
    rectangular_assignment dists
  let out_x = map (\g -> grid_xs[g]) grid_for_point
  let out_y = map (\g -> grid_ys[g]) grid_for_point
  in (out_x, out_y)
