def arrange (distanceTable: [](i32,i32,f64)) : (f64,i32) = -- (gridTable: [](i32,i32)) [](i32,i32,f64) =
    let hu = reduce (\((dist1,index1),(dist2,index2)) -> if dist2 < dist1 then (dist2,index2) else (dist1,index1)) (5,0) (zip (indices distanceTable) (map (\(_,_,dist) -> dist) distanceTable))
    in hu

def main [n] (gridId: [n]i32) (meanPointId: [n]i32) (distance: [n]f64): ([n]i32, [n]i32) =
    zip3 gridId meanPointId distance |> arrange |> unzip

