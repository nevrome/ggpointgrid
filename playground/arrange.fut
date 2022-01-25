import "lib/github.com/diku-dk/sorts/radix_sort"

-- general helper functions

def sortIndexf64 [n] (xs: [n]f64): [n]i32 =
    zip xs (iota n)
    |> radix_sort_float_by_key (.0) f64.num_bits f64.get_bit
    |> map ((.1) >-> i32.i64)

def nubi32 [n] (bs: [n]i32): []i32 =
    loop acc = [] for i < length bs do (filter (!= bs[i]) acc) ++ [bs[i]]

def searchIndexi32 [n] (s: i32) (xs: [n]i32): []i32 =
    zip xs (iota n)
    |> filter (\(x,_) -> x == s)
    |> map ((.1) >-> i32.i64)

def isIni32 (x: i32) (xs: []i32) =
    or (map (== x) xs)

-- specific helper functions

def getBestGridPoint [n] (gridIds: [n]i32) (pointIds: [n]i32) (distanceIndex: [n]i32) (x: i32): i32 = 
    let curPointIndex = searchIndexi32 x pointIds
    let minDistIndex = filter (\x -> isIni32 x curPointIndex) distanceIndex |> head
    in gridIds[minDistIndex]

def bestGridPointForEachInputPoint [n] (gridIds: [n]i32) (pointIds: [n]i32) (distances: [n]f64) : [](i32, i32) =
    let distanceIndex = sortIndexf64 distances
    let uniquePointIds = nubi32 pointIds
    let gridPointComb = map (\x -> (getBestGridPoint gridIds pointIds distanceIndex x, x)) uniquePointIds
    in gridPointComb

def arrange [n] (gridIds: [n]i32) (pointIds: [n]i32) (distances: [n]f64) : [](i32, i32) =
    bestGridPointForEachInputPoint gridIds pointIds distances

def main [n] (gridIds: [n]i32) (pointIds: [n]i32) (distances: [n]f64): (i32, i32) =
    arrange gridIds pointIds distances |> head

-- futhark c arrange.fut
-- echo [1,2,3] [1,1,2] [0.1,0,0.2] | ./arrange


