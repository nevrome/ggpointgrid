import "lib/github.com/diku-dk/sorts/radix_sort"

-- general helper functions

def sortIndexesi32 [n] (xs: [n]i32): [n]i32 =
    zip xs (iota n)
    |> radix_sort_float_by_key (.0) i32.num_bits i32.get_bit
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

--def is    

-- specific helper functions

def getBestGridPointForOneInputPoint [n] (gridIds: [n]i32) (pointIds: [n]i32) (distances: [n]i32) (distanceIndexes: [n]i32) (x: i32): (i32, i32, i32) = 
    let curPointIndexes = searchIndexesi32 x pointIds
    let minDistIndex = filter (\x -> isIni32 x curPointIndexes) distanceIndexes |> head
    in (gridIds[minDistIndex], x, distances[minDistIndex])

def getBestGridPointForEachInputPoint [n] (gridIds: [n]i32) (pointIds: [n]i32) (distances: [n]i32) : [](i32, i32, i32) =
    let distanceIndexes = sortIndexesi32 distances
    let uniquePointIds = nubi32 pointIds
    let bests = map (\x -> getBestGridPointForOneInputPoint gridIds pointIds distances distanceIndexes x) uniquePointIds
    let (_, _, ds) = bests |> unzip3
    let distanceIndexesAmongBests = sortIndexesi32 ds
    in map (\i -> bests[i]) distanceIndexesAmongBests

def arrange (input: ([](i32, i32, i32),[](i32, i32, i32))) : [](i32, i32, i32) =
    let (_,output) = loop (toHandle, result) = input while (length toHandle > 0) do
        let (gridIds, pointIds, distances) = unzip3 toHandle
        let bestCombs = getBestGridPointForEachInputPoint gridIds pointIds distances
        let (gs, _, _) = bestCombs |> unzip3
        -- find what's still in need of love
        let overloadedGridPoints = getDuplii32 gs |> nubi32
        let withoutMultipleCandidates = filter (\(x,_,_) -> isIni32 x overloadedGridPoints |> not) bestCombs
        let withMultipleCandidates = filter (\(x,_,_) -> isIni32 x overloadedGridPoints) bestCombs
        -- get the best of the best
        let withMultipleCandidatesDecision = map (\x -> withMultipleCandidates[getFirstIndexi32 x (map (.0) withMultipleCandidates)]) overloadedGridPoints
        -- combine the already good stuff
        let good = result ++ withoutMultipleCandidates ++ withMultipleCandidatesDecision
        let (gg, pg, _) = good |> unzip3
        -- combine the leftovers
        let leftOvers = filter (\(gi,pi,_) -> (not (isIni32 gi gg)) && (not (isIni32 pi pg))) toHandle
        in (leftOvers, good)
    in output
    

def main [n] (gridIds: [n]i32) (pointIds: [n]i32) (distances: [n]i32): ([]i32, []i32, []i32) =
    arrange ((zip3 gridIds pointIds distances),[]) |> unzip3

-- futhark c arrange.fut
-- echo [1,2,3,4] [1,1,1,2] [0.1,0,0.2,0.1] | ./arrange


