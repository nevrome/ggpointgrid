import "lib/github.com/diku-dk/sorts/radix_sort"

def sortIndexf64 [n] (xs: [n]f64): [n]i32 =
    zip xs (iota n)
    |> radix_sort_float_by_key (.0) f64.num_bits f64.get_bit
    |> map ((.1) >-> i32.i64)

def nubi32 [n] (bs: [n]i32): []i32 =
    loop acc = [] for i < length bs do (filter (!= bs[i]) acc) ++ [bs[i]]

def searchIndezesi32 [n] (s: i32) (xs: [n]i32): []i32 =
    zip xs (iota n)
    |> filter (\(x,_) -> x == s)
    |> map ((.1) >-> i32.i64)

def arrange [n] (gridId: [n]i32) (meanPointId: [n]i32) (distance: [n]f64) : ([]i32, i32) =
    let sortedIndices = sortIndexf64 distance
    let uniquePointIds = nubi32 meanPointId
    --let gu = map (\x -> ) uniquePointIds
    in (searchIndezesi32 2 meanPointId, gridId[head(sortedIndices)])

def main [n] (gridId: [n]i32) (meanPointId: [n]i32) (distance: [n]f64): ([]i32, i32) =
    arrange gridId meanPointId distance

-- futhark c arrange.fut
-- echo [1,2,3] [1,1,2] [0.1,0,0.2] | ./arrange


