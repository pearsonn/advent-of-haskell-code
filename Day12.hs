import Util

data V3 = V3 {
    x :: Int,
    y :: Int,
    z :: Int
} deriving (Show, Eq, Ord)

input = map (\p -> (p, initial)) [V3 (-1) (-4) 0, V3 4 7 (-1), V3 (-14) (-10) 9, V3 1 2 17]
initial = V3 0 0 0

part1 :: Int
part1 = sum $ map energy $ iterate step input !! 1000

part2 :: Int
part2 = lcm (pathLength input x) (lcm (pathLength input y) (pathLength input z))

pathLength :: [(V3, V3)] -> (V3 -> Int) -> Int
pathLength state axis = path' (step state) + 1
    where         
        path' moons
            | next == initial = 1
            | otherwise = 1 + path'(step moons)
            where 
                next = map axis $ map fst moons
        initial = map axis $ map fst state        
                

step :: [(V3, V3)] -> [(V3, V3)]
step vs = applyVelocity $ calcGravity vs
    where 
        applyVelocity = map (\((V3 x y z), vel@(V3 vx vy vz)) -> (V3 (x + vx) (y + vy) (z + vz), vel))
        calcGravity = map (\(pos, vel) -> (pos, gravity vel pos))
        gravity v (V3 px py pz) = foldl (\(V3 vx vy vz) (V3 x y z) -> V3 (cmp px x + vx) (cmp py y + vy) (cmp pz z + vz)) v (map fst vs)
        cmp a b
            | a > b = -1
            | a < b = 1
            | otherwise = 0

energy :: (V3, V3) -> Int
energy (V3 x y z, V3 vx vy vz) = (abs x + abs y + abs z) * (abs vx + abs vy + abs vz)