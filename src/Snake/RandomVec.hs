module Snake.RandomVec (
    randomV2Unbounded,
    randomV2
) where

import System.Random
import SDL.Vect (V2 (..))


randomV2Unbounded :: StdGen -> (V2 Int, StdGen)
randomV2Unbounded g =
    let (x, g2) = next g in
    let (y, g3) = next g2 in
        (V2 x y, g3)

-- Returns random V2 with coordinates less than in the provided V2
randomV2 :: StdGen -> V2 Int -> (V2 Int, StdGen)
randomV2 g boundary =
    let (V2 x y, g2) = randomV2Unbounded g in
        (V2 (x `mod` bx) (y `mod` by), g2)
    where (V2 bx by) = boundary
