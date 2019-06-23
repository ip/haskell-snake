module Vec2.Random (
    randomVec2Unbounded,
    randomVec2
) where

import System.Random
import Vec2


randomVec2Unbounded :: StdGen -> (Vec2, StdGen)
randomVec2Unbounded g =
    let (x, g2) = next g in
    let (y, g3) = next g2 in
        (Vec2 x y, g3)

-- Returns random Vec2 with coordinates less than in the provided Vec2
randomVec2 :: StdGen -> Vec2 -> (Vec2, StdGen)
randomVec2 g bound =
    let (v, g2) = randomVec2Unbounded g in
        (Vec2 {
            x = x v `mod` x bound,
            y = y v `mod` y bound
        }, g2)
