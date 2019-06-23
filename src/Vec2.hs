module Vec2 (
    Vec2 (..),
    (//)
) where


data Vec2 = Vec2 {
    x :: Int,
    y :: Int
} deriving (Show)

instance Num Vec2 where
    a + b = Vec2 (x a + x b) (y a + y b)
    a * b = Vec2 (x a * x b) (y a * y b)
    a - b = Vec2 (x a - x b) (y a - y b)
    abs a = Vec2 (abs $ x a) (abs $ y a)
    signum a = Vec2 (signum $ x a) (signum $ y a)
    fromInteger a = let b = fromInteger a in Vec2 b b

infixl 7 //
(//) :: Vec2 -> Int -> Vec2
(//) v m = Vec2 {
    x = x v `div` m,
    y = y v `div` m
}
