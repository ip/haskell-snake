module Snake.Core (
    GameState (..),
    Direction (..),
    Inputs (..),
    fieldSize,
    initSnake,
    updateState,
) where

import System.Random (StdGen)
import SDL.Vect (V2 (..))


data GameState = GameState {
    randomGen :: StdGen,
    foodPosition :: V2 Int,
    -- Snake head is the list head
    snakeBody :: [V2 Int],
    snakeLength :: Int,
    direction :: V2 Int
} deriving (Show)

fieldSize :: V2 Int
fieldSize = V2 24 18

-- Inputs from a given frame
data Inputs = Inputs (Maybe Direction)

data Direction = DirectionLeft | DirectionRight | DirectionUp | DirectionDown


updateState :: Inputs -> GameState -> GameState
updateState i = eatFood . moveSnake . updateDirection_ i


eatFood :: GameState -> GameState
eatFood = growSnakeOnEat

growSnakeOnEat :: GameState -> GameState
growSnakeOnEat s
    | isAtFood s = updateSnakeLength (+1) s
    | otherwise  = s

isAtFood :: GameState -> Bool
isAtFood s = snakeHead s == foodPosition s


updateDirection_ :: Inputs -> GameState -> GameState
updateDirection_ (Inputs dir) = updateDirection $ updateDirectionOnInput dir

updateDirectionOnInput :: Maybe Direction -> V2 Int -> V2 Int
updateDirectionOnInput (Just DirectionUp) _    = V2 0    (-1)
updateDirectionOnInput (Just DirectionDown) _  = V2 0    1
updateDirectionOnInput (Just DirectionRight) _ = V2 1    0
updateDirectionOnInput (Just DirectionLeft) _  = V2 (-1) 0
updateDirectionOnInput Nothing d               = d

initSnake :: V2 Int -> [V2 Int]
initSnake fieldSize = [(`div` 2) <$> fieldSize]

moveSnake :: GameState -> GameState
moveSnake = trimSnake . growSnake

growSnake :: GameState -> GameState
growSnake s = updateSnakeBody ((:) newHead) s
    where newHead  = snakeHead s + direction s

trimSnake :: GameState -> GameState
trimSnake s = updateSnakeBody trimSnake_ s
    where trimSnake_ = take (snakeLength s)

snakeHead :: GameState -> V2 Int
snakeHead = head . snakeBody

-- Setters

updateSnakeBody :: ([V2 Int] -> [V2 Int]) -> GameState -> GameState
updateSnakeBody f s = GameState {
    randomGen = randomGen s,
    foodPosition = foodPosition s,
    snakeLength = snakeLength s,
    direction = direction s,

    snakeBody = f $ snakeBody s
}

updateDirection :: (V2 Int -> V2 Int) -> GameState -> GameState
updateDirection f s = GameState {
    randomGen = randomGen s,
    foodPosition = foodPosition s,
    snakeBody = snakeBody s,
    snakeLength = snakeLength s,

    direction = f $ direction s
}

updateSnakeLength :: (Int -> Int) -> GameState -> GameState
updateSnakeLength f s = GameState {
    randomGen = randomGen s,
    foodPosition = foodPosition s,
    snakeBody = snakeBody s,
    direction = direction s,

    snakeLength = f $ snakeLength s
}
