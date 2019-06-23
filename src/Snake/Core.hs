module Snake.Core (
    GameState (..),
    Key_ (..),
    Inputs (..),
    initSnake,
    updateState
) where

import System.Random (StdGen)
import Vec2


data GameState = GameState {
    randomGen :: StdGen,
    foodPosition :: Vec2,
    -- Snake head is the list head
    snakeBody :: [Vec2],
    snakeLength :: Int,
    direction :: Vec2
} deriving (Show)

data Key_ = KeyLeft_ | KeyRight_ | KeyUp_ | KeyDown_
data Inputs = Inputs (Maybe Key_)


updateState :: Inputs -> GameState -> GameState
updateState i = moveSnake . updateDirection_ i

updateDirection_ :: Inputs -> GameState -> GameState
updateDirection_ (Inputs key) = updateDirection $ keyToDirection key

keyToDirection :: Maybe Key_ -> Vec2 -> Vec2
keyToDirection (Just KeyUp_) _    = Vec2 0    (-1)
keyToDirection (Just KeyDown_) _  = Vec2 0    1
keyToDirection (Just KeyRight_) _ = Vec2 1    0
keyToDirection (Just KeyLeft_) _  = Vec2 (-1) 0
keyToDirection Nothing d         = d

initSnake :: Vec2 -> [Vec2]
initSnake fieldSize = [fieldSize // 2]

moveSnake :: GameState -> GameState
moveSnake = trimSnake . growSnake

growSnake :: GameState -> GameState
growSnake s = updateSnakeBody ((:) newHead) s
    where newHead  = prevHead + direction s
          prevHead = head $ snakeBody s

trimSnake :: GameState -> GameState
trimSnake s = updateSnakeBody trimSnake_ s
    where trimSnake_ = take (snakeLength s)

-- Setters

updateSnakeBody :: ([Vec2] -> [Vec2]) -> GameState -> GameState
updateSnakeBody f s = GameState {
    randomGen = randomGen s,
    foodPosition = foodPosition s,
    snakeLength = snakeLength s,
    direction = direction s,

    snakeBody = f $ snakeBody s
}

updateDirection :: (Vec2 -> Vec2) -> GameState -> GameState
updateDirection f s = GameState {
    randomGen = randomGen s,
    foodPosition = foodPosition s,
    snakeBody = snakeBody s,
    snakeLength = snakeLength s,

    direction = f $ direction s
}
