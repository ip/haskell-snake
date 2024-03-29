module Snake.Core (
    GameState (..),
    Inputs (..),
    Direction (..),
    fieldSize,
    initState,
    updateState
) where

import System.Random (StdGen)
import Snake.RandomVec
import SDL.Vect (V2 (..))

-- Public

data GameState = GameState {
    randomGen :: StdGen,
    foodPosition :: V2 Int,
    -- Snake head is the list head
    snakeBody :: [V2 Int],
    snakeLength :: Int,
    direction :: V2 Int
} deriving (Show)

-- Inputs from a given frame
data Inputs = Inputs (Maybe Direction)

data Direction = DirectionLeft | DirectionRight | DirectionUp | DirectionDown

fieldSize :: V2 Int
fieldSize = V2 24 18

initState :: StdGen -> GameState
initState randomGen = respawnFood $ GameState {
    randomGen = randomGen,
    foodPosition = V2 0 0,
    snakeBody = initSnake fieldSize,
    snakeLength = 1,
    direction = V2 0 1
}

updateState :: Inputs -> GameState -> GameState
updateState i = dieOnCollision . eatFood . moveSnake . updateDirectionOnInput i

-- Internal

eatFood :: GameState -> GameState
eatFood = respawnFoodOnEat . growSnakeOnEat

growSnakeOnEat :: GameState -> GameState
growSnakeOnEat s
    | isAtFood s = updateSnakeLength (+1) s
    | otherwise  = s

respawnFoodOnEat :: GameState -> GameState
respawnFoodOnEat s
    | isAtFood s = respawnFood s
    | otherwise  = s

respawnFood :: GameState -> GameState
respawnFood s =
    let (newPos, newRandomGen) = randomV2 (randomGen s) fieldSize in
        updateFoodPosition (const newPos) $
        updateRandomGen (const newRandomGen) s

isAtFood :: GameState -> Bool
isAtFood s = snakeHead s == foodPosition s


updateDirectionOnInput :: Inputs -> GameState -> GameState
updateDirectionOnInput (Inputs (Just dir)) =
    updateDirection $ const $ directionToVec dir
updateDirectionOnInput (Inputs Nothing)    = id

directionToVec :: Direction -> V2 Int
directionToVec DirectionUp    = V2 0    (-1)
directionToVec DirectionDown  = V2 0    1
directionToVec DirectionRight = V2 1    0
directionToVec DirectionLeft  = V2 (-1) 0

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


dieOnCollision :: GameState -> GameState
dieOnCollision s
        | isCollidingWithSelf s = restartGame s
        | otherwise = s

isCollidingWithSelf :: GameState -> Bool
isCollidingWithSelf s = snakeHead s `elem` bodyExceptHead
    where bodyExceptHead = drop 1 $ snakeBody s

restartGame :: GameState -> GameState
restartGame s = initState randomGen_
    where randomGen_ = randomGen s

-- Setters

updateSnakeBody :: ([V2 Int] -> [V2 Int]) -> GameState -> GameState
updateSnakeBody f s = s { snakeBody = f $ snakeBody s }

updateDirection :: (V2 Int -> V2 Int) -> GameState -> GameState
updateDirection f s = s { direction = f $ direction s }

updateSnakeLength :: (Int -> Int) -> GameState -> GameState
updateSnakeLength f s = s { snakeLength = f $ snakeLength s }

updateFoodPosition :: (V2 Int -> V2 Int) -> GameState -> GameState
updateFoodPosition f s = s { foodPosition = f $ foodPosition s }

updateRandomGen :: (StdGen -> StdGen) -> GameState -> GameState
updateRandomGen f s = s { randomGen = f $ randomGen s }
