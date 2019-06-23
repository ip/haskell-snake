module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import HsCharm
import Vec2
import Vec2.Random


data GameState = GameState {
    randomGen :: StdGen,
    screenSize :: Vec2,
    foodPosition :: Vec2,
    -- Snake head is the list head
    snakeBody :: [Vec2],
    snakeLength :: Int,
    direction :: Vec2
} deriving (Show)

data Inputs = Inputs

frameDelay = 500 * 1000 -- Microseconds

main :: IO ()
main = do
    initialState <- initGame
    runLoop initialState

runLoop :: GameState -> IO ()
runLoop prevState = do
    nextState <- runFrame prevState
    threadDelay frameDelay
    runLoop nextState

runFrame :: GameState -> IO GameState
runFrame state = do
    drawFrame state
    inputs <- getInputs
    let nextState = updateState inputs state in return nextState

---------------
-- Pure part
---------------

updateState :: Inputs -> GameState -> GameState
updateState _ = moveSnake

initSnake :: Vec2 -> [Vec2]
initSnake screenSize = [screenSize // 2]

moveSnake :: GameState -> GameState
moveSnake = trimSnake . growSnake

growSnake :: GameState -> GameState
growSnake s = updateSnakeBody ((:) newHead) s
    where newHead  = prevHead + direction s
          prevHead = head $ snakeBody s

trimSnake :: GameState -> GameState
trimSnake s = updateSnakeBody trimSnake_ s
    where trimSnake_ = take (snakeLength s)

updateSnakeBody :: ([Vec2] -> [Vec2]) -> GameState -> GameState
updateSnakeBody f s = GameState {
    randomGen = randomGen s,
    screenSize = screenSize s,
    foodPosition = foodPosition s,
    snakeLength = snakeLength s,
    direction = direction s,

    snakeBody = f $ snakeBody s
}

---------------
-- Side effects
---------------

initGame :: IO GameState
initGame = do
    startCharm
    screenSize <- getScreenSize
    randomGen <- getStdGen
    let (initialFoodPosition, randomGen2) = randomVec2 randomGen screenSize in
        return GameState {
            randomGen = randomGen2,
            screenSize = screenSize,
            foodPosition = initialFoodPosition,
            snakeBody = initSnake screenSize,
            snakeLength = 5,
            direction = Vec2 0 1
        }


drawFrame :: GameState -> IO ()
drawFrame state = do
    clearScreen
    drawFood state
    drawSnake state

drawFood :: GameState -> IO ()
drawFood state = drawChar 'o' pos
        where pos = foodPosition state

drawSnake :: GameState -> IO ()
drawSnake state = mapM_ drawLink snakeBody_
        where snakeBody_ = snakeBody state
              drawLink = drawChar '#'

drawChar :: Char -> Vec2 -> IO ()
drawChar char pos = do
    moveCursor (x pos) (y pos)
    blotChar char

getInputs :: IO Inputs
getInputs = return Inputs

getScreenSize :: IO Vec2
getScreenSize = do
    w <- getWidth
    h <- getHeight
    return $ Vec2 w h
