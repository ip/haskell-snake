module Main where

import Control.Concurrent
import Control.Monad
import HsCharm

data Vec2 = Vec2 {
    x :: Int,
    y :: Int
} deriving (Show)

data GameState = GameState {
    foodPosition :: Vec2
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
    renderFrame state
    inputs <- getInputs
    let nextState = updateState inputs state in return nextState

-- Pure game logic

updateState :: Inputs -> GameState -> GameState
updateState _ = id

addVec2 :: Vec2 -> Vec2 -> Vec2
a `addVec2` b = Vec2 {
    x = x a + x b,
    y = y a + y b
}

-- Side effects

initGame :: IO GameState
initGame = do
    startCharm
    return GameState {
        foodPosition = Vec2 5 5
    }


renderFrame :: GameState -> IO ()
renderFrame state = do
    clearScreen
    drawFood state

drawFood :: GameState -> IO ()
drawFood state = do
    let Vec2 x y = foodPosition state in moveCursor x y
    blotChar 'o'


getInputs :: IO Inputs
getInputs = return Inputs

getScreenSize :: IO Vec2
getScreenSize = do
    w <- getWidth
    h <- getHeight
    return $ Vec2 w h
