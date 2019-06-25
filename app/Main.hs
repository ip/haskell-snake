module Main where

import Control.Concurrent
import System.Random
import Snake.Core
import Snake.Io
import Snake.RandomVec
import SDL.Vect (V2 (..))


frameDelay = 200 * 1000 -- Microseconds

main :: IO ()
main = do
    snakeIo <- initIo
    initialState <- initGame
    runLoop initialState snakeIo

runLoop :: GameState -> SnakeIo -> IO ()
runLoop prevState io = do
    nextState <- runFrame prevState io
    threadDelay frameDelay
    runLoop nextState io

runFrame :: GameState -> SnakeIo -> IO GameState
runFrame state io = do
    drawFrame io state
    inputs <- getInputs
    let nextState = updateState inputs state in return nextState

initGame :: IO GameState
initGame = (\randomGen -> initFood $ GameState {
    randomGen = randomGen,
    foodPosition = V2 0 0,
    snakeBody = initSnake fieldSize,
    snakeLength = 5,
    direction = V2 0 1
}) <$> getStdGen
