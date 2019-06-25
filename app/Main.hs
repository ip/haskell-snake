module Main where

import Control.Concurrent
import System.Random
import Vec2
import Vec2.Random
import Snake.Core
import Snake.Io


frameDelay = 300 * 1000 -- Microseconds

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
initGame = do
    randomGen <- getStdGen
    let (initialFoodPosition, randomGen2) = randomVec2 randomGen fieldSize in
        return GameState {
            randomGen = randomGen2,
            foodPosition = initialFoodPosition,
            snakeBody = initSnake fieldSize,
            snakeLength = 5,
            direction = Vec2 0 1
        }
