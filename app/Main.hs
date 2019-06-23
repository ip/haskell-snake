module Main where

import Control.Concurrent
import System.Random
import Vec2
import Vec2.Random
import Snake.Core
import Snake.Io


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
-- Side effects
---------------

initGame :: IO GameState
initGame = do
    initIo
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
        where screenSize = Vec2 24 16
