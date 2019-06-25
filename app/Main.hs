module Main where

import Control.Concurrent
import System.Random
import Snake.Core
import Snake.Io
import Snake.RandomVec


frameDelay = 200 * 1000 -- Microseconds

main :: IO ()
main = do
    snakeIo <- initIo
    randomGen <- getStdGen
    let initialState = initState randomGen in
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
