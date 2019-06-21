module Main where

import Control.Concurrent
import Control.Monad
import HsCharm

main :: IO ()

{--
-- A REPL which multiplies numbers by 2
someFunc = runStep

runStep :: IO ()
runStep = do
    putStrLn "Enter a number or \"q\" to quit: "
    line <- getLine
    if line == "q" then
        return ()
    else do
        let x = read line :: Integer
            in putStrLn $ show x ++ " * 2 = " ++ (show $ x * 2)
        runStep
--}

-- someFunc = tick 10

-- tick :: Integer -> IO ()
-- tick step = do
--     print step
--     threadDelay 100000
--     when (step > 0) $ tick (step - 1)



data GameState = GameState Integer

frameDelay = 300 * 1000 -- Microseconds
initialState = GameState 0

main = do
    initGame
    runLoop initialState

runLoop :: GameState -> IO ()
runLoop prevState = do
    nextState <- runFrame prevState
    threadDelay frameDelay
    runLoop nextState

initGame :: IO ()
initGame = startCharm

runFrame :: GameState -> IO GameState
runFrame state = do
    -- w <- getWidth
    -- h <- getHeight
    clearScreen
    let i_ = fromIntegral i in moveCursor i_ i_
    blotChar 'o'
    return $ GameState (i + 1)
        where GameState i = state
