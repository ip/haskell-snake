module Main where

import Control.Concurrent
import Control.Monad
import HsCharm

data Vec2 = Vec2 { x :: Int
                 , y :: Int } deriving (Show)

data GameState = GameState { position :: Vec2
                           , velocity :: Vec2 }

frameDelay = 300 * 1000 -- Microseconds
initialState = GameState { position = Vec2 0 0
                         , velocity = Vec2 1 1 }

main :: IO ()
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
    let Vec2 x y = position state in moveCursor x y
    blotChar 'o'
    return $ updateState state

updateState :: GameState -> GameState
updateState state = GameState { position = (position state) `addVec2` (velocity state)
                              , velocity = velocity state }

addVec2 :: Vec2 -> Vec2 -> Vec2
a `addVec2` b = Vec2 { x = (x a) + (x b)
                     , y = (y a) + (y b) }
