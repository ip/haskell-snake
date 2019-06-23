module Main where

import Control.Concurrent
import Control.Monad
import HsCharm

data Vec2 = Vec2 { x :: Int
                 , y :: Int } deriving (Show)

data GameState = GameState { position :: Vec2
                           , velocity :: Vec2 } deriving (Show)

data Inputs = Inputs { screenSize :: Vec2 }

frameDelay = 50 * 1000 -- Microseconds
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

runFrame :: GameState -> IO GameState
runFrame state = do
    renderFrame state
    inputs <- getInputs
    let nextState = updateState inputs state in return nextState

-- Pure game logic

updateState :: Inputs -> GameState -> GameState
updateState inputs = bounceOffWalls inputs . advancePosition

advancePosition :: GameState -> GameState
advancePosition s =
    GameState {
        position = position s `addVec2` velocity s,
        velocity = velocity s
    }

bounceOffWalls :: Inputs -> GameState -> GameState
bounceOffWalls inputs s =
    GameState {
        position = p,
        velocity = Vec2 {
            x = if x p == (w - 1) || x p == 0 then -x v else x v,
            y = if y p == (h - 1) || y p == 0 then -y v else y v
        }
    }
        where v = velocity s
              p = position s
              w = x $ screenSize inputs
              h = y $ screenSize inputs

addVec2 :: Vec2 -> Vec2 -> Vec2
a `addVec2` b = Vec2 { x = x a + x b
                     , y = y a + y b }

-- Side effects

initGame :: IO ()
initGame = startCharm

renderFrame :: GameState -> IO ()
renderFrame state = do
    clearScreen
    let Vec2 x y = position state in moveCursor x y
    blotChar 'o'

getInputs :: IO Inputs
getInputs = fmap Inputs getScreenSize

getScreenSize :: IO Vec2
getScreenSize = do
    w <- getWidth
    h <- getHeight
    return $ Vec2 w h
