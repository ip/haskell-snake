module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import HsCharm


-- Vec2

data Vec2 = Vec2 {
    x :: Int,
    y :: Int
} deriving (Show)

instance Num Vec2 where
    a + b = Vec2 (x a + x b) (y a + y b)
    a * b = Vec2 (x a * x b) (y a * y b)
    a - b = Vec2 (x a - x b) (y a - y b)
    -- TODO: remove duplication with signum by instancing Functor and using fmap
    abs a = Vec2 (abs $ x a) (abs $ y a)
    signum a = Vec2 (signum $ x a) (signum $ y a)
    fromInteger a = let b = fromInteger a in Vec2 b b

infixl 7 //
(//) :: Vec2 -> Int -> Vec2
(//) v m = Vec2 {
    x = x v `div` m,
    y = y v `div` m
}


data GameState = GameState {
    randomGen :: StdGen,
    screenSize :: Vec2,
    foodPosition :: Vec2,
    snakeBody :: [Vec2]
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

---------------
-- Pure part
---------------

updateState :: Inputs -> GameState -> GameState
updateState _ = id

initSnake :: Vec2 -> [Vec2]
initSnake screenSize = [screenSize // 2]

-- Random

randomVec2 :: StdGen -> (Vec2, StdGen)
randomVec2 g =
    let (x, g2) = next g in
    let (y, g3) = next g2 in
        (Vec2 x y, g3)

-- Returns random Vec2 with coordinates less than in the provided Vec2
randomVec2Bounded :: StdGen -> Vec2 -> (Vec2, StdGen)
randomVec2Bounded g bound =
    let (v, g2) = randomVec2 g in
        (Vec2 {
            x = x v `mod` x bound,
            y = y v `mod` y bound
        }, g2)

---------------
-- Side effects
---------------

initGame :: IO GameState
initGame = do
    startCharm
    screenSize <- getScreenSize
    randomGen <- getStdGen
    let (initialFoodPosition, randomGen2) = randomVec2Bounded randomGen screenSize in
        return GameState {
            randomGen = randomGen2,
            screenSize = screenSize,
            foodPosition = initialFoodPosition,
            snakeBody = initSnake screenSize
        }


renderFrame :: GameState -> IO ()
renderFrame state = do
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
