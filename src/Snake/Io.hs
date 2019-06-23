module Snake.Io (
    initIo,
    getScreenSize,
    drawFrame,
    getInputs
) where

import Vec2
import Snake.Core
import HsCharm

-- Exported

initIo :: IO ()
initIo = startCharm

getScreenSize :: IO Vec2
getScreenSize = do
    w <- getWidth
    h <- getHeight
    return $ Vec2 w h

drawFrame :: GameState -> IO ()
drawFrame state = do
    clearScreen
    drawFood state
    drawSnake state
getInputs :: IO Inputs
getInputs = fmap (Inputs . convertKey) getKey


-- Internal

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

-- Converts from HsCharm's Key to IO-independent Snake.Core.Key_;
-- Also wraps into Maybe
convertKey :: Key -> Maybe Key_
convertKey KeyLeft  = Just KeyLeft_
convertKey KeyRight = Just KeyRight_
convertKey KeyUp    = Just KeyUp_
convertKey KeyDown  = Just KeyDown_
convertKey _        = Nothing
