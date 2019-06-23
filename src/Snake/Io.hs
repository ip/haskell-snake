{-# LANGUAGE OverloadedStrings #-}

module Snake.Io (
    initIo,
    SnakeIo (..)
) where

import Control.Applicative
import GHC.Word (Word8)
import Vec2
import Snake.Core
import SDL
import SDL.Vect

-- Exported

data SnakeIo = SnakeIo {
    drawFrame :: GameState -> IO (),
    getInputs :: IO Inputs
}

initIo :: IO SnakeIo
initIo = do
    initializeAll
    window <- createWindow "Haskell Snake" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    let ioState = IoState {
        window_ = window,
        renderer_ = renderer,
        windowSize_ = fromIntegral <$> windowInitialSize defaultWindow
    } in return SnakeIo {
        drawFrame = drawFrame_ ioState,
        getInputs = getInputs_
    }


-- Internal

data IoState = IoState {
    window_ :: Window,
    renderer_ :: Renderer,
    windowSize_ :: V2 Int
}

drawFrame_ :: IoState -> GameState -> IO ()
drawFrame_ ioState state = do
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    drawFood ioState state
    drawSnake ioState state

    present renderer
        where renderer = renderer_ ioState

getInputs_ :: IO Inputs
getInputs_ = do
    pollEvents
    return $ Inputs Nothing

drawFood :: IoState -> GameState -> IO ()
drawFood ioState state = drawTile ioState color pos
        where pos = vec2ToV2 $ foodPosition state
              color = V4 255 255 0 255

drawSnake :: IoState -> GameState -> IO ()
drawSnake ioState state = mapM_ (drawLink . vec2ToV2) snakeBody_
        where snakeBody_ = snakeBody state
              drawLink = drawTile ioState color
              color = V4 0 0 255 255

drawTile :: IoState -> V4 Word8 -> V2 Int -> IO ()
drawTile ioState color pos = do
    rendererDrawColor renderer $= color
    fillRect renderer $ Just $ fromIntegral <$> rect
        where renderer = renderer_ ioState
              rect = rectToWindowSpace windowSize2 worldSpaceRect
              worldSpaceRect = Rectangle (P pos) $ V2 1 1
              windowSize2 = windowSize_ ioState

rectToWindowSpace :: V2 Int -> Rectangle Int -> Rectangle Int
rectToWindowSpace winSize rect =
    let Rectangle (P a) b = rect in
        Rectangle (P $ vecToWindowSpace_ a) $ vecToWindowSpace_ b
            where vecToWindowSpace_ = vecToWindowSpace winSize

vecToWindowSpace :: V2 Int -> V2 Int -> V2 Int
vecToWindowSpace winSize vec = (vec * winSize) `divV2` fieldSize_
        where fieldSize_ = vec2ToV2 fieldSize

divV2 :: V2 Int -> V2 Int -> V2 Int
divV2 = liftA2 div

vec2ToV2 :: Vec2 -> V2 Int
vec2ToV2 v = V2 x y
    where Vec2 x y = v

-- Converts from HsCharm's Key to IO-independent Snake.Core.Key_;
-- Also wraps into Maybe
